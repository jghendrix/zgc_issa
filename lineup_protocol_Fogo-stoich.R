## Lineup protocol to test iSSA goodness of fit

library(amt)
library(tidyverse)
library(terra)
library(raster)
library(patchwork)
library(tidyterra)
library(sp)


# Generating simulations takes a while, saved some in the output/ folder after first run if not doing from scratch
## Can skip to Line 242 if not running the simulation again to load the output/sims.Rds files


# This simulation system is built with amt, but our model is glmmTMB
# amt has a make_issf_model() function that we want to use to convert our model
# you have to manually define the coefficients as a vector

## First, let's focus on the winter %N model ----

# We need a single stacked raster with all of the predictors in it
# easy enough for lichen + vascular stoich, but what about prop forest?
# we subsampled that out into a raster to extract from in the targets workflow, just pull that one in?
forest <- raster('output/prop_forest.tif')
f.terra <- rast(forest)
targets::tar_load(lichen)
lich <- rast(lichen)
targets::tar_load(vascular)
vasc <- rast(vascular)

# need the forest % back in 5m cells to match up with the stoich layers, though
f5 <- disagg(f.terra, 20)
# then shift it to match the stoich layers
fshift <- terra::shift(f5, dx = 5, dy = 115)
# and match up the extents
ftrim <- terra::crop(fshift, lich)
# make sure all the CRS are good too
epsg <- 32621
crs <- st_crs(epsg)
crs_sp <- CRS(crs$wkt)

crs(lich) <- crs$wkt
crs(vasc) <- crs$wkt
crs(ftrim) <- crs$wkt

# update the names to match the model
names(lich) <- "N_Cladonia"
varnames(lich) <- "N_Cladonia"
names(vasc) <- "N_vasc"
varnames(vasc) <- "N_vasc"
names(ftrim) <- "prop_forest"
varnames(ftrim) <- "prop_forest"
# Then bring the different predictor surfaces together
layers <- c(ftrim, lich)
layers <- c(layers, vasc)

# Save this stacked raster for future needs so we don't have to mess with that again
writeRaster(layers, 'output/issa_preds.tif')

# We want the separate rasters to use as underlying maps to plot the simulations on top of, after we've generated the simulated tracks
# crop each raster using the Fogo shapefile, and focus on northern half of island
fogo_poly <- read_sf('input/FogoPoly/FogoPoly.shp')
forest_poly <- crop(ftrim, fogo_poly, mask = TRUE) %>% filter(y > 5503000)
lich_poly <- crop(lich, fogo_poly, mask = TRUE) %>% filter(y > 5503000)
vasc_poly <- crop(vasc, fogo_poly, mask = TRUE) %>% filter(y > 5503000)


## WORKFLOW STARTS HERE-----
# read in layers if using the saved raster stack
# layers <- rast(raster('output/issa_surface.tif'))

## Winter comparisons of 19 simulations to one real ----

# first we need to make our amt() version of the model
# reminder of what the parameter estimates look like
summary(winter_model)

# ignoring the individual random effects
w.coefs <- c("log(sl_)" = -0.023459,
             "cos(ta_)" = -0.005796,
             "N_Cladonia_end" = -0.765825,
             "N_vasc_end" = 0.328766,
             "prop_forest_end" = -0.047950,
             "log(sl_):prop_forest_end" = -0.406460,
             "log(sl_):N_Cladonia_end" = 0.426530,
             "log(sl_):N_vasc_end" = -0.041197
)

# Also need the parameters of the step length and turn angle distributions we drew from
# each individual got its own distributions, but if we average those to the population for both seasons
# kappa = 0.1634281    
# shape = 0.6341374   
# scale = 378.2702

# Using those parameters, specify a new amt iSSF model
wint <- amt::make_issf_model(coefs = w.coefs,
                             sl = make_gamma_distr(shape = 0.6341374, scale = 378.2702),
                             ta = make_vonmises_distr(kappa = 0.1634281)
)


# Now need to load the winter data
# subset to only the real observed steps, not the random ones we generated
tar_load(winter_prep)
winter_steps <- winter_prep %>% filter(case_ == TRUE)

# We need to randomly select an individual
indivs <- winter_steps %>% dplyr::filter(!duplicated(id)) %>% slice_sample(n = 1)
# FO2016013 was selected
# We need a general starting point from which to run all the simulations, where is a good starting point from this animal's data?
# where do most of their steps start?
fogo <- winter_steps %>% filter(id == "FO2016013")
hist(fogo$x1_, breaks = 50)
hist(fogo$y1_, breaks = 50)

# let's call it ~703500 and ~5511000

# we should generate new starting points for the 19 simulations, using different starts within ~1.5km of this region
# Randomly select a start point within our parameters to choose the actual track for comparing against the 19
begin <- fogo %>% filter(x1_ > 702000 & x1_ < 706000 & y1_ > 5510000 & y1_ < 5512000)
# 959 options for starting points of a 120-step segment 
begin %<>% slice_sample(n = 1)
# random selection turned out to be step_id_ 14344

w.track <- fogo %>% filter(step_id_ >= 14344 & step_id_ < (14344 + 120)) %>%
  make_track(x1_, y1_, t1_) %>%
  mutate(id = 20) # how we will distinguish this from the 19 simulated

# Run 19 distinct simulations from this range of starting points
n <- 19
ssf_sim <-list()

for(i in 1:n){
  start <- make_start(c(runif(1, 702000, 706000), 
                        runif(1,5510000,5512000)), 
                      dt = hours(2),
                      crs = crs$wkt,
                      ta_ = 0)
  
  rdk <- amt::redistribution_kernel(wint, 
                                    start = start,
                                    map = layers,
                                    as.rast = FALSE,
                                    landscape= "continuous",
                                    tolerance.outside = 0.1)
  
  ssf_sim[[i]] <- simulate_path(rdk, n.steps = 120) 
  ### simulate individual trajectories
  ssf_sim[[i]]$id <- i # add a label to each trajectory
}

# take all those tracks and bind them together
ssf_simdat <- do.call(rbind, ssf_sim) 

ssf_simtr <- ssf_simdat %>% make_track(x_, y_, t_ , id = id)

# Bring the actual track into the same df as these simulations
w.comp <- rbind(ssf_simtr, w.track)
w.comp %>% summarise(minx = min(x_, na.rm = T), maxx = max(x_, na.rm = T),
                     miny = min(y_, na.rm = T), maxy = max(y_, na.rm = T))
# seeing the spatial range to set the boundaries on our plots

# Save these simulated trajectories to save time in future
saveRDS(w.comp, 'output/winter_sims.Rds')

# Summer simulations ----
## same as above, but for the summer model now
tar_load(summer_prep)
summer_steps <- summer_prep %>% filter(case_ == TRUE)

# first, pick an individual to use as our basis
indivs <- winter_steps %>% dplyr::filter(!duplicated(id)) %>% slice_sample(n = 1)
# random draw gave FO2017007
sea <- summer_steps %>% filter(id == "FO2017007")

hist(sea$x1_, breaks = 50)
# let's use 710 000 to 712 000
hist(sea$y1_, breaks = 50)
# y-values between 550 3000 (lower bounds) and 550 5000

begin <- sea %>% filter(x1_ > 710000 & x1_ < 712000 & y1_ > 5505000)
# 1582 options for starting points of a 120-step segment 
rand <- begin %>% slice_sample(n = 1) %>% dplyr::select(c(step_id_))
# random selection chose step_id_ 6433
s.track <- sea %>% filter(step_id_ >= 10649 & step_id_ < (10649 + 120)) %>%
  make_track(x1_, y1_, t1_) %>%
  mutate(id = 20)


## Build amt() version of summer_model
summary(summer_model)

s.coefs <- c("log(sl_)" = -0.103300,
             "cos(ta_)" = -0.204928,
             "N_Cladonia_end" = -0.532593,
             "N_vasc_end" = 0.091747,
             "prop_forest_end" = 0.727411,
             "log(sl_):prop_forest_end" = -0.042207,
             "log(sl_):N_Cladonia_end" = -0.012711,
             "log(sl_):N_vasc_end" = 0.046633
)

summ <- amt::make_issf_model(coefs = s.coefs,
                             sl = make_gamma_distr(shape = 0.6341374, scale = 378.2702),
                             ta = make_vonmises_distr(kappa = 0.1634281)
) 

# Run 19 distinct simulations from starting points
n <- 19
ssf_sim <-list()

for(i in 1:n){
  start <- make_start(c(runif(1, 709000, 711000), 
                        runif(1, 5505000, 5507000)), 
                      dt = hours(2),
                      crs = crs$wkt,
                      ta_ = 0)
  
  rdk <- amt::redistribution_kernel(summ, 
                                    start = start,
                                    map = layers_poly,
                                    as.rast = FALSE,
                                    landscape = "continuous",
                                    tolerance.outside = 0.1)
  
  ssf_sim[[i]] <- simulate_path(rdk, n.steps = 120) 
  ### simulate individual trajectories
  ssf_sim[[i]]$id <- i
}

# take all those tracks and bind them together
ssf_simdat <- do.call(rbind, ssf_sim) 

ssf_simtr <- ssf_simdat %>% make_track(x_, y_, t_ , id = id)

# Now  insert the single observed track into this set of 19
s.comp <- rbind(ssf_simtr, s.track)
s.comp %>% summarise(minx = min(x_), maxx = max(x_),
                     miny = min(y_), maxy = max(y_))
# make sure none of them dip outside our StDM range
south <- s.comp %>% filter(y_ > 5503000)

# save these simulations for future use
saveRDS(s.comp, 'output/summer_sims.Rds')

## Load simulations, if skipping that step ----
# w.comp <- readRDS('output/whole_island/inter_sims.Rds')
# s.comp <- readRDS('output/summer_sims.Rds')


# Winter plots --------
# We have to randomize the order of these facets, but make sure we know which is which
# Randomize the order of the ids in w.comp
w.comp$randid <- as.factor(w.comp$id)
levels(w.comp$randid) <- sample(unique(w.comp$id))

#winter <- w.comp[order(levels(w.comp$rid)),]

# then use this random order and pair it with W1 through W20
# DO NOT LOOK AT w_text if you don't want to know which one is real yet
w_text <- data.frame(
  randid = as.factor(levels(w.comp$randid)),
  season = "W",
  num = seq(from = 1, to = 20)
  ) %>% 
  mutate(label = as.factor(paste0(season, num)))


labs <- w_text %>% dplyr::select(c(randid, label))
w.comp <- left_join(w.comp, labs, by = "randid")

# Reorder w.comp based on this W1-W20 string, which shuffles the original 1-20 IDs, so now the 'actual' data is randomly placed within the 5 x 4 grid
levels(w.comp$label) <- c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10", "W11", "W12", "W13", "W14", "W15", "W16", "W17", 'W18', 'W19', 'W20')

# the real, non-simulated track will be the letter associated with rid == 20 in w_text


# over prop_forest
w_on_forest <- ggplot() + 
  geom_spatraster(data = forest_poly)  +
  geom_path(data = w.comp, aes(x_, y_), 
            col = "black") +
  facet_wrap(~ label, nrow = 4) + 
  scale_fill_terrain_c() +
  xlim(c(700000, 710000)) + 
  ylim(c(5505000, 5515000)) +
  xlab("") +
  ylab("") +
  labs(fill = "Proportion forested") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "grey90"),
        panel.grid = element_blank())

ggsave(filename = "graphics/validation/W_forest_120.png",
       w_on_forest,
       height = 2000,
       width = 2400,
       units = "px")

# winter tracks over Cladonia
# the segments are the same, just changing the underlying plot those trajectories are shown on top of

sim_on_Clad <- ggplot() + 
  geom_spatraster(data = lich_poly)  +
  geom_path(data = w.comp, aes(x_, y_), 
            col = "black") +
  facet_wrap(~ label, nrow = 4) + 
  scale_fill_terrain_c() +
  xlim(c(700000, 710000)) + 
  ylim(c(5505000, 5515000)) +
  xlab("") +
  ylab("") +
  labs(fill = "%N in Cladonia") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "grey90"),
        panel.grid = element_blank())
ggsave(filename = "graphics/validation/W_sim120_Cladonia.png",
       sim_on_Clad,
       height = 2000,
       width = 2400,
       units = "px")




# Summer plots ------
# randomize order like for winter, but now 20 new random trajectories
s.comp$randid <- as.factor(s.comp$id)
levels(s.comp$randid) <- sample(unique(s.comp$id))

s_text <- data.frame(
  randid = seq(1:20),
  season = "S") %>%
  mutate(label = as.factor(paste0(season, randid)))

labs <- s_text %>% dplyr::select(c(randid, label))
s.comp <- left_join(s.comp, labs, by = "randid")

levels(s.comp$label) <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10", "S11", "S12", "S13", "S14", "S15", "S16", "S17", 'S18', 'S19', 'S20')


## Over vascular %N
s_sim_on_vasc <- ggplot() + 
  geom_spatraster(data = vasc_poly)  +
  geom_path(data = s.comp, aes(x_, y_), 
            col = "black") +
  facet_wrap(~ label, nrow = 4) + 
  scale_fill_terrain_c() +
  xlim(c(705000, 715000)) + 
  ylim(c(5503000, 5510000)) +
  xlab("") +
  ylab("") +
  labs(fill = "%N in vascular plants") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "grey90"),
        panel.grid = element_blank())

ggsave(filename = "graphics/validation/northern_summer_120_vasc.png",
       s_sim_on_vasc,
       height = 1400,
       width = 2400,
       units = "px")

# try it on the forest cover map as well

s_sim_on_forest <- ggplot() + 
  geom_spatraster(data = forest_poly)  +
  geom_path(data = s.comp, aes(x_, y_), 
            col = "black") +
  facet_wrap(~ label, nrow = 4) + 
  scale_fill_terrain_c() +
  xlim(c(705000, 715000)) + 
  ylim(c(5503000, 5510000)) +
  xlab("") +
  ylab("") +
  labs(fill = "Proportion forested") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "grey90"),
        panel.grid = element_blank())

ggsave(filename = "graphics/validation/S_120_forest.png",
       s_sim_on_forest,
       height = 2000,
       width = 2400,
       units = "px")
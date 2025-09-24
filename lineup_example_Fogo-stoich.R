library(amt)
library(tidyverse)
library(terra)
library(raster)
library(patchwork)
library(tidyterra)

# This simulation system is built with amt... my model is glmmTMB

# amt has a make_issf_model() function
# you have to manually define the coefficients as a vector
# let's focus on the winter %N model for now?

# We need a single stacked raster with all of the predictors in it -----
# easy enough for lichen + vascular stoich, but what about prop forest?
# we subsampled that out into a raster to extract from in the targets workflow, can we just pull that one in?

forest <- raster('output/prop_forest.tif')
f.terra <- rast(forest)

lich <- rast(lichen)
vasc <- rast(vascular)

# need the forest % back in 5m cells to match up with the stoich, though
f5 <- disagg(f.terra, 20)
# then shift it to match the stoich layers
fshift <- terra::shift(f5, dx = 5, dy = 115)
# and match up the extents
ftrim <- terra::crop(fshift, lich)
# make sure all the CRS are good too
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
layers <- c(ftrim, lich)
layers <- c(layers, vasc)

writeRaster(layers, 'output/issa_preds.tif')

## WORKFLOW STARTS HERE-----
#layers <- rast(raster('output/issa_surface.tif'))

## Winter comparisons of 19 simulations to one real ----
# first need to make our amt() version of the model
summary(winter_model)

# ignoring the individual random effects, I suppose
w.coefs <- c("log(sl_)" = -0.023459,
           "cos(ta_)" = -0.005796,
           "N_Cladonia_end" = -0.765825,
           "N_vasc_end" = 0.328766,
           "prop_forest_end" = -0.047950,
           "log(sl_):prop_forest_end" = -0.406460,
           "log(sl_):N_Cladonia_end" = 0.426530,
           "log(sl_):N_vasc_end" = -0.041197
)
# what were the parameters of the distribution we drew from?
# each individual got its own distributions, but if we average those to the population for both seasons
# kappa 0.1634281    
# shape 0.6341374   
# scale 378.2702

wint <- amt::make_issf_model(coefs = w.coefs,
                       sl = make_gamma_distr(shape = 0.6341374, scale = 378.2702),
                       ta = make_vonmises_distr(kappa = 0.1634281)
)


# maybe start with which step begins around the origin we picked above...
tar_load(winter_prep)
winter_steps <- winter_prep %>% filter(case_ == TRUE)

# where do most of these steps start?
# pick an individual to start with... 2016013 has the most steps
fogo <- winter_steps %>% filter(id == "FO2016013")
hist(subset(fogo,x1_ > 702000 & x1_ < 706000)$x1_, breaks = 30)
# let's call it 703500
hist(subset(fogo, x1_ > 702000 & x1_ < 706000)$y1_, breaks = 30)
# and 5511000

# we should generate new starting points for the 19 references, using different starts

# Run 19 distinct simulations with different starting points ------
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
  
  # Originally did 50 steps, but we should extend that probably eh?
  ssf_sim[[i]] <- simulate_path(rdk, n.steps = 120) 
  ### simulate individual trajectories
  ssf_sim[[i]]$id <- i
}

# take all those tracks and bind them together
ssf_simdat <- do.call(rbind, ssf_sim) 

ssf_simtr <- ssf_simdat %>% make_track(x_, y_, t_ , id = id)

# Plot just four of these, at first
#ssf_simtr_sample <- ssf_simtr |> filter(id %in% c(1:4)) 

#ggplot() + 
 # geom_spatraster(data = lich)  +
  #geom_path(data = ssf_simtr, aes(x_, y_), 
   #         col= "black") +
#  facet_wrap(~ id, nrow = 4) + 
 # scale_fill_terrain_c() +
  #theme_bw()

# Now we have to insert the single observed track into this set of 19
# pick a random individual, and a start point within our parameters
fogo <- winter_steps %>% filter(id == "FO2016013")
begin <- fogo %>% filter(x1_ > 702000 & x1_ < 706000 & y1_ > 5510000 & y1_ < 5512000)
# 959 options for starting points of a 50-step segment 
begin %<>% slice_sample(n = 1)
# randomly choose one, turned out to be step_id_ 14344

w.track <- fogo %>% filter(step_id_ >= 14344 & step_id_ < (14344 + 120)) %>%
  make_track(x1_, y1_, t1_) %>%
  mutate(id = 20)

ggplot() + 
  geom_spatraster(data = lich)  +
  geom_path(data = w.track, aes(x_, y_), 
            col= "red") +
 # facet_wrap(~ id, nrow = 4) + 
  scale_fill_terrain_c() +
  xlim(c(700000, 708000)) + 
  ylim(c(5505000, 5515000))
  theme_bw()


# now bring them all together
w.comp <- rbind(ssf_simtr, w.track)
w.comp %>% summarise(minx = min(x_, na.rm = T), maxx = max(x_, na.rm = T),
                   miny = min(y_, na.rm = T), maxy = max(y_, na.rm = T))
# use these to trim down our plots slightly so you can actually see the trajectories

# Also want to include a perimeter of Fogo somehow
# could use the shape file to mask the rasters to NA outside of the poly
fogo_poly <- read_sf('input/FogoPoly/FogoPoly.shp')
forest_poly <- crop(ftrim, fogo_poly, mask = TRUE)
lich_poly <- crop(lich, fogo_poly, mask = TRUE)
vasc_poly <- crop(vasc, fogo_poly, mask = TRUE)

# Winter plots --------
## on forest -----
# We want to randomize the order of these facets, but make sure we know which is which
# Randomize the order of the ids in w.comp
w.comp$id <- as.factor(w.comp$id)
levels(w.comp$id) <- sample(unique(w.comp$id))

# then use this random order associated with the letter A through T
w_text <- data.frame(
  label = c(letters[seq(from = 1, to = 20)]),
  id = as.factor(levels(w.comp$id))
)

# the real, non-simulated track will be the letter associated with id == 20 in w_text

sim_on_forest <- ggplot() + 
  geom_spatraster(data = forest_poly)  +
  geom_path(data = w.comp, aes(x_, y_), 
            col = "black") +
  facet_wrap(~ id, nrow = 4) + 
  scale_fill_terrain_c() +
  xlim(c(700000, 710000)) + 
  ylim(c(5505000, 5515000)) +
  xlab("") +
  ylab("") +
  labs(fill = "Proportion forested") +
  geom_text(data = w_text, 
            mapping = aes(x = -Inf, y = -Inf, label = label),
            hjust = -1, vjust = -5.5, size = 5
  ) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.background = element_rect(fill = "grey90"),
        panel.grid = element_blank())

ggsave(filename = "graphics/validation/random_winter_forest_120.png",
       sim_on_forest,
       height = 2000,
       width = 2400,
       units = "px")






# winter Cladonia plot?

sim_on_Clad <- ggplot() + 
  geom_spatraster(data = lich_poly)  +
  geom_path(data = w.comp, aes(x_, y_), 
            col = "black") +
  facet_wrap(~ id, nrow = 4) + 
  scale_fill_terrain_c() +
  xlim(c(700000, 710000)) + 
  ylim(c(5505000, 5515000)) +
  xlab("") +
  ylab("") +
  labs(fill = "%N in Cladonia") +
  theme_bw() +
  geom_text(data = w_text, 
            mapping = aes(x = -Inf, y = -Inf, label = label),
            hjust = -1, vjust = -5.5, size = 5
  ) +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.background = element_rect(fill = "grey90"),
        panel.grid = element_blank())
ggsave(filename = "graphics/validation/random_winter_sim120_Cladonia.png",
       sim_on_Clad,
       height = 2000,
       width = 2400,
       units = "px")






## Summer comparisons of 19 simulations to one real ----
tar_load(summer_prep)
summer_steps <- summer_prep %>% filter(case_ == TRUE)

# first, pick an individual to use as our basis
ids <- summer_prep %>% group_by(id) %>% summarise(n = n())
# lots of individuals could work... 2017007 has the most steps
# we could theoretically randomly sample one of them too, but for now let's just use her... she died spring 2024 of brainworm tho, so make sure to use earlier data
sea <- summer_steps %>% filter(id == "FO2017007")

hist(subset(sea, x1_ > 710000 & x1_ < 712000)$x1_, breaks = 50)
# let's call it 710 000 to 712 000
hist(subset(sea, x1_ > 710000 & x1_ < 712000)$y1_, breaks = 50)
# among these steps, the y-values are between 550 3000 and 550 5000

begin <- sea %>% filter(x1_ > 710000 & x1_ < 712000 & y1_ > 5503000 & y1_ < 5505000)
# 1887 options for starting points of a 50-step segment 
rand <- begin %>% slice_sample(n = 1)
# randomly choose one, turned out to be step_id_ 998
s.track <- sea %>% filter(step_id_ >= 10967 & step_id_ < (10967+120)) %>%
  make_track(x1_, y1_, t1_) %>%
  mutate(id = 20)

# step id 998 only got us 108 steps... 
# regenerated, 10967 got us the full 120
s.track %>% summarise(xl = min(x_), xh = max(x_), yl = min(y_), yh = max(y_))


ggplot() + 
  geom_spatraster(data = vasc_poly)  +
  geom_path(data = s.track, aes(x_, y_), 
            col= "red") +
  # facet_wrap(~ id, nrow = 4) + 
  scale_fill_terrain_c() +
  xlim(c(708000, 713000)) + 
  # zoomed these out by an additional 1km on all sides to get better perspective on where we are
  ylim(c(5502000, 5506000)) +
  theme_bw()

# This ended up being in the deep southeast coast, like just west of Cape Cove... I don't think this is the best place necessarily to test our stoich model LOL
# but let's go for it, i suppose

# Building amt() version of summer_model
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

# Run 19 distinct simulations with different starting points ------
n <- 19
ssf_sim <-list()

for(i in 1:n){
  start <- make_start(c(runif(1, 709000, 711000), 
                        runif(1,5503000,5505000)), 
                      dt = hours(2),
                      crs = crs$wkt,
                      ta_ = 0)
  
  rdk <- amt::redistribution_kernel(summ, 
                                    start = start,
                                    map = layers,
                                    as.rast = FALSE,
                                    landscape= "continuous",
                                    tolerance.outside = 0.1)
  
  ssf_sim[[i]] <- simulate_path(rdk, n.steps = 120) 
  ### simulate individual trajectories
  ssf_sim[[i]]$id <- i
}

# take all those tracks and bind them together
ssf_simdat <- do.call(rbind, ssf_sim) 

ssf_simtr <- ssf_simdat %>% make_track(x_, y_, t_ , id = id)

# Plot just four of these, at first
ssf_simtr_sample <- ssf_simtr |> filter(id %in% c(1:4)) 

ggplot() + 
  geom_spatraster(data = vasc)  +
  geom_path(data = ssf_simtr, aes(x_, y_), 
            col= "black") +
  facet_wrap(~ id, nrow = 4) + 
  scale_fill_terrain_c() +
  theme_bw()

# Now we have to insert the single observed track into this set of 19
# pick a random individual, and a start point within our parameters
# now bring them all together
s.comp <- rbind(ssf_simtr, s.track)
s.comp %>% summarise(minx = min(x_), maxx = max(x_),
                   miny = min(y_), maxy = max(y_))
# use these to trim down our plots slightly so you can actually see the trajectories

# Summer plots ---

# Randomize the order of the ids
s.comp$id <- as.factor(s.comp$id)
levels(s.comp$id) <- sample(unique(s.comp$id))
# then use this random order associated with the letter A through T
s_text <- data.frame(
  label = c(letters[seq(from = 1, to = 20)]),
  id = as.factor(levels(s.comp$id))
)

s_sim_on_vasc <- ggplot() + 
  geom_spatraster(data = vasc_poly)  +
  geom_path(data = s.comp, aes(x_, y_), 
            col = "black") +
  facet_wrap(~ id, nrow = 4) + 
  scale_fill_terrain_c() +
  xlim(c(705000, 715000)) + 
  ylim(c(5500000, 5510000)) +
  xlab("") +
  ylab("") +
  labs(fill = "%N in vascular plants") +
  geom_text(data = s_text, 
            mapping = aes(x = -Inf, y = -Inf, label = label),
            hjust = -1, vjust = -5.5, size = 5
  ) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.background = element_rect(fill = "grey90"),
        panel.grid = element_blank())

ggsave(filename = "graphics/validation/random_summer_sim120_vasc.png",
       s_sim_on_vasc,
       height = 2000,
       width = 2400,
       units = "px")

# try it on the forest cover map?

s_sim_on_forest <- ggplot() + 
  geom_spatraster(data = forest_poly)  +
  geom_path(data = s.comp, aes(x_, y_), 
            col = "black") +
  facet_wrap(~ id, nrow = 4) + 
  scale_fill_terrain_c() +
  xlim(c(705000, 715000)) + 
  ylim(c(5499000, 5508000)) +
  xlab("") +
  ylab("") +
  labs(fill = "Proportion forested") +
  geom_text(data = s_text, 
            mapping = aes(x = -Inf, y = -Inf, label = label),
            hjust = -1, vjust = -5.5, size = 5
  ) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.background = element_rect(fill = "grey90"),
        panel.grid = element_blank())

ggsave(filename = "graphics/validation/random_summer_sim120_forest.png",
       height = 2000,
       width = 2400,
       units = "px")

saveRDS(w.comp, 'output/winter_sims.Rds')
saveRDS(s.comp, 'output/summer_sims.Rds')

# How to randomize order ----
## We want to randomize the order of these plots tho, it shouldn't always be the last one

# Try this new order out?

# Ok, this randomization worked, BUT it's still identified as #20... if we remove the facet labels though, we have no way of knowing which is which
# have to have duplicate copies with and without?
# Or we can we add manual labels A through T, perhaps?

dat_text <- data.frame(
  label = c(letters[seq( from = 1, to = 20 )]),
  id = as.factor(levels(w.comp$id))
)

rand_on_Clad <- ggplot() + 
  geom_spatraster(data = lich_poly)  +
  geom_path(data = w.comp, aes(x_, y_), 
            col = "black") +
  facet_wrap(~ id, nrow = 4) + 
  scale_fill_terrain_c() +
  xlim(c(700000, 710000)) + 
  ylim(c(5505000, 5515000)) +
  xlab("") +
  ylab("") +
  labs(fill = "%N in Cladonia") +
  geom_text(data = dat_text, 
            mapping = aes(x = -Inf, y = -Inf, label = label),
            hjust = -0.5, vjust = -5
            ) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.background = element_rect(fill = "grey90"),
        panel.grid = element_blank())

rand_on_Clad

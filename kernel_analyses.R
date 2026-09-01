# Fogo Island caribou: seasonal range kernels and forage nitrogen content-----

library(raster)
library(SDMTools)
library(adehabitatHR)
library(ctmm)
library(tidyverse)

# Fogo Island shapefile
fogo_poly <- read_sf('input/FogoPoly/FogoPoly.shp')

# Individual winter and summer aKDEs
all_winter <- readRDS('output/other/kernels/winter_akdes.Rds')
winter.full <- mean(all_winter) # take average of all the individuals

all_summer <- readRDS('output/other/kernels/summer_akdes.Rds')
summer.full <- mean(all_summer)

# What do range sizes look like between seasons
# Fogo is 238 km2, but some of these kernels include ocean

summary(winter.full, level.UD = 0.95) # 270 km2... bigger than Fogo
summary(winter.full, level.UD = 0.50) # 56.2 km2
summary(winter.ud, level.UD = 0.10) # 6.0 km2

summary(summer.full, level.UD = 0.95) # 345 km2... much bigger than Fogo
summary(summer.full, level.UD = 0.50) # 80.8 km2
summary(summer.full, level.UD = 0.10) # 11.1 km2

# the 95% kernel is bigger than the entire island in both cases
# regardless of the % kernel, summer range is always bigger 
# Compromise: use the 50% going forward

# convert UD to polygons of 50% kernel for visualization purposes
winter_50 <- SpatialPolygonsDataFrame.UD(winter.full, level.UD = 0.5)
w_sf <- st_as_sf(winter_50)

summer_50 <- SpatialPolygonsDataFrame.UD(summer.full, level.UD = 0.5)
s_sf <- st_as_sf(summer_50)

# Are summer vs. winter ranges spatially distinct? ----

# convert to rasters
sk <- rast(raster(summer.full))
wk <- rast(raster(winter.full))

# get winter and summer kernels into same scale with Fogo polygon
scrop <- crop(sk, fogo_poly, mask = TRUE) %>%
  rename(summer = layer) %>%
  filter(y > 5503000)
wcrop <- crop(wk, fogo_poly, mask = TRUE) %>%
  rename(winter = layer) %>%
  filter(y > 5503000)
wres <- resample(wcrop, scrop)

seasons <- c(scrop, wres)

# Correlation between layers
cor(
  values(seasons)[, 1],
  values(seasons)[, 2],
  use = "na.or.complete"
)
# Correlation among the raster version is -21 %, i.e. winter and summer are spaced out from one another

# From the 50% core areas?
wint <- w_sf %>% slice(2) # just want the mean estimate, not the lower and upper 95% CI
summ <- s_sf %>% slice(2)

comp <- st_intersection(wint, summ) %>%
  dplyr::select(-c(name.1))

seasonal_50 <- rbind(wint, summ, comp) %>%
  mutate(Id = c("winter50", "summer50", "overlap50"))

# Calculate area of each shape, then convert from m2 to km2 
s50_areas <- st_area(seasonal_50)/1000000

# winter = 56.20
# summer = 80.38
# overlap = 12.18758
# I.e., 21.6% overlap wrt winter, 15.1% overlap wrt summer
## so 18.3% overlap on average?

# Comparing to StDMs -----
epsg <- 32621
crs <- st_crs(epsg)
crs_sp <- CRS(crs$wkt)

# Use the Cladonia N map as an underlying surface on which to plot these kernels to check what areas they're covering

lichen <- raster('input/Cladonia_N.tif')
lich <- rast(lichen)

crs(lich) <- crs$wkt
names(lich) <- "N_Cladonia"
varnames(lich) <- "N_Cladonia"
lich_poly <- crop(lich, fogo_poly, mask = TRUE) %>%
  filter(y > 5503000) # only looking at northern half of Fogo Island

# Winter data, core 50% plot
ggplot() +
  geom_spatraster(data = lich_poly) +
  geom_spatvector(data = w_sf, alpha = 0.3) +
  labs(fill = "%N Cladonia") +
  ggtitle("Winter 50% aKDE, with 95% CI")
ggsave(filename = "graphics/maps/winter_50_lichen.png",
       height = 2000,
       width = 2400,
       units = "px")

# Summer data, core 50% plot
ggplot() +
  geom_spatraster(data = lich_poly) +
  geom_spatvector(data = s_sf, alpha = 0.3) +
  labs(fill = "%N Cladonia") +
  ggtitle("Summer 50% aKDE, with 95% CI")
ggsave(filename = "graphics/maps/summer_50_lichen.png",
       height = 2000,
       width = 2400,
       units = "px")


## Calculating seasonal range areas ----

## We want area of the winter range, so we can use it to calculate the kg N per m2. The 50% KDE estimate is 56 km2, but this includes large sections of ocean that we want to exclude from our estimate.
w_sf

# The winter KDE 50% shape contains three objects, the mean estimate and then 95% CI either side of that
# can we calculate the inland area of each of these polygons?

# make sure CRS is same for Fogo shape and KDE shape so we can get the overlap
fogo_trans <- st_transform(fogo_poly, crs = crs$wkt)

# Split the KDE into the lower CI, mean, and upper CI shapes
low <- w_sf %>% slice(1)
est <- w_sf %>% slice(2)
high <- w_sf %>% slice(3)

land_lower <- st_intersection(fogo_trans, low)
land_area <- st_intersection(fogo_trans, est)
land_upper <- st_intersection(fogo_trans, high)

# Check that these did what we wanted them to:
ggplot() +
  geom_spatvector(data = land_lower, alpha = 0.3, fill = "darkgreen") +
  geom_spatvector(data = land_area, alpha = 0.3, fill = "darkgreen") +
  geom_spatvector(data = land_upper, alpha = 0.3, fill = "darkgreen")

# Recombine these three land-only HR estimates into a single df
terr_HR <- rbind(land_lower, land_area, land_upper) %>%
  mutate(Id = c("lower", "est", "upper"))

# Calculate area of each shape, then convert from m2 to km2 
winter_areas <- st_area(terr_HR)/1000000

## Winter 50% home range land area = 38.39 km2 (LCL = 15.67, UCL = 76.04)

# Also trim the summer kernel to land surface
low_s <- s_sf %>% slice(1)
est_s <- s_sf %>% slice(2)
high_s <- s_sf %>% slice(3)

land_lower_s <- st_intersection(fogo_trans, low_s)
land_area_s <- st_intersection(fogo_trans, est_s)
land_upper_s <- st_intersection(fogo_trans, high_s)

sum_HR <- rbind(land_lower_s, land_area_s, land_upper_s) %>%
  mutate(Id = c("lower", "est", "upper"))

# Calculate area of each shape, then convert from m2 to km2 
summer_areas <- st_area(sum_HR)/1000000
## summer 50% home range land area = 64.11 km2 (LCL = 44.52, UCL = 87.77)

# overlap of summer and winter land areas = 9.020492 km2
# 14.1% of summer
# 23.5% of winter


# Nitrogen relative to seasonal ranges ----
## We want to compare the % N of lichen and plants from our sampling sites, relative to where those plots fall on the winter and summer ranges.

# First, start by just comparing inside vs. outside the 50% kernel polygon we just created

# Sample locations:
sites <- read_csv('input/sp_prep.csv') %>% 
  dplyr::select(c(site, lc, species, x, y, Lat, Long, percent_N)) %>%
  filter(species != "Black_spruce" & species != "graminoid_spp.")

# convert to sf object
locs <- st_as_sf(sites, coords = c("x", "y"), crs = crs$wkt)

# extract which sites are within vs. without the 50% range
all_inside <- c(st_intersects(locs, land_area, sparse = FALSE))
all_sp <- cbind(locs, all_inside) %>%
  rename(winter_range = all_inside) %>%
  mutate(`Winter range` = ifelse(winter_range == TRUE, "inside", "outside"))

all_sp$species <- gsub("_", " ", all_sp$species)
all_sp$species <- gsub("Kalmia", "Sheep laurel", all_sp$species)
all_sp$species <- as.factor(all_sp$species)

# Is percent N higher inside the winter range vs. outside, depending on species?

# boxplot of all species %N
ggplot(all_sp, aes(x = species, y = percent_N, colour = `Winter range`)) +
  geom_boxplot() +
  scale_colour_viridis(discrete = TRUE, option = "plasma", end = 0.8) +
  theme_bw() +
  xlab("") +
  ylab("Percent N (dry mass)")
ggsave(filename = 'graphics/maps/N-inside-outside_boxplot.png',
       height = 4,
       width = 8)


# geom_point and showing mean, not median
sp_avg <- all_sp %>% group_by(`Winter range`, species) %>%
  summarise(N = mean(percent_N, na.rm = T))

ggplot(all_sp, aes(x = species, y = percent_N, colour = `Winter range`)) +
  geom_jitter(alpha = 0.5,
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.7)) +
  scale_colour_manual(values = c("steelblue", "sienna1")) +
  geom_point(data = sp_avg, aes(x = species, y = N, group = `Winter range`),
             size = 8, colour = "black", shape = 95,
             position = position_dodge(0.7)) +
  theme_bw() +
  xlab("") +
  ylab("Percent N (dry mass)")

ggsave(filename = 'graphics/maps/N-inside-outside_jitter.png',
       height = 4,
       width = 8)



## Statistical comparison of inside vs. outside %N ----
# Most species appear to be very slightly higher in N at sites inside the 50% kernel vs. outside, but not really any significant differences based on this dichotomous variable
# Alder appears to be behaving opposite the rest though, which is perhaps interesting given it is a nitrogen fixer

spp <- unique(all_sp$species)

for(i in spp){
  
  dat <- all_sp %>% filter(species == i)
  print(i)
  print(summary(i <- lm(percent_N ~ winter_range, data = dat)))
}

## What about average vascular like that used in the models?
df <- left_join(sites, wint, by = "site") %>%
  mutate(vasc = ifelse(species == "Cladonia", "non-vasc", "vasc"))

ggplot(df, aes(x = vasc, y = percent_N, colour = winter_range)) +
  geom_boxplot()

summary(lm(percent_N ~ winter_range, data = subset(df, vasc == "vasc")))






# 25 August - trying to figure out UDOI overlap of summer vs. winter?? ------
library(ctmm)
library(adehabitatHR)
library(terra)
library(tidyverse)
library(sf)
library(magrittr)
library(tidyterra)

bb <- c(
  xmin = 693000,
  ymin = 5503000,
  xmax = 716900,
  ymax = 5515100)

epsg <- 32621
crs <- st_crs(epsg)
crs_sp <- CRS(crs$wkt)

summer_pop <- readRDS('output/other/kernels/summer_pop.Rds')
winter_pop <- readRDS('output/other/kernels/winter_pop.Rds')

fogo_poly <- read_sf('input/FogoPoly/FogoPoly.shp')
fogo_trans <- st_transform(fogo_poly, crs = crs$wkt)
fogo <- st_crop(fogo_trans, bb)

win_rast <- rast(raster(winter_pop))
crs(win_rast) <- "epsg:32621"
win_crop <- crop(win_rast, fogo, mask = TRUE)  

sum_rast <- rast(raster(summer_pop))
crs(sum_rast) <- "epsg:32621"
sum_crop <- crop(sum_rast, fogo, mask = TRUE)  

summary(win_crop)

# These are currently in KDE style, i.e. lowest values = highest intensity of use area
# for kerneloverlaphr, it needs to be the other way around
win_inv <- win_crop %>% mutate(layer = abs(1-layer))
summary(win_inv)
sum_inv <- sum_crop %>% mutate(layer = abs(1-layer))


# replace NAs with 0s in the middle of the ocean
wz <- win_inv %>% mutate(layer = ifelse(is.na(layer), 0, layer))
sz <- sum_inv %>% mutate(layer = ifelse(is.na(layer), 0, layer))

# template raster against which to harmonize these two, before converting to spdf
temp <- rast(xmin=693000, xmax=716900, ymin=5503000, ymax=5515100, res = 25, crs= crs$wkt)
w2 <- project(wz, temp)
s2 <- project(sz, temp)

wr <- raster(w2)
sr <- raster(s2)
wspdf <- as(wr, "SpatialPixelsDataFrame")
summary(wspdf)

sspdf <- as(sr, "SpatialPixelsDataFrame")
summary(sspdf)

# s2 and w2 have the same dimensions, but sspdf is smaller than wspdf somehow?
# the x-boundaries are 3 cells narrower but it reports having the same ncol and nrow, somehow...

# I'm sure there's a better way than this but it should work?
windf <- as.data.frame(wspdf) %>%
  mutate(loc = paste0(x, y))
sumdf <- as.data.frame(sspdf) %>%
  mutate(loc = paste0(x, y))

add <- windf %>% filter(!loc %in% sumdf$loc) %>%
  mutate(layer = 0)

sum2 <- rbind(sumdf, add) %>% dplyr::select(-c(loc))
win2 <- windf %>% dplyr::select(-c(loc))
sumc <- sum2
winc <- win2

coordinates(winc) = c("x", "y") # promote to SpatialPointsDataFrame
gridded(winc) <- TRUE 
coordinates(sumc) = c("x", "y") # promote to SpatialPointsDataFrame
gridded(sumc) <- TRUE 

sumc
winc

# go ahead with winc and sumc now

#data(meuse.grid)
#m = SpatialPixelsDataFrame(points = meuse.grid[c("x", "y")], data = meuse.grid)

# Define parameters and slots for estUD - had to do this for some reason, get a volume error message otherwise
fullgrid(winc) <- FALSE
fullgrid(sumc) <- FALSE

hli <- list(h = 1, meth="specified")

win.estUD <- new("estUD", winc)
slot(win.estUD, "h") <- hli
slot(win.estUD, "vol") <- FALSE

sum.estUD <- new("estUD", sumc)
slot(sum.estUD, "h") <- hli
slot(sum.estUD, "vol") <- FALSE

UD.combined <- list(winter = win.estUD, summer = sum.estUD)
class(UD.combined) <- "estUDm"

saveRDS(UD.combined, 'output/summer-winter-UD.Rds')


# Restart R session
UD.combined <- readRDS('output/summer-winter-UD.Rds')
library(adehabitatHR)

summary(UD.combined)
plot(UD.combined[[1]])
plot(UD.combined[[2]])

kerneloverlaphr(UD.combined, method = "VI")

ver <- getverticeshr(win.estUD, percent = 95)

# WHAT NOWWW

# 27 August - Can we manually calculate the volume intersection from the dataframes? ----

# sum2 and win2 are dataframes with x and y values
sum2 %<>% rename(sumk = layer)
win2 %<>% rename(wink = layer)

sea <- left_join(sum2, win2, by = c("x", "y")) %>%
  mutate(int = sumk*wink)

# average intersection across all cells is 7.4%
mean(sea$int)

# if we sum the total probability for each season's UD and divide the volume of intersection by that?
mean_tot <- (sum(sea$sumk) + sum(sea$wink))/2

sum(sea$int)/mean_tot

# 35.35% of each season's volume is tied up in the other... but is that statistically unexpected?? who the fuck knows




# percent = 100, conditional = TRUE

# method = "HR", output = NaN
# method = "PHR", output = 0
# method = "VI", output = 2e8  -OR- 0 if conditional = TRUE
# method = "BA", output = 0
# method = "UDOI", output = 0
# method = "HD", output = 0
data(puechabonsp)
  
kud <- kernelUD(puechabonsp$relocs[,1],
                grid=200, same4all=TRUE)
kerneloverlaphr(kud, meth = "BA", conditional = FALSE)
plot(kud[[1]])


# So this code using example data actually gives me appropriate ratio values less than 1... why does my code generate values in the two hundred millions?

# In the example code, the highest values are in the highest intensity of use areas??? wtf???





# other code pre-realizing my mistakes




library(sp)

win_re <- new("estUD", wspdf)
class(win_re) <- "estUD"

sum_re <- new("estUD", sspdf)
class(sum_re) <- "estUD"
UD.re <- list(winter = win_re, summer = sum_re)
class(UD.re) <- "estUDm"
saveRDS(UD.re, 'output/summer-winter-kernels_pre-hli-adj.Rds')

# Trying to just figure out plotting, good lord
library(adehabitatHR)
library(raster)

UD.combined <- readRDS('output/summer-winter-kernels.Rds')
UD.pre <- readRDS('output/summer-winter-kernels_pre-hli-adj.Rds')

plot(UD.combined[[1]])
plot(UD.pre[[1]])

# These appear to be functionally identical, and they appear accurate for where the highest intensity of use should be? There's just a ton of 0s in the ocean, is all

kerneloverlaphr(UD.pre, method = "BA")
# this function only works after we do the h <- "hli" adjustment, I have no idea why
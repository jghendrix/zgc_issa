# Fogo Island caribou: seasonal range kernels and forage nitrogen content-----

library(raster)
library(SDMTools)
library(adehabitatHR)
library(ctmm)

# Fogo Island shapefile
fogo_poly <- read_sf('input/FogoPoly/FogoPoly.shp')

# Individual winter and summer aKDEs
all_winter <- readRDS('output/kernels/winter_akdes.Rds')
winter.full <- mean(all_winter) # take average of all the individuals

all_summer <- readRDS('output/kernels/summer_akdes.Rds')
summer.full <- mean(all_summer)

# What do range sizes look like relative to the island?
# Fogo is 238 km2, but some of these kernels include ocean

summary(winter.full, level.UD = 0.95) # 270 km2... bigger than Fogo
summary(winter.full, level.UD = 0.50) # 56.2 km2
summary(winter.ud, level.UD = 0.10) # 6.0 km2

summary(summer.full, level.UD = 0.95) # 345 km2... much bigger than Fogo
summary(summer.full, level.UD = 0.50) # 80.8 km2
summary(summer.full, level.UD = 0.10) # 11.1 km2

# the 95% kernel is bigger than the entire island in both cases
# regardless of the % kernel, summer range is always bigger though
# Let's just use the 50% going forward

# convert UD to polygons of 50% kernel for visualization purposes
winter_50 <- SpatialPolygonsDataFrame.UD(winter.full, level.UD = 0.5)
w_sf <- st_as_sf(winter_50)

summer_50 <- SpatialPolygonsDataFrame.UD(summer.full, level.UD = 0.5)
s_sf <- st_as_sf(summer_50)

# Summer vs. winter ranges - spatially distinct? ----
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


### WIP: UDOI overlap? ----
# To use the hr_overlap function, we need amt KDE files
# it won't work on the ctmm akde type
# is there a way to convert the raster version back into an amt home range/UD?
## not that I have yet to figure out






# Plotting -----
# Use the Cladonia N map as an underlying surface on which to plot these kernels to check what areas they're covering

tar_load(lichen)
lich <- rast(lichen)
epsg <- 32621
crs <- st_crs(epsg)
crs_sp <- CRS(crs$wkt)
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
## We want area of the winter range to calculate the kg N per m2. The 50% KDE estimate is 56 km2, but this includes large sections of ocean that we want to exclude from our estimate.
w_sf

# The winter KDE 50% shape contains three objects, the mean estimate and then 95% CI either side of that
# can we calculate the inland area of each of these polygons?

ctmm::plot(fogo_poly)
# our Fogo island shapefle excludes freshwater as well. Which during winter, they do walk on sometimes... but we also exclude freshwater from our StDMs so that's ok

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
saveRDS(terr_HR, 'output/kernels/winter_50_land_area.Rds')

# Calculate area of each shape, then convert from m2 to km2 
winter_areas <- st_area(terr_HR)/1000000

## Winter 50% home range land area = 38.39 km2 (LCL = 15.67, UCL = 76.04)



# Nitrogen relative to seasonal ranges ----
## We want to compare the % N of lichen and plants from our sampling sites, relative to where those plots fall on the winter and summer ranges.

# First, start by just comparing inside vs. outside the 50% kernel polygon we just created

# something along the lines of st_contains(winter_core, sites) should do it?
# bring in the sampling locations data and tidy up a bit
sites <- read_csv('input/sp_prep.csv') %>% 
  dplyr::select(c(site, lc, species, x, y, Lat, Long, percent_N)) %>%
  filter(species != "Black_spruce" & species != "graminoid_spp.")

# convert to sf object
locs <- st_as_sf(sites, coords = c("x", "y"), crs = crs$wkt)

# extract which sites are within vs. without the 50% range
all_inside <- c(st_intersects(locs, land_area, sparse = FALSE))
all_sp <- cbind(locs, all_inside) %>%
  rename(winter_range = all_inside)

# Is percent N higher inside the winter range vs. outside, depending on species?
ggplot(all_sp, aes(x = species, y = percent_N, colour = winter_range)) +
  geom_boxplot() +
  theme_bw()

# Most species appear to be very slightly higher in N at sites inside the 50% kernel vs. outside, but not really any significant differences based on this dichotomous variable
# Alder appears to be behaving opposite the rest though, which is perhaps interesting given it is a nitrogen fixer

# Using continuous winter kernel estimate at each site, rather than 50% kernel inside vs. outside? -----
# convert UD into a raster from which to extract % kernel by site
w_rast <- raster(winter.ud)
s_rast <- raster(summer.ud)

DT <- setDT(locs)

# function to extract raster value at XY points
extract_pt <- function(DT, layer, coords = FALSE) {
  object_name <- deparse(substitute(layer))
  DT[, (paste0('pt_', object_name)) := extract(layer, cbind(.SD)),
     .SDcols = c(coords)]
}

extract_pt(DT, w_rast, coords = c("x", "y")) # winter kernel
extract_pt(DT, s_rast, coords = c("x", "y")) # summer kernel
DT %<>% rename(winter_k = pt_w_rast,
               summer_k = pt_s_rast)

saveRDS(DT, "output/stoich_w_continuous_kde.Rds")

## Plot: % N by kernel (lower KDE values = more intensely used areas, so we'd expect a negative slope)
ggplot(DT, aes(x = winter_k, y = percent_N, colour = species)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  scale_colour_viridis(discrete = TRUE, option = "B", end = 0.9) +
  theme_bw() +
  labs(x = "Winter KDE", y = "% nitrogen content", title = "Forage N content by winter intensity of use")
ggsave(filename = "graphics/maps/N_by_winter_KDE_continuous.png",
       height = 1600,
       width = 2000,
       units = "px")

## Plot: % N by winter kernel, but facet out each species so it's a bit easier to see
ggplot(DT, aes(x = winter_k, y = percent_N)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  facet_wrap(vars(species), nrow = 2) +
  theme_bw() +
  labs(x = "Winter KDE", y = "% nitrogen content", title = "Forage N content by winter intensity of use")
ggsave(filename = "graphics/maps/N_by_winter_KDE_continuous_faceted.png",
       height = 1600,
       width = 2400,
       units = "px")

# Seems like there might be an effect for at least some species
# Want to look at what the slope is for each individual forage

spp <- unique(DT$species)

for(i in spp){
  
  dat <- DT %>% filter(species == i)
  print(i)
  print(summary(i <- lm(percent_N ~ winter_k, data = dat)))
}
# From these model outputs:
# Crowberry and lingonberry have higher %N in higher caribou intensity areas
# most other species tend towards the same, but not a significant effect
# Alder has a non-significant opposite response with potentially higher N in lower-use caribou areas

## Also, for sake of comparison, what about summer kernel relationship to %N 
ggplot(DT, aes(x = summer_k, y = percent_N, colour = species)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  scale_colour_viridis(discrete = TRUE, option = "B", end = 0.9) +
  theme_bw() +
  labs(x = "Summer KDE", y = "% nitrogen content", title = "Forage N content by summer intensity of use")
ggsave(filename = "graphics/maps/N_by_summer_KDE_continuous.png",
       height = 1600,
       width = 2000,
       units = "px")

# Again, facet out each species
ggplot(DT, aes(x = summer_k, y = percent_N)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  facet_wrap(vars(species), nrow = 2) +
  theme_bw() +
  labs(x = "Summer KDE", y = "% nitrogen content", title = "Forage N content by summer intensity of use")
ggsave(filename = "graphics/maps/N_by_summer_KDE_continuous_faceted.png",
       height = 1600,
       width = 2400,
       units = "px")

# Appears like ther emight be a bit more of an effect with summer locations

for(i in spp){
  dat <- DT %>% filter(species == i)
  print(i)
  print(summary(i <- lm(percent_N ~ summer_k, data = dat)))
}

## blueberry, deergrass, Kalmia, and lingonberry all have higher %N in higher summer use areas by caribou
## dwarf birch might have lower %N in higher summer use areas? (p = 0.07)
## Is this an effect of N deposition or are caribou acting as an indicator of underlying higher-quality forage locations... is this just evidence of selection vs. evidence of caribou actually causing the increase


# 20 January - comparing upper and lower quartiles, maybe?

DT <- readRDS('output/stoich_w_continuous_kde.Rds')
hist(DT$winter_k)
# two natural breakpoints in the data, between 26 and 26%, and 61 and 73%
# take everything below 30 and above 70 as high vs. low
DT %<>% mutate(winter_bin = ifelse(winter_k < 0.3, "core", NA),
               winter_bin = ifelse(winter_k > 0.7, "peripheral", winter_bin))

bins <- DT %>% filter(!is.na(winter_bin))

ggplot(bins, aes(x = winter_bin, y = percent_N)) +
  geom_boxplot() +
  facet_wrap(vars(species), nrow = 2) +
  theme_bw()

summary(lm(percent_N ~ winter_bin*species, data = bins))

spp <- unique(bins$species)

for(i in spp){
  
  dat <- bins %>% filter(species == i)
  print(i)
  print(summary(i <- lm(percent_N ~ winter_bin, data = dat)))
}

# Eh, basically the same as the linear regression, nevermind

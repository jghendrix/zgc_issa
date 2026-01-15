library(raster)
library(SDMTools)
library(adehabitatHR)
library(ctmm)

# 7 November - Bhattacharya overlap of summer and winter population kernels------
summer <- readRDS('output/kernels/summer_north_akdes.Rds')
summer.ud <- mean(summer_sub)
# giving an error... [[5]] is not a UD object, it's a ctmm object, remove it
summer_sub <- summer[-5]
summer.ud <- mean(summer_sub)

winter <- readRDS('output/kernels/winter_north_akdes.Rds')
winter.ud <- mean(winter)

fogo_poly <- read_sf('input/FogoPoly/FogoPoly.shp')

## 12 January - polygons of summer and winter ranges
### 95% kdes cover the entire island, basically
### 50% kdes might make more sense?
### also don't know if we want convex or actual kdes

summer_poly <- SpatialPolygonsDataFrame.UD(summer.ud,
                                          level.UD = 0.95)
summer_con <- SpatialPolygonsDataFrame.UD(summer.ud, convex = TRUE,
                                           level.UD = 0.95)
core_summer_poly <- SpatialPolygonsDataFrame.UD(summer.ud, level.UD = 0.5)
core_summer_con <- SpatialPolygonsDataFrame.UD(summer.ud, convex = TRUE,
                                               level.UD = 0.5)
#terra::plot(summer_poly)
#terra::plot(summer_con)
summary(summer.ud, level.UD = 0.95) # 95% summer akde is 197 km2
summary(summer.ud, level.UD = 0.50) # 50% summerakde is 51.7 km2
summary(summer.ud, level.UD = 0.10) # 10% winter akde is 4.2 km2


# same for winter, now
winter_poly <- SpatialPolygonsDataFrame.UD(winter.ud, level.UD = 0.95)
winter_con <- SpatialPolygonsDataFrame.UD(summer.ud, convex = TRUE,
                                          level.UD = 0.95)
core_winter_poly <- SpatialPolygonsDataFrame.UD(winter.ud, level.UD = 0.5)
core_winter_con <- SpatialPolygonsDataFrame.UD(winter.ud, convex = TRUE,
                                          level.UD = 0.5)

summary(winter.ud, level.UD = 0.95) # 95% winter akde is 270 km2
# 95% kernel is bigger than summer? but uncertainty much larger too
summary(winter.ud, level.UD = 0.50) # 50% winter akde is 56.2 km2
# again, larger than core area in summer. WTF.
summary(winter.ud, level.UD = 0.10) # 10% winter akde is 6.0 km2... still bigger

# Ok, why is winter home range LARGER now? ----
## it seems like, if we remove the southern steps, and then calculate kde using those alone, we are removing more summer data than winter. Only looking at the notrh half of the island, we see more relative use in winter
# If we calculate akdes using the whole data set, including southern fixes. And then we take the completed home range estimate and censor it to that boundary, then winter is smaller?

all_winter <- readRDS('output/kernels/winter_akdes.Rds')
winter.full <- mean(all_winter)
all_summer <- readRDS('output/kernels/summer_akdes.Rds')
summer.full <- mean(all_summer)

# What are these range sizes when considering the entire island?
summary(winter.full, level.UD = 0.95) # 270 km2... bigger than Fogo
summary(winter.full, level.UD = 0.50) # 56.2 km2
summary(winter.ud, level.UD = 0.10) # 6.0 km2

summary(summer.full, level.UD = 0.95) # 345 km2... way bigger than Fogo
summary(summer.full, level.UD = 0.50) # 80.8 km2
summary(summer.full, level.UD = 0.10) # 11.1 km2

# so the 10%, 50% and 95% are all bigger in summer now
# the 95% is bigger than the entire island in both cases. The 50% makes sense to use, I think?


# Plotting -----
# need the polygons as sf objects
s_sf <- st_as_sf(summer_poly)
s_vex_sf <- st_as_sf(summer_con)
s_core_sf <- st_as_sf(core_summer_poly)
s_core_vex_sf <- st_as_sf(core_summer_con)

w_sf <- st_as_sf(winter_poly)
w_vex_sf <- st_as_sf(winter_con)
w_core_sf <- st_as_sf(core_winter_poly)
w_core_vex_sf <- st_as_sf(core_winter_con)

# Ok, now we just want the underlying island to plot them on top of

tar_load(lichen)
lich <- rast(lichen)
epsg <- 32621
crs <- st_crs(epsg)
crs_sp <- CRS(crs$wkt)
crs(lich) <- crs$wkt
names(lich) <- "N_Cladonia"
varnames(lich) <- "N_Cladonia"
lich_poly <- crop(lich, fogo_poly, mask = TRUE) %>%
  filter(y > 5503000)


## Plot: Summer 95% range over lichen quality -----
ggplot() +
  geom_spatraster(data = lich_poly) +
  geom_spatvector(data = s_sf, alpha = 0.3)+
  labs(fill = "%N Cladonia") +
  ggtitle("North GPS fixes summer 95% aKDE polygons, with 95% CI")
ggsave(filename = "graphics/maps/summer_north_95_lichen.png",
               height = 2000,
               width = 2400,
               units = "px")
# summer 95% convex
ggplot() +
  geom_spatraster(data = lich_poly) +
  geom_spatvector(data = s_vex_sf, alpha = 0.3) +
  labs(fill = "%N Cladonia") +
  ggtitle("North GPS fixes summer 95% aKDE convex, with 95% CI")
ggsave(filename = "graphics/maps/summer_north_convex_95_lichen.png",
       height = 2000,
       width = 2400,
       units = "px")

# summer core
ggplot() +
  geom_spatraster(data = lich_poly) +
  geom_spatvector(data = s_core_sf, alpha = 0.3) +
  labs(fill = "%N Cladonia") +
  ggtitle("North GPS fixes summer 50% aKDE, with 95% CI")
ggsave(filename = "graphics/maps/summer_north_50_lichen.png",
       height = 2000,
       width = 2400,
       units = "px")
# summer core convex
ggplot() +
  geom_spatraster(data = lich_poly) +
  geom_spatvector(data = s_core_vex_sf, alpha = 0.3) +
  labs(fill = "%N Cladonia") +
  ggtitle("North GPS fixes summer 50% aKDE convex, with 95% CI")
ggsave(filename = "graphics/maps/summer_north_convex_50_lichen.png",
       height = 2000,
       width = 2400,
       units = "px")


# Plots in Winter: 95% polygon over lichen -----
ggplot() +
  geom_spatraster(data = lich_poly) +
  geom_spatvector(data = w_sf, alpha = 0.3)+
  labs(fill = "%N Cladonia") +
  ggtitle("North GPS fixes winter 95% aKDE polygons, with 95% CI")
ggsave(filename = "graphics/maps/winter_north_95_lichen.png",
       height = 2000,
       width = 2400,
       units = "px")
# winter 95% convex
ggplot() +
  geom_spatraster(data = lich_poly) +
  geom_spatvector(data = w_vex_sf, alpha = 0.3) +
  labs(fill = "%N Cladonia") +
  ggtitle("North GPS fixes winter 95% aKDE convex, with 95% CI")
ggsave(filename = "graphics/maps/winter_north_convex_95_lichen.png",
       height = 2000,
       width = 2400,
       units = "px")

# winter core
ggplot() +
  geom_spatraster(data = lich_poly) +
  geom_spatvector(data = w_core_sf, alpha = 0.3) +
  labs(fill = "%N Cladonia") +
  ggtitle("North GPS fixes winter 50% aKDE, with 95% CI")
ggsave(filename = "graphics/maps/winter_north_50_lichen.png",
       height = 2000,
       width = 2400,
       units = "px")
# winter core convex
ggplot() +
  geom_spatraster(data = lich_poly) +
  geom_spatvector(data = w_core_vex_sf, alpha = 0.3) +
  labs(fill = "%N Cladonia") +
  ggtitle("North GPS fixes winter 50% aKDE convex, with 95% CI")
ggsave(filename = "graphics/maps/winter_north_convex_50_lichen.png",
       height = 2000,
       width = 2400,
       units = "px")


# All the plots above are using only northern GPS fixes, in which case winter ranges appear larger. What if we plot the akdes from the complete data set?

winter_50_full <- SpatialPolygonsDataFrame.UD(winter.full, level.UD = 0.5)
w_full_sf <- st_as_sf(winter_50_full)

summer_50_full <- SpatialPolygonsDataFrame.UD(summer.full, level.UD = 0.5)
s_full_sf <- st_as_sf(summer_50_full)

# all winter data, core 50% plot
ggplot() +
  geom_spatraster(data = lich_poly) +
  geom_spatvector(data = w_full_sf, alpha = 0.3) +
  labs(fill = "%N Cladonia") +
  ggtitle("Full GPS dataset winter 50% aKDE, with 95% CI")
ggsave(filename = "graphics/maps/winter_whole-island_50_lichen.png",
       height = 2000,
       width = 2400,
       units = "px")

# all summer data, core 50% plot
ggplot() +
  geom_spatraster(data = lich_poly) +
  geom_spatvector(data = s_full_sf, alpha = 0.3) +
  labs(fill = "%N Cladonia") +
  ggtitle("Full GPS dataset summer 50% aKDE, with 95% CI")
ggsave(filename = "graphics/maps/summer_whole-island_50_lichen.png",
       height = 2000,
       width = 2400,
       units = "px")



# 14 January -----
## 1. We want area of the winter range. 50% kde estimate is 56 km2, but this includes all of Shoal Bay. can we calculate the area excluding ocean?
## 2. %N of lichen and the %N of plants from all the sites we have data for in the winter range and %N of lichen and the %N of plants from all the other sites. then do a simple ANOVA or Chi-Square to see if winter grounds have higher %N in lichen or vascular

# 1 - winter area
w_core_sf
# three objects, the mean estimate and then 95% CI either side of that
# can we calculate the area of each of these polygons that is off the coast?
ctmm::plot(fogo_poly)
# fogo_poly excludes freshwater too... which during winter, they do walk on sometimes... idk let's leave that be for now.

# calculate intersection of these polygons?
land_area <- st_intersection(w_core_sf, fogo_poly)

st_crs(w_core_sf)
st_crs(fogo_poly) # NAD83, let's change this to match the caribou data

epsg <- 32621
crs <- st_crs(epsg)
crs_sp <- CRS(crs$wkt)
fogo_trans <- st_transform(fogo_poly, crs = crs$wkt)

land_area <- st_intersection(fogo_trans, w_core_sf)
# this worked, but I think it did it for the upper 95% CI... I would like this to give me three estimates
low <- w_core_sf %>% slice(1)
est <- w_core_sf %>% slice(2)
high <- w_core_sf %>% slice(3)

land_lower <- st_intersection(fogo_trans, low)
land_area <- st_intersection(fogo_trans, est)
land_upper <- st_intersection(fogo_trans, high)


ggplot() +
  geom_spatvector(data = land_lower, alpha = 0.3, fill = "darkgreen") +
  geom_spatvector(data = land_area, alpha = 0.3, fill = "darkgreen") +
  geom_spatvector(data = land_upper, alpha = 0.3, fill = "darkgreen")

terr_HR <- rbind(land_lower, land_area, land_upper) %>%
  mutate(Id = c("lower", "est", "upper"))
saveRDS(terr_HR, 'output/kernels/winter_50_land_area.Rds')

winter_areas <- st_area(terr_HR)/1000000

# Winter 50% home range land area = 38.39 km2 (LCL = 15.67, UCL = 76.04)

# what is the nitrogen content within these areas?
lich_north <- lich_poly %>%
  filter(y > 5503000)


#lich_inside <- st_intersection(lich_north, land_area)
lich_vals <- extract(lich_north, land_area)
mean(lich_vals$N_Cladonia, na.rm = T) # 0.3245888 %N in Cladonia within the mean winter core range


# want a polygon EXCLUDING the winter range
outside <- st_sym_difference(fogo_trans, land_area)
lich_outside <- extract(lich_north, outside)
mean(lich_outside$N_Cladonia, na.rm = T) # 0.3386499 %N in Cladonia outside the mean winter core range

outside <- lich_outside %<>% filter(!is.na(N_Cladonia)) %>%
  mutate(site = "outside")

inside <- lich_vals %<>% filter(!is.na(N_Cladonia)) %>%
  mutate(site = "inside")


Cladonia <- rbind(outside, inside)

ggplot(Cladonia, aes(x = site, y = N_Cladonia)) +
  geom_boxplot()
summary(lm(N_Cladonia ~ site, data = Cladonia))
# lichen %N values are slightly higher outside vs. inside the core winter range
## by .01 %, but it is highly significant.

# What about vascular N vs. winter range? ----
tar_load(vascular)
vasc <- rast(vascular)
crs(vasc) <- crs$wkt
names(vasc) <- "N_vascular"
varnames(vasc) <- "N_vascular"
vasc_poly <- crop(vasc, fogo_poly, mask = TRUE)
vasc_north <- vasc_poly %>%
  filter(y > 5503000)

vasc_inside <- extract(vasc_north, land_area)
mean(vasc_inside$N_vascular, na.rm = T)

outside <- st_sym_difference(fogo_trans, land_area)
vasc_outside <- extract(vasc_north, outside)
mean(vasc_outside$N_vascular, na.rm = T) 
# 2.21 %N in vascular plants inside the mean winter core range
# 2.35 %N in vascular plants outside winter range

v_outside <- vasc_outside %<>% filter(!is.na(N_vascular)) %>%
  mutate(site = "outside")
v_inside <- vasc_inside %<>% filter(!is.na(N_vascular)) %>%
  mutate(site = "inside")

vascular <- rbind(v_outside, v_inside)

summary(lm(N_vascular ~ site, data = vascular))
# vascular %N values are  higher outside vs. inside the core winter range
## by .14 %, but highly significant

ggplot(vascular, aes(x = site, y = N_vascular)) +
  geom_boxplot()



# Ok, well then. Nitrogen levels in both vascular and lichen are higher OUTSIDE of the core winter range. So much for that.

# What if we go even more core? 10%? ----
winter_10 <- SpatialPolygonsDataFrame.UD(winter.full, level.UD = 0.1)
w_10_sf <- st_as_sf(winter_10)

low10 <- w_10_sf %>% slice(1)
mean10 <- w_10_sf %>% slice(2)
high10 <- w_10_sf %>% slice(3)

lower10 <- st_intersection(fogo_trans, low10)
inside10 <- st_intersection(fogo_trans, mean10)
upper10 <- st_intersection(fogo_trans, high10)


# 10% KDE vs. Cladonia N
inside_10_lichen <- extract(lich_north, inside10)
mean(inside_10_lichen$N_Cladonia, na.rm = T) # 0.3255207 %N in Cladonia within the 10% winter KDE

# want a polygon EXCLUDING the winter 10% range
outside10 <- st_sym_difference(fogo_trans, inside10)
lich_outside10 <- extract(lich_north, outside10)
mean(lich_outside10$N_Cladonia, na.rm = T) 
# 0.335053 %N in Cladonia outside the mean winter core range


ggplot() +
  geom_spatraster(data = lich_north) +
  geom_spatvector(data = inside10, alpha = 0.3, fill = "darkgreen")


# What about at the 10% KDE for vascular? ----
vasc_inside10 <- extract(vasc_north, inside10)
mean(vasc_inside10$N_vascular, na.rm = T) # 2.108391

vasc_outside10 <- extract(vasc_north, outside10)
mean(vasc_outside10$N_vascular, na.rm = T) 
# 2.11 %N in vascular plants inside the 10% winter aKDE
# 2.32 %N in vascular plants outside winter 10% KDE


# Kristy wants actual lichen nitrogen samples relative to seasonal ranges, not the predicted values -----

# something along the lines of st_contains(winter_core, sites) should do it?
library(tidyverse)
sites <- read_csv('input/sp_prep.csv')
clad <- sites %>% filter(species == "Cladonia") %>%
  dplyr::select(c(site, percent_N, x, y, Lat, Long))
c_locs <- clad %>% filter(!duplicated(paste0(x, y)))

points <- st_as_sf(c_locs, coords = c("x", "y"), crs = crs$wkt)

locs <- sites %>% dplyr::select(c(site, lc, species, x, y, Lat, Long, percent_N))
locs <- st_as_sf(locs, coords = c("x", "y"), crs = crs$wkt)

inside <- c(st_intersects(points, land_area, sparse = FALSE))

all_inside <- c(st_intersects(locs, land_area, sparse = FALSE))

all_sp <- cbind(locs, all_inside)

ggplot() +
  geom_spatvector(data = land_area) +
  geom_point(data = c_locs, aes(x = x, y = y))
# I think this actually worked!

group <- pts %>% dplyr::select(Lat, Long, inside)

clad_g <- left_join(clad, group, by = c("Lat", "Long"))


ggplot(clad_g, aes(x = inside, y = percent_N)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.4) +
  xlab("Within winter range") +
  ylab("%N in Cladonia")

clad_g %>% group_by(inside) %>%
  summarise(n = n(),
            mean = mean(percent_N, na.rm = T), 
            median = median.default(percent_N, na.rm = T), 
            sd = sd(percent_N, na.rm = T))


summary(lm(percent_N ~ inside, data = clad_g))
# No difference wrt lichen N, at least

all_sp %<>%
  filter(species != "Black_spruce" & species != "graminoid_spp.") %>%
  rename(winter_range = all_inside)


summary(lm(percent_N ~ winter_range*species, data = all_sp))
ggplot(all_sp, aes(x = species, y = percent_N, colour = winter_range)) +
  geom_boxplot()

lcs <- all_sp %>% filter(!duplicated(site)) %>%
  group_by(lc, winter_range) %>%
  summarise(n = n()) %>%
  ungroup()

lcs %<>% group_by(winter_range) %>%
  mutate(prop_inside = n/sum(n)) %>%
  ungroup() %>%
  dplyr::select(-c(geometry))

summary(lm(percent_N ~ winter_range + lc, data = subset(all_sp, species == "Cladonia")))

cladonia <- all_sp %>% filter(species == "Cladonia") %>%
  mutate(open = ifelse(lc %in% c("Wetland", "Scrub", "Rocky", "Lichen"), "open", "closed"))

ggplot(cladonia, aes(x = open, y = percent_N, colour = winter_range)) +
  geom_boxplot()
summary(lm(percent_N ~ winter_range*open, data = cladonia))


terra::plot(lich_north)

sk <- rast(raster(summer.ud))
wk <- rast(raster(winter.ud))

scrop <- crop(sk, fogo_poly, mask = TRUE) #%>%
  rename(summer = layer) %>%
  filter(y > 5503000)
wcrop <- crop(wk, fogo_poly, mask = TRUE) %>%
  rename(winter = layer) %>%
  filter(y > 5503000)



ctmm::projection(summer.ud)
ctmm::projection(winter.ud)

r <- stack(summer.ud, winter.ud)
ctmm::overlap(c(summer.ud, winter.ud))

# it'll do between individuals within a season... how do I get it to do season vs. season for the population average?
# winter UD area 12.20485
# summer UD area 23.91385
seasons <- c(winter.ud, summer.ud)
ctmm::overlap(seasons)

# Inconsistent grid resolutions, apparently

UD.seasonal <- list(summer = summer.ud, winter = winter.ud)



ctmm::projection(winter.ud) <- crs_sp # doesn't work
# there's no way to reproject within ctmm according to the package author. Need to have the rasters aligned first, then "assign their arrays back to the UD object and update the dr slot."

UD.combined
ctmm::overlap(UD.combined) #this is estUD, not UD.... fucking eh


class(UD.combined) <- "estUDm"
kerneloverlaphr(UD.combined, method = "UDOI")


## Comparing summer and winter kernels 21 October -----
# they have different resolutions but also different extents
fogo_poly <- read_sf('input/FogoPoly/FogoPoly.shp') #%>%

sk <- rast(raster(summer.ud))
wk <- rast(raster(winter.ud))

scrop <- crop(sk, fogo_poly, mask = TRUE) %>%
  rename(summer = layer) %>%
  filter(y > 5503000)
wcrop <- crop(wk, fogo_poly, mask = TRUE) %>%
  rename(winter = layer) %>%
  filter(y > 5503000)

 w_resampled <- resample(wcrop, scrop)
# w_resampled %<>% filter(y > 5503000)
# scrop %<>% filter(y > 5503000)
# sum_rast <- scrop
# win_rast <- w_resampled

## UDOI to better quantify overlap of summer and winter ranges... ----

amt::hr_overlap(w_resampled, scrop, type = "udoi")
# can't be a SpatRaster... needs to be an hr object

# is it easiest to just recalculate home ranges in amt?

eroy <- amt_fisher |> filter(name == "Leroy")

tar_load(locs_prep)
winter <- locs_prep %>% filter(doy < 61) %>%
  make_track(x_, y_, key = Animal_ID)
summer <- locs_prep %>% filter(doy >= 167 & doy <= 228) %>%
  make_track(x_, y_, key = Animal_ID)

wsea <- winter |> filter(key == "FO2017007")

wrast <- make_trast(winter, res = 25)
srast <- make_trast(summer, res = 25)
hr_w <- hr_kde(winter, trast = wrast, levels = c(0.5, 0.9))


wrast <- raster(w_resampled)
srast <- raster(scrop)
amt::hr_overlap(wrast, srast, type = "udoi")


### Ok, let's try this again starting from the rasters ------
summer <- readRDS('output/kernels/summer_akdes.Rds')
winter <- readRDS('output/kernels/akdes.Rds')
sx <- mean(summer)
wx <- mean(winter)

#sdf <- as.data.frame(sx)
#res(wk)





## Nope, that's not working

# starting with .tif population rasters for summer and winter ----
sum_rast <- raster(scrop)
win_rast <- raster(w_resampled)

sumdf <- as(sum_rast, "SpatialPixelsDataFrame")
windf <- as(win_rast, "SpatialPixelsDataFrame")


fullgrid(sumdf) <- FALSE
fullgrid(windf) <- FALSE

hli <- list(h = 1, meth = "specified")

sumUD <- new("estUD", sumdf)

winUD <- new("estUD", windf)

slot(sumUD, "h") <- hli
slot(sumUD, "vol") <- FALSE

slot(winUD, "h") <- hli
slot(winUD, "vol") <- FALSE

#Combine estUDs and convert to estUDm
UD.combined <- list(summer = sumUD, winter = winUD)
class(UD.combined) <- "estUDm"

kerneloverlaphr(UD.combined, method = c("UDOI"), na.rm = T)
# giving an error message because there are NAs in the dataframe


# I can't use the amt code to calculate UDOI because I have NAs in my dataframe
# there are NAs because it's the fucking ocean and all the ponds
# how do i clip the raster to only the island itself??

# can I just do this manually somehow??











## Lazy version: just want the correlation coefficient from the Spat Rasters?? ----


both <- c(scrop, w_resampled)
terra::plot(both) # ggtitle("Seasonal utilization distributions")


# Correlation between layers
cor(
  values(both)[, 1],
  values(both)[, 2],
  use = "na.or.complete"
)
# 8 % correlation between summer and winter density kernels
# Not quite a negative correlation... but visually they appear to be using quite different locations no

# Can we simplify this to the same northern half we did our models?
north <- both %>% filter(y > 5503000)
terra::plot(north)

cor(
  values(north)[, 1],
  values(north)[, 2],
  use = "na.or.complete"
)
# Looking specifically at this northern half, correlation is -21.6 %


# Can we bring in the stoich maps to this now?

tar_load(c(lichen, vascular))
lich <- rast(lichen)
vasc <- rast(vascular)
lich_res <- resample(lich, north)
vasc_res <- resample(vasc, north)
trio <- c(north, lich_res)
quad <- c(trio, vasc_res)

wc <- c(w_resampled, lich_res)
sc <- c(w_resampled, vasc_res)

nquad <- quad %>% filter(y > 5503000)
# summer, winter, lichen, vascular kernel

cor(
  values(sc)[, 1],
  values(sc)[, 2],
  use = "na.or.complete"
)

# Pairwise correlations between kernel rasters and predicted N rasters.... -----
# 12.4% correlation between winter kernel and Cladonia N
# 12.6% correlation between summer kernel and Cladonia N
# 11.0% between winter and vascular N
# -46.8% correlation between summer and vascular N???


# Calculating discrete seasonal ranges. What kernel % do we want to use?



# Old explorations -----
#ctmm::plot(AKDES[[1]], cont=50)
#adf <- data.frame(AKDES[[1]])

id3 <- AKDES[[3]]
id3$scaled <- id3$PDF/m

for(i in 1:44) {
#  AKDES[[i]]$scaled <- AKDES[[i]]$PDF/max(AKDES[[i]]$PDF)
  
 # writeRaster(AKDES[[i]],
#              filename = paste0('output/UDs/population_UD_', i, '.tif'), 
#              format = "GTiff", DF = "PDF")
}


#writeRaster(pop.ud, 'output/population_UD.tif', format = "GTiff", DF = "PDF")

## Spare code? -----
#DT <- w_fixes
DT %<>% dplyr::select(Longitude = x_, 
                      Latitude = y_, 
                      id = Animal_ID,
                      timestamp = t_)

conflicts_prefer(ctmm::head)

DTt <- as.telemetry(DT, projection = crs("epsg:32621")) 


# check what it's projected as...
ctmm::projection(DTt)
# ok, good to go

# calculate fit guess object
#GUESS <- ctmm.guess(DTt, interactive = FALSE)
# This won't work for a set of individuals.... ok
# try the loop?

FITS <- AKDES <- list()

for(i in 1:length(DTt)) {
  
  GUESS <- ctmm.guess(DTt[[i]], interactive=FALSE)
  # use ctmm.select unless you are certain that the selected model is OUF
  FITS[[i]] <- ctmm.fit(DTt[[i]],GUESS)
  
}
ctmm::projection(AKDES)
AKDES <- akde(DTt, FITS)

saveRDS(AKDES, 'output/summer_akdes.Rds')

# color to be spatially distinct
# COL <- color(AKDES, by='individual')

# plot AKDEs
#  ctmm::plot(AKDES,col.DF=COL,col.level=COL,col.grid=NA,level=NA)


AKDES <- readRDS('output/kernels/summer_akdes.Rds')

pop.ud <- mean(AKDES)

writeRaster(pop.ud, 'output/summer_population_UD.tif', format = "GTiff", DF = "PDF")




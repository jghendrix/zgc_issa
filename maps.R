# Visualizations - maps
# first plotting the StDM predicted surfaces
# then looking at the forest cover data
# then the aKDEs derived from caribou GPS collar data

# Import Fogo Island shape file
fogo_poly <- read_sf('input/FogoPoly/FogoPoly.shp')

# Define CRS
epsg <- 32621
crs <- st_crs(epsg)
crs_sp <- CRS(crs$wkt)

fogo <- st_transform(fogo_poly, crs$wkt)

# Predicted nitrogen content of Cladonia and vascular plants----

# import StDMs
tar_load(c(vascular, lichen))
lich <- rast(lichen)
vasc <- rast(vascular)
crs(lich) <- crs$wkt
crs(vasc) <- crs$wkt

# update variable names
names(lich) <- "N_Cladonia"
varnames(lich) <- "N_Cladonia"
names(vasc) <- "N_vasc"
varnames(vasc) <- "N_vasc"

# crop to Fogo perimeter
lich_poly <- crop(lich, fogo, mask = TRUE)
vasc_poly <- crop(vasc, fogo, mask = TRUE)

# %N in Cladonia predicted surface
ggplot() + 
  geom_spatraster(data = lich_poly) +
  scale_fill_terrain_c() +
  xlim(c(693000, 717000)) + # 20k
  ylim(c(5503000, 5515000)) + #12k
  labs(fill = "Cladonia %N") +
  theme_bw()

ggsave(filename = "graphics/maps/lichen_N_map.png",
       height = 1600,
       width = 3200,
       units = "px") # 2:1 ratio in figure pixels works out to square easting/northing at a 3:5 ratio when including legend

# Vascular %N predicted surface
ggplot() + 
  geom_spatraster(data = vasc_poly) +
  scale_fill_terrain_c() +
  xlim(c(693000, 717000)) + # 20k
  ylim(c(5503000, 5515000)) + #12k
  labs(fill = "Vascular %N") +
  theme_bw()
ggsave(filename = "graphics/maps/vascular_N_map.png",
       height = 1600,
       width = 3200,
       units = "px")

# Proportion forest cover surface ----

forest <- raster('output/prop_forest.tif')
f.terra <- rast(forest)
crs(f.terra) <- crs$wkt

names(f.terra) <- "prop_forest"
varnames(f.terra) <- "prop_forest"

# Crop raster to Fogo perimeter
forest_poly <- crop(f.terra, fogo, mask = TRUE)

# Proportion forest cover plot
ggplot() + 
  geom_spatraster(data = forest_poly) +
  scale_fill_terrain_c() +
  xlim(c(693000, 717000)) + # 20k
  ylim(c(5503000, 5515000)) + #12k
  labs(fill = "% forest cover") +
  theme_bw()

ggsave(filename = "graphics/maps/forest_cover_map.png",
       height = 1600,
       width = 3200,
       units = "px") # 2:1 ratio works for a 3:5 ratio + legend


## Caribou aKDEs ----
# import UD rasters
summer <- raster('output/summer_population_UD.tif')
tsum <- rast(summer) 

# crop to bounding box
e <- c(657382.9, 753139.2, 5503002, 5529230)

scrop <- crop(tsum, e) %>%
  mutate(UD = summer_population_UD/max(summer_population_UD),
         KDE = 1 - UD)
crs(scrop) <- crs$wkt

summer_poly <- crop(scrop, fogo, mask = TRUE)
  
winter <- raster('output/winter_population_UD.tif')
twin <- rast(winter) 

wcrop <- crop(twin, e) %>%
  rename(UD = winter_population_UD) %>%
  mutate(UD = winter_population_UD/max(winter_population_UD),
         KDE = 1 - UD)

crs(wcrop) <- crs$wkt
winter_poly <- crop(wcrop, fogo, mask = TRUE) 

# Summer kernel
ggplot() + 
  geom_spatraster(data = scrop, aes(fill = KDE)) +
  scale_fill_viridis_c(na.value = "transparent", labels = label_percent()) +
  xlim(c(693000, 717000)) + # 20k
  ylim(c(5503000, 5515000)) + #12k
  ggplot2::annotate("text", 
                    x = 694000, y = 5514000, 
                    label = "A", size = 10) +
  labs(fill = "KDE", x = "", y = "") +
  theme_bw()
ggsave(filename = "graphics/maps/summer_KDE.png",
       height = 1000,
       width = 2000,
       units = "px")

# Winter kernel
ggplot() +
  geom_spatraster(data = wcrop, aes(fill = KDE)) +
  scale_fill_viridis_c(na.value = "transparent", labels = label_percent()) + 
  xlim(c(693000, 717000)) +
  ylim(c(5503000, 5515000)) + 
  ggplot2::annotate("text", 
                    x = 694000, y = 5514000, 
                    label = "B", size = 10) +
  labs(fill = "KDE", x = "", y = "") +
  theme_bw()
ggsave(filename = "graphics/maps/winter_KDE.png",
       height = 1000,
       width = 2000,
       units = "px")


## 18 August: how many of the steps are in the southeast region that Reviewer2 flagged? ----

# what are the boundaries of this area we're concerned with?
# approximately... south of 5507000, east of 707000

SEw <- winter_prep %>% filter(x1_ > 706000, y1_ < 5507000)
SEs <- summer_prep %>% filter(x1_ > 706000, y1_ < 5507000)
length(SEs$x1_)/length(summer_prep$x1_) # 29.6 % ...
length(SEw$x1_)/length(winter_prep$x1_) # 17.3 % ...
# so almost 30% of summer steps are in this region, but only 17% of winter steps at least?

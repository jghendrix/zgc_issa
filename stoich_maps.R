# Making maps of predictive stoich layers

fogo_poly <- read_sf('input/FogoPoly/FogoPoly.shp')
tar_load(c(vascular, lichen))
lich <- rast(lichen)
vasc <- rast(vascular)
forest <- raster('output/prop_forest.tif')
f.terra <- rast(forest)

epsg <- 32621
crs <- st_crs(epsg)
crs_sp <- CRS(crs$wkt)

fogo <- st_transform(fogo_poly, crs$wkt)
crs(lich) <- crs$wkt
crs(vasc) <- crs$wkt
crs(f.terra) <- crs$wkt

# update the names to match the model
names(lich) <- "N_Cladonia"
varnames(lich) <- "N_Cladonia"
names(vasc) <- "N_vasc"
varnames(vasc) <- "N_vasc"
names(f.terra) <- "prop_forest"
varnames(f.terra) <- "prop_forest"


forest_poly <- crop(f.terra, fogo, mask = TRUE)
lich_poly <- crop(lich, fogo, mask = TRUE)
vasc_poly <- crop(vasc, fogo, mask = TRUE)


# Lichen stoich surface
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
       units = "px") # 2:1 ratio works for a 3:5 ratio + legend

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
       units = "px") # 2:1 ratio works for a 3:5 ratio + legend

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


## And the seasonal kernels, using the same ylims ----

summer <- raster('output/summer_population_UD.tif')
tsum <- rast(summer) %>%
  rename(kernel = summer_population_UD) %>%
  mutate(kernel = kernel/max(kernel))
crs(tsum) <- crs$wkt

summer_poly <- crop(tsum, fogo, mask = TRUE)
  
winter <- raster('output/winter_population_UD.tif')
twin <- rast(winter) %>%
  rename(kernel = winter_population_UD) %>%
  mutate(kernel = kernel/max(kernel, na.rm = T))
crs(twin) <- crs$wkt
winter_poly <- crop(twin, fogo, mask = TRUE) 


## This is plotting so the highest intensity locations are higher values... we want these to be kernel density, right? so the hotspots should be like 10% or whatever? Can we just invert these around 0.5?
summer_kde <- summer_poly %>% mutate(KDE = 1 - kernel)

ggplot() + 
  geom_spatraster(data = summer_kde, aes(fill = KDE)) +
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


# Winter looks weird at first, just leave it be
# ggplot() + 
#   geom_spatraster(data = winter_poly) +
#   scale_fill_terrain_c() +
#   #xlim(c(693000, 717000)) + # 20k
#   #ylim(c(5503000, 5515000)) + #12k
#   labs(fill = "Winter % KDE") +
#   theme_bw()
# 
# # why is nothing above like 0.2??
# wmax <- winter_poly %>% filter(kernel > 0.5)
# ggplot() + 
#   geom_spatraster(data = wcrop) +
#   scale_fill_terrain_c() +
#   #xlim(c(693000, 717000)) +
#   #ylim(c(5503000, 5515000)) +
#   labs(fill = "Winter % KDE") +
#   theme_bw()

# there's a crazy dense high spot down by stag harbour... idk what the fuck is going on there
# If we trim the extent and then recalculate the relative kernel?
e <- c(657382.9, 753139.2, 5503002, 5529230)
wcrop <- crop(twin, e)
wcrop %<>% mutate(kernel = kernel/max(kernel, na.rm = T))
winter_poly <- crop(wcrop, fogo, mask = TRUE) 

winter_kde <- winter_poly %>% mutate(KDE = 1 - kernel)

# let's try this again

ggplot() + 
  geom_spatraster(data = winter_kde, aes(fill = KDE)) +
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

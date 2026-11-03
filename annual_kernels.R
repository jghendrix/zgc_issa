#### 15 October - seasonal aKDE home ranges -----
library(ctmm)
library(adehabitatHR)
library(terra)
library(tidyverse)
library(sf)
library(magrittr)
library(tidyterra)
library(viridis)
library(ggfx)

bb <- c(
  xmin = 693000,
  ymin = 5503000,
  xmax = 716900,
  ymax = 5515100)

epsg <- 32621
crs <- st_crs(epsg)
crs_sp <- CRS(crs$wkt)

fogo_poly <- read_sf('input/FogoPoly/FogoPoly.shp')
fogo_n <- st_crop(fogo_poly, y = bb) # crop to the 5503000 southern boundary

sums <- readRDS('output/summer_50_polys.Rds') # 50% contours of annual range as sf() objects
wins <- readRDS('output/winter_50_polys.Rds')

wy <- wins$year

for(i in wy){

  # for 2017, b/c there's no "old" to plot on top of,  we have to pull that out
  current <- wins %>% filter(year == i)
  
  if(i == 2017) {
    
    ggplot() +
      as_reference(geom_sf(data = fogo_n, fill = "grey85", colour = NA),
                   id = "island") +
      with_blend(
        geom_sf(data = current, fill = "violetred4", colour = "violetred4",
                alpha = current$trans),
        blend_type = "multiply",
        bg_layer = "island") +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            legend.position = "blank") +
      ggtitle(paste0("Winter population 50% kernels - ", i, " (n = ", current$winter, ")"))
    
    ggsave(filename = paste0("graphics/maps/kernels/mult_winter_", i, ".png"),
           height = 800,
           width = 1600,
           units = "px")
  } 
  
  else {
  prev <- wins %>% filter(year < i)
  
  ggplot() +
    as_reference(geom_sf(data = fogo_n, fill = "grey85", colour = NA),
                 id = "island") +
    with_blend(
      geom_sf(data = prev, aes(alpha = trans*0.67),
              fill = "slateblue4", colour = NA),
      bg_layer = "island",
      blend_type = "multiply",
      id = "old") +
    with_blend(
      geom_sf(data = current, fill = "violetred4", colour = "violetred4",
              alpha = current$trans),
      blend_type = "multiply",
      bg_layer = "old") +
    scale_alpha_identity() +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          legend.position = "blank") +
    ggtitle(paste0("Winter population 50% kernels - ", i, " (n = ", current$winter, ")"))
  
  ggsave(filename = paste0("graphics/maps/kernels/mult_winter_", i, ".png"),
         height = 800,
         width = 1600,
         units = "px")
  
}
}


# Summer visualizations with multiply layers

sy <- sums$year

for(i in sy){
  
  # this breaks for 2017 b/c there's no "old" to plot on top of, so we have to pull that out
  current <- sums %>% filter(year == i)
  
  if(i == 2016) {
    
    ggplot() +
      as_reference(geom_sf(data = fogo_n, fill = "grey85", colour = NA),
                   id = "island") +
      with_blend(
        geom_sf(data = current, fill = "violetred4", colour = "violetred4",
                alpha = current$trans),
        blend_type = "multiply",
        bg_layer = "island") +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            legend.position = "blank") +
      ggtitle(paste0("Summer population 50% kernels - ", i, " (n = ", current$summer, ")"))
    
    ggsave(filename = paste0("graphics/maps/kernels/mult_summer_", i, ".png"),
           height = 800,
           width = 1600,
           units = "px")
  } 
  
  else {
    prev <- sums %>% filter(year < i)
    
    ggplot() +
      as_reference(geom_sf(data = fogo_n, fill = "grey85", colour = NA),
                   id = "island") +
      with_blend(
        geom_sf(data = prev, aes(alpha = trans*0.67),
                fill = "slateblue4", colour = NA),
        bg_layer = "island",
        blend_type = "multiply",
        id = "old") +
      with_blend(
        geom_sf(data = current, fill = "violetred4", colour = "violetred4",
                alpha = current$trans),
        blend_type = "multiply",
        bg_layer = "old") +
      scale_alpha_identity() +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            legend.position = "blank") +
      ggtitle(paste0("Summer population 50% kernels - ", i, " (n = ", current$summer, ")"))
    
    ggsave(filename = paste0("graphics/maps/kernels/mult_summer_", i, ".png"),
           height = 800,
           width = 1600,
           units = "px")
    
  }
}



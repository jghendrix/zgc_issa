## Intensity of use of caribou, as a raster

# This lumps all years together... how do we map this over yr??

caribou_intensity <- function(DT) {
  
  years <- c(2017, 2018, 2019, 2020, 2023, 2024, 2025)
  
  # using min and max x- and y- values to set the same perimeter for all years
  bbox <- extent(693190, 716540, 5490195, 5514695)

  
  for(i in years){
  
    locs <- DT %>% 
      dplyr::filter(yr == i) %>%
      dplyr::select(x = x_long, y = y_lat)
    
  sf_locs <- st_as_sf(locs, 
                     coords = c("x", "y"),
                     crs = "+proj=longlat +datum=WGS84")
  
  reproj <- st_transform(sf_locs, "EPSG:32621")
  
  empty_kernel_grid <- raster(ext = bbox, resolution = 25, crs = st_crs(reproj))
  
  kernel <- rasterize(coordinates(as_Spatial(reproj)), empty_kernel_grid, fun = 'count', background = 0)
  
  #plot(kernel)
  
  kernel_terra <- rast(kernel)
  
  terra::writeRaster(kernel_terra, 
                     paste0("output/kernel_", i, ".tif"), 
                     filetype = "GTiff",
                     overwrite = TRUE)

  }

}
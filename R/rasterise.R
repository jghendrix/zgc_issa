
rasterise <- function(df){

 df$N_vascular <- rowMeans(subset(df, select = c(3:5, 7:11)), na.rm = T)
  
  write.csv(df, 'output/species_N_by_pixel.csv') 
  
  cladonia <- df %>% dplyr::select(c(x, y, N_Cladonia))
  vascular <- df %>% dplyr::select(c(x, y, N_vascular))
  clad <- rasterFromXYZ(cladonia)
  vasc <- rasterFromXYZ(vascular)
  
  terra::writeRaster(clad, "output/Cladonia_N.tif", filetype = "GTiff")
  terra::writeRaster(vasc, "output/Vascular_N.tif", filetype = "GTiff")
  
}
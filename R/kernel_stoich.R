# Tying overall winter caribou relative density kernel to the stoich data

kernel_stoich <- function(DT, crs) {

 
  locs <- setDT(DT)
  pt <- c('x', 'y')
  
  # bring in kernel data 
  pUD <- raster('output/population_UD.tif')
  
  # These PDF values aren't scaled yet though... they max out at like, .00001, mostly 10e-8 
  # convert to terra so we can manipulate the values
  kernel <- rast(pUD)
  
  tUD <- kernel %>% rename(kernel = population_UD) %>%
    mutate(kernel = kernel/max(kernel))
  
  kernel <- raster(tUD)
  
  if (st_crs(kernel) != crs) {
    kernel <- raster::projectRaster(kernel, crs = crs$wkt, method = 'ngb')
  }
  
  
  extract_pt(locs, kernel, pt)
  return(locs)
  
}
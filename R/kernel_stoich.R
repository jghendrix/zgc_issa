# Tying overall winter caribou relative density kernel to the stoich data

kernel_stoich <- function(DT, kernel, crs) {

 
  DT %<>% filter(species == "Cladonia")
  locs <- setDT(DT)
  pt <- c('x', 'y')
  
  if (st_crs(kernel) != crs) {
    kernel <- raster::projectRaster(kernel, crs = crs$wkt, method = 'ngb')
  }
  
  
  extract_pt(locs, kernel, pt)
  return(locs)
  
}
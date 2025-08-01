## Extracting land cover of given point

extract_lc <- function(DT, crs, lc, legend) {
  
  sites <- setDT(DT)
  
  pt <- c('x', 'y')
  
  if (st_crs(lc) != crs) {
    lc <- raster::projectRaster(lc, crs = crs$wkt, method = 'ngb')
  }
  
  extract_pt(sites, lc, pt)
  sites[legend, lc_description := label, on = .(pt_lc = class)]
  
}
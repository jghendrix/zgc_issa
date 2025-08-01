## Extracting raster value of given point

extract_raster <- function(DT, crs, r1, r2, r3, r4, r5, r6) {

	#sites <- DT %>% filter(!duplicated(paste0(x, y))) %>%
	 # dplyr::select(c(x, y))
  
  sites <- setDT(DT)

  pt <- c('x', 'y')
  
  if (st_crs(r1) != crs) {
    r1 <- raster::projectRaster(r1, crs = crs$wkt, method = 'ngb')
  }
  
  if (st_crs(r2) != crs) {
    r2 <- raster::projectRaster(r2, crs = crs$wkt, method = 'ngb')
  }
  if (st_crs(r3) != crs) {
    r3 <- raster::projectRaster(r3, crs = crs$wkt, method = 'ngb')
  }
  if (st_crs(r4) != crs) {
    r4 <- raster::projectRaster(r4, crs = crs$wkt, method = 'ngb')
  }
  if (st_crs(r5) != crs) {
    r5 <- raster::projectRaster(r5, crs = crs$wkt, method = 'ngb')
  }
  if (st_crs(r6) != crs) {
    r6 <- raster::projectRaster(r6, crs = crs$wkt, method = 'ngb')
  }
  
		extract_pt(sites, r1, pt)
		extract_pt(sites, r2, pt)
		extract_pt(sites, r3, pt)
		extract_pt(sites, r4, pt)
		extract_pt(sites, r5, pt)
		extract_pt(sites, r6, pt)
		
  return(sites)
}
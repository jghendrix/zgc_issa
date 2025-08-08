## Extracting land cover of given point

extract_stoich <- function(DT, crs, stoich, pixel) {

  sites <- setDT(DT)
  
  pt <- c('x2_', 'y2_')
  
  crs(stoich) <- crs$wkt
  
  # stoich rasters are at a 5m resolution, can we average those out to like 30m?
  
  # first sub 0s for NAs
  r <- rast(stoich) %>% classify(cbind(NA, 0))
  # then aggregate 5m to 30m
  r2 <- aggregate(r, fact = pixel)
  
  r3 <- raster(r2)
  
 	extract_pt(sites, r3, pt)
	
			}

  
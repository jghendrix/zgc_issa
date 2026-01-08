## Extracting land cover of given point

extract_stoich <- function(DT, crs, stoich, var, pixel) {

  sites <- setDT(DT)
  
  pt <- c('x2_', 'y2_')
  
  crs(stoich) <- crs$wkt
  
  r <- rast(stoich) 
  # stoichiometry rasters are at a 5m resolution
  # 'pixel' argument is a factor by which to average calls - pixel = 6 transforms to 30m cells, the same scale as the land cover map

  # aggregate 5m to 30m
  r2 <- aggregate(r, fact = pixel)
  
  r3 <- raster(r2)

 	extract_pt(sites, r3, pt) %>%
 	  rename(!!var := pt_r3)
			}

  
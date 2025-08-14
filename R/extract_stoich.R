## Extracting land cover of given point

extract_stoich <- function(DT, crs, stoich, var, pixel) {

  sites <- setDT(DT)
  
  pt <- c('x2_', 'y2_')
  
  crs(stoich) <- crs$wkt
  
  r <- rast(stoich) 
  # stoich rasters are at a 5m resolution, can we average those out to like 30m?
  
  # first sub 0s for NAs, if it's the percentage map. we don't want to impute zeros for the C:N ratio b/c 0 is extremely HIGH quality forage
  
  
  if(var == "N_Cladonia" | var == "N_vasc") {
    r  %<>% classify(cbind(NA, 0))
  }
  else{}
  
  # then aggregate 5m to 30m
  r2 <- aggregate(r, fact = pixel)
  
  r3 <- raster(r2)

 	extract_pt(sites, r3, pt) %>%
 	  rename(!!var := pt_r3)
			}

  
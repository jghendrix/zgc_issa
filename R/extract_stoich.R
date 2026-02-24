## Extracting land cover of given point

extract_stoich <- function(DT, crs, stoich, var, pixel) {

  sites <- setDT(DT)
  
  pt <- c('x2_', 'y2_')
  
  crs(stoich) <- crs$wkt
  
  r <- rast(stoich) 
  # stoichiometry rasters are at a 5m resolution
  # 'pixel' argument is a factor by which to average calls - pixel = 6 transforms to 30m cells, the same scale as the land cover map

  # For either layer, we want to make NAs into 0s before averaging out into these larger squares
  if(var == "N_Cladonia"){
  r %<>% mutate(Cladonia_N = ifelse(is.na(Cladonia_N), 0, Cladonia_N))
  }
  
  else{
    r %<>% mutate(Vascular_N = ifelse(is.na(Vascular_N), 0, Vascular_N)) 
  }
  
  # aggregate 5m to 30m
  r2 <- aggregate(r, fact = pixel, na.rm = T)
  
  r3 <- raster(r2)

 	extract_pt(sites, r3, pt) %>%
 	  rename(!!var := pt_r3)
			}

  
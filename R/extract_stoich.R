## Extracting land cover of given point

extract_stoich <- function(DT, crs, stoich) {

  sites <- setDT(DT)
  
  pt <- c('x2_', 'y2_')
  
  crs(stoich) <- crs$wkt
  
   # stoich <- raster::projectRaster(stoich, crs = crs$wkt, method = 'ngb')
  
		extract_pt(sites, stoich, pt)
	#	sites[legend, lc_description := label, on = .(pt_lc = class)]

			}

  
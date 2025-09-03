## Extracting land cover of given point

extract_forest <- function(DT, crs, lc, pixel) {

  sites <- setDT(DT)
  
  pt <- c('x2_', 'y2_')
  
  crs(lc) <- crs$wkt
  
  # lc rasters are at a 30m resolution, can we average those out to 90m?
  
  # first sub 1s for the forests and 0 for everything else
  bin <- subs(lc, data.frame(id=c(1,2,3,4,5,6,7,8,9,10), v=c(0,1,1,0,1,0,0,0,0,0)))
  
  # then aggregate 5m to 100m
  r <- aggregate(bin, fact = pixel)
  
  writeRaster(r, 'output/100m_prop_forest.tif')
  
  extract_pt(sites, r, pt)
	
			}

  
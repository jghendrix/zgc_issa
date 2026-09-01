## Extracting land cover of given point

extract_prop_lc <- function(DT, crs, lc, variable, pixel) {
  
  sites <- setDT(DT)
  
  pt <- c('x2_', 'y2_')
  
  crs(lc) <- crs$wkt
  
  if(variable == "forest"){
    # first sub 1s for the forests and 0 for everything else
    bin <- subs(lc, data.frame(id=c(1,2,3,4,5,6,7,8,9,10), v=c(0,1,1,0,1,0,0,0,0,0)))
  }
  
  if(variable == "wetland") {
    bin <- subs(lc, data.frame(id=c(1,2,3,4,5,6,7,8,9,10), v=c(1,0,0,0,0,0,0,0,0,0))) 
  }
  
  
  if(variable == "barrens") {
    bin <- subs(lc, data.frame(id=c(1,2,3,4,5,6,7,8,9,10), v=c(0,0,0,0,0,1,0,1,0,0))) 
  }
  
  
  if(variable == "scrub") {
    bin <- subs(lc, data.frame(id=c(1,2,3,4,5,6,7,8,9,10), v=c(0,0,0,1,0,0,0,0,0,0))) 
  }
  # then aggregate
  r <- aggregate(bin, fact = pixel, na.rm = T)
  
  writeRaster(r, paste0('output/120m_prop_', variable, '.tif'), overwrite = TRUE)
  
  extract_pt(sites, r, pt) 
  
}


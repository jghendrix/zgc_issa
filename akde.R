# Calculating aKDE for winter caribou data
# This takes forever, we already did it and wrote to an .rds file, just read that into the workflow in subsequent analyses

akde <- function(DT, season) {
  
  #DT <- w_fixes
  DT %<>% dplyr::select(Longitude = x_, 
                        Latitude = y_, 
                        id = Animal_ID,
                        timestamp = t_) %>%
    arrange(timestamp, by = id)
  
  conflicts_prefer(ctmm::head)
  
  DTt <- as.telemetry(DT, projection = crs("epsg:32621")) 
  
  
  # check what it's projected as...
  ctmm::projection(DTt)
  # ok, good to go
  
  # calculate fit guess object
  #GUESS <- ctmm.guess(DTt, interactive = FALSE)
  # This won't work for a set of individuals.... ok
  # try the loop?
  
  FITS <- AKDES <- list()
  
  for(i in 1:length(DTt)) {
    
    GUESS <- ctmm.guess(DTt[[i]], interactive=FALSE)
    # use ctmm.select unless you are certain that the selected model is OUF
    FITS[[i]] <- ctmm.fit(DTt[[i]],GUESS)
    
  }
  
  AKDES <- akde(DTt, FITS)
  
  saveRDS(AKDES, paste0('output/kernels/', season, '_north_akdes.Rds'))
  
  # color to be spatially distinct
 # COL <- color(AKDES, by='individual')
  
  # plot AKDEs
#  ctmm::plot(AKDES,col.DF=COL,col.level=COL,col.grid=NA,level=NA)
  
  return(AKDES)

}
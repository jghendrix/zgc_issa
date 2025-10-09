# Calculating aKDE for winter caribou data?

akde <- function(DT) {
  
  #DT <- w_fixes
  DT %<>% dplyr::select(Longitude = x_, 
                        Latitude = y_, 
                        id = Animal_ID,
                        timestamp = t_)
  
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
    
    GUESS <- ctmm.guess(DTt[[i]],interactive=FALSE)
    # use ctmm.select unless you are certain that the selected model is OUF
    FITS[[i]] <- ctmm.fit(DTt[[i]],GUESS)
  }
  
  AKDES <- akde(DTt, FITS)
  
  # color to be spatially distinct
  COL <- color(AKDES, by='id')
  
  # plot AKDEs
  plot(AKDES,col.DF=COL,col.level=COL,col.grid=NA,level=NA)
  
  # cluster-analysis of buffalo
  cluster(AKDES,sort=TRUE)
  # Compute akde object
  UD <- akde(Cilla,FIT)
  
  # Plot data with AKDE contours
  plot(Cilla,UD=UD)  
}
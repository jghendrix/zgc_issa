# Calculating aKDE for winter caribou data
# This takes forever, we already did it and wrote to an .rds file, just read that into the workflow in subsequent analyses

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
    
    GUESS <- ctmm.guess(DTt[[i]], interactive=FALSE)
    # use ctmm.select unless you are certain that the selected model is OUF
    FITS[[i]] <- ctmm.fit(DTt[[i]],GUESS)
    
  }
  
  AKDES <- akde(DTt, FITS)
  
  saveRDS(AKDES, 'output/akdes.Rds')
  
  # color to be spatially distinct
 # COL <- color(AKDES, by='individual')
  
  # plot AKDEs
#  ctmm::plot(AKDES,col.DF=COL,col.level=COL,col.grid=NA,level=NA)
  
  return(AKDES)

}
AKDES <- readRDS('output/akdes.Rds')

pop.ud <- mean(AKDES)

writeRaster(pop.ud, 'output/population_UD.tif', format = "GTiff", DF = "PDF")

#ctmm::plot(AKDES[[1]], cont=50)
#adf <- data.frame(AKDES[[1]])

id3 <- AKDES[[3]]
id3$scaled <- id3$PDF/m

for(i in 1:44) {
#  AKDES[[i]]$scaled <- AKDES[[i]]$PDF/max(AKDES[[i]]$PDF)
  
 # writeRaster(AKDES[[i]],
#              filename = paste0('output/UDs/population_UD_', i, '.tif'), 
#              format = "GTiff", DF = "PDF")
}


#writeRaster(pop.ud, 'output/population_UD.tif', format = "GTiff", DF = "PDF")

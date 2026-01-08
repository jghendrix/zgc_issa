# Calculating aKDE for caribou data
akde <- function(DT, season) {
  
  DT %<>% dplyr::select(location_long = x_, 
                        location_lat = y_, 
                        id = Animal_ID,
                        timestamp = t_) %>%
    arrange(timestamp, by = id)
  
# long lat cannot be called X and Y, else it thinks they're UTM values in zone 1

    conflicts_prefer(ctmm::head)
  
  DTt <- as.telemetry(DT, projection = "epsg:32621")
  
  # check what it's projected as
  ctmm::projection(DTt)
  # ok, good to go
  
  
  FITS <- AKDES <- list()
  
  for(i in 1:length(DTt)) {
    
    GUESS <- ctmm.guess(DTt[[i]], interactive=FALSE)
    # use ctmm.select unless you are certain that the selected model is OUF
    FITS[[i]] <- ctmm.fit(DTt[[i]],GUESS)
    
  }

  
  AKDES <- akde(DTt, FITS)
  
  saveRDS(AKDES, paste0('output/kernels/', season, '_akdes.Rds'))
  
  return(AKDES)

}
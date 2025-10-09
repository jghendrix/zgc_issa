# Calculating aKDE for winter caribou data?

akde <- function(DT) {
  
  #DT <- winter_prep
  
  DT %<>% filter(case_ == TRUE) %>%
    dplyr::select(x = x1_, y = y1_, id, timestamp = t1_)
  
  #conflicts_prefer(ctmm::head)
  DTt <- as.telemetry(DT, UTM.zone="21") # keeps reading it as UTM zone 1... WHYYYYYY
  
  
  DT$UTM.zone <- "21 +north +datum=WGS84"
  
  
  wgs84 <- "+proj=utm +zone=21 +north +datum=WGS84 +units=m +no_defs" 
  DTt <- as.telemetry(DT, projection=CRS(wgs84))
  
  # calculate fit guess object
  GUESS <- ctmm.guess(DTt, interactive = FALSE)
  
  # in general, you should be running ctmm.select here instead of ctmm.fit
  FIT <- ctmm.fit(Cilla,GUESS)
  
  # Compute akde object
  UD <- akde(Cilla,FIT)
  
  # Plot data with AKDE contours
  plot(Cilla,UD=UD)  
}
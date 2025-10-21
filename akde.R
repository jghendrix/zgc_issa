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
  
  saveRDS(AKDES, 'output/summer_akdes.Rds')
  
  # color to be spatially distinct
 # COL <- color(AKDES, by='individual')
  
  # plot AKDEs
#  ctmm::plot(AKDES,col.DF=COL,col.level=COL,col.grid=NA,level=NA)
  
  return(AKDES)

}
AKDES <- readRDS('output/summer_akdes.Rds')

pop.ud <- mean(AKDES)

writeRaster(pop.ud, 'output/summer_population_UD.tif', format = "GTiff", DF = "PDF")



## Comparing summer and winter kernels 21 October -----
summer <- readRDS('output/kernels/summer_akdes.Rds')
winter <- readRDS('output/kernels/akdes.Rds')
sx <- mean(summer)
wx <- mean(winter)

#sdf <- as.data.frame(sx)

sk <- rast(raster(sx))
res(sk)
wk <- rast(raster(wx))
res(wk)

# they have different resolutions but also different extents
fogo_poly <- read_sf('input/FogoPoly/FogoPoly.shp')

scrop <- crop(sk, fogo_poly, mask = TRUE) %>%
  rename(summer = layer)
wcrop <- crop(wk, fogo_poly, mask = TRUE) %>%
  rename(winter = layer)

w_resampled <- resample(wcrop, scrop)

both <- c(scrop, w_resampled)
terra::plot(both) # ggtitle("Seasonal utilization distributions")


# Correlation between layers
cor(
  values(both)[, 1],
  values(both)[, 2],
  use = "na.or.complete"
)
# 8 % correlation between summer and winter density kernels
# Not quite a negative correlation... but visually they appear to be using quite different locations no

# Can we simplify this to the same northern half we did our models?
north <- both %>% filter(y > 5503000)
terra::plot(north)

cor(
  values(north)[, 1],
  values(north)[, 2],
  use = "na.or.complete"
)
# Looking specifically at this northern half, correlation is -21.6 %


# Can we bring in the stoich maps to this now?



#ggplot(comp, aes(x = summer, y = winter)) + geom_point(alpha = 0.1)
plot(both)

sk_z <- sk %>% rename(kernel = ) %>%
  mutate(kernel = kernel/max(kernel))






# Old explorations -----
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

# Calculating aKDE for winter caribou data
# This takes forever, we already did it and wrote to an .rds file, just read that into the workflow in subsequent analyses

akde <- function(DT, season) {
  
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
  
  saveRDS(AKDES, paste0('output/kernels/', season, '_akdes.Rds'))
  
  # color to be spatially distinct
 # COL <- color(AKDES, by='individual')
  
  # plot AKDEs
#  ctmm::plot(AKDES,col.DF=COL,col.level=COL,col.grid=NA,level=NA)
  
  return(AKDES)

}


# 7 November - Bhattacharya overlap of summer and winter population kernels------
AKDES <- readRDS('output/kernels/summer_akdes.Rds')
summer.ud <- mean(AKDES)
winter <- readRDS('output/kernels/akdes.Rds')
winter.ud <- mean(winter)

writeRaster(winter.ud, 'output/winter_population_UD.tif', format = "GTiff", DF = "PDF")
writeRaster(summer.ud, 'output/summer_population_UD.tif', format = "GTiff", DF = "PDF")

ctmm::overlap(seasons)
# it'll do between individuals within a season... how do I get it to do season vs. season for the population average?
# winter UD area 12.20485
# summer UD area 23.91385
seasons <- c(winter.ud, summer.ud)
ctmm::overlap(seasons)

# Inconsistent projections, apparently
ctmm::projection(winter.ud) <- crs_sp # doesn't work
# there's no way to reproject within ctmm according to the package author. Need to have the rasters aligned first, then "assign their arrays back to the UD object and update the dr slot."

## Comparing summer and winter kernels 21 October -----

w_resampled <- resample(wcrop, scrop)
w_resampled %<>% filter(y > 5503000)
scrop %<>% filter(y > 5503000)
sum_rast <- scrop
win_rast <- w_resampled

## UDOI to better quantify overlap of summer and winter ranges... ----

amt::hr_overlap(w_resampled, scrop, type = "udoi")
# can't be a SpatRaster... needs to be an hr object

# is it easiest to just recalculate home ranges in amt?

eroy <- amt_fisher |> filter(name == "Leroy")

tar_load(locs_prep)
winter <- locs_prep %>% filter(doy < 61) %>%
  make_track(x_, y_, key = Animal_ID)
summer <- locs_prep %>% filter(doy >= 167 & doy <= 228) %>%
  make_track(x_, y_, key = Animal_ID)

wsea <- winter |> filter(key == "FO2017007")

wrast <- make_trast(winter, res = 25)
srast <- make_trast(summer, res = 25)
hr_w <- hr_kde(winter, trast = wrast, levels = c(0.5, 0.9))


wrast <- raster(w_resampled)
srast <- raster(scrop)
amt::hr_overlap(wrast, srast, type = "udoi")


### Ok, let's try this again starting from the rasters ------
summer <- readRDS('output/kernels/summer_akdes.Rds')
winter <- readRDS('output/kernels/akdes.Rds')
sx <- mean(summer)
wx <- mean(winter)

#sdf <- as.data.frame(sx)
sk <- rast(raster(sx))
#res(sk)
wk <- rast(raster(wx))
#res(wk)

# they have different resolutions but also different extents
fogo_poly <- read_sf('input/FogoPoly/FogoPoly.shp')

scrop <- crop(sk, fogo_poly, mask = TRUE) %>%
  rename(summer = layer) %>%
  filter(y > 5503000)
wcrop <- crop(wk, fogo_poly, mask = TRUE) %>%
  rename(winter = layer) %>%
  filter(y > 5503000)



## Nope, that's not working
library(raster)
library(SDMTools)
library(adehabitat)
# starting with .tif population rasters for summer and winter
sum_rast <- raster(scrop)
win_rast <- raster(w_resampled)

sumdf <- as(sum_rast, "SpatialPixelsDataFrame")
windf <- as(win_rast, "SpatialPixelsDataFrame")


fullgrid(sumdf) <- FALSE
fullgrid(windf) <- FALSE

hli <- list(h = 1, meth = "specified")

sumdf <- new("estUD", sumdf)
windf <- new("estUD", windf)

slot(sumdf, "h") <- hli
slot(sumdf, "vol") <- FALSE

slot(windf, "h") <- hli
slot(windf, "vol") <- FALSE

#Combine estUDs and convert to estUDm
UD.combined <- list(summer = sumdf, winter = windf)
class(UD.combined) <- "estUDm"

kerneloverlaphr(UD.combined, method = c("UDOI"), na.rm = T)
# giving an error message because there are NAs in the dataframe


# I can't use the amt code to calculate UDOI because I have NAs in my dataframe
# there are NAs because it's the fucking ocean and all the ponds
# how do i clip the raster to only the island itself??

# can I just do this manually somehow??











## Lazy version: just want the correlation coefficient from the Spat Rasters?? ----


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

tar_load(c(lichen, vascular))
lich <- rast(lichen)
vasc <- rast(vascular)
lich_res <- resample(lich, north)
vasc_res <- resample(vasc, north)
trio <- c(north, lich_res)
quad <- c(trio, vasc_res)

nquad <- quad %>% filter(y > 5503000)
# summer, winter, lichen, vascular kernel

cor(
  values(nquad)[, 3],
  values(nquad)[, 4],
  use = "na.or.complete"
)
# 12% correlation between winter kernel and Cladonia N
# 11% between winter and vascular N
# - 10% between Cladonia and Vascular N

#ggplot(comp, aes(x = summer, y = winter)) + geom_point(alpha = 0.1)
terra::plot(nquad)

#sk_z <- sk %>% rename(kernel = ) %>%
#  mutate(kernel = kernel/max(kernel))






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

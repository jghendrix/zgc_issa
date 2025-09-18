## Exported google earth images of phenological timing on Fogo, one image per year, with three bands? I think?
# https://doi.org/10.5067/VIIRS/VNP22Q2.001
# Moved them from Google Drive into the GEE/ folder here
library(raster)
library(terra)
library(tidyverse)
library(sf)
library(viridis)

epsg <- 32621
crs <- st_crs(epsg)
crs_sp <- CRS(crs$wkt)

fogo <- read_sf('input/FogoPoly/FogoPoly.shp')

first <- raster("GEE/2013_01_01.tif")
ft <- rast(first)
crs(ft) <- crs$wkt
ft_poly <- crop(ft, fogo, mask = TRUE, snap = "in")
plot(ft_poly)

fdf <- as.data.frame(ft) %>% filter(Date_Mid_Greenup_Phase_1 > 0) %>%
  mutate(doy = Date_Mid_Greenup_Phase_1 - 366*13,
         date = parse_date_time(paste(2013, doy), orders = "yj"))

mean(fdf$date)

# mean peak-greenup date is June 10th... earliest is April 23rd.
# I'd rather have the beginning of greenup... or like, maybe we use the 25th percentile? IDK

n <- c(2013:2022)
peak <- data.frame()

for(i in n){
  r <- raster(paste0("GEE/", i, "_01_01.tif"))
  tr <- rast(r)
  
  
  rdf <- as.data.frame(tr) %>% filter(Date_Mid_Greenup_Phase_1 > 0) %>%
    mutate(doy = Date_Mid_Greenup_Phase_1 - 366*(i - 2000),
           date = parse_date_time(paste(i, doy), orders = "yj"),
           year = i)
  
  peak <- rbind(peak, rdf)
}

# I can't believe that worked first try!!!

ggplot(all, aes(x = doy, fill = as.factor(year))) +
  geom_density(alpha = 0.1) +
  scale_colour_viridis(discrete = TRUE) +
  theme_bw()

mean(all$doy)

# overall mean is June 16th
starts <- all %>% group_by(year) %>% summarise(ten = quantile(doy, 0.1),
                                     quart = quantile(doy, 0.25))
mean(starts$ten) # 148 = May 27th
mean(starts$quart) # 157 = June 6th


## What about initiation of greenup?
n <- c(2013:2022)
start <- data.frame()

for(i in n){
  r <- raster(paste0("GEE/start_", i, "_01_01.tif"))
  tr <- rast(r)
  
  
  rdf <- as.data.frame(tr) %>% 
    # filter(EVI2_Onset_Greenness_Increase_1 > 0) %>%
    mutate(#doy =  EVI2_Onset_Greenness_Increase_1 - 366*(i - 2000),
           #date = parse_date_time(paste(i, doy), orders = "yj"),
           year = i) %>%
    rename(init = EVI2_Onset_Greenness_Increase_1)
  
  start <- rbind(start, rdf)
}
hist(start$init)

# This data is useless.... it's nothing but zeros.

## What about senescence? ----

## What about end of the growing season?
n <- c(2013:2022)
end <- data.frame()

for(i in n){
  r <- raster(paste0("GEE/senesce/", i, "_01_01.tif"))
  tr <- rast(r)
  
  
  rdf <- as.data.frame(tr) %>% 
     filter(Date_Mid_Senescence_Phase_1 > 0) %>%
    mutate(doy = Date_Mid_Senescence_Phase_1 - 366*(i - 2000),
      date = parse_date_time(paste(i, doy), orders = "yj"),
      year = i) %>%
    rename(sen = Date_Mid_Senescence_Phase_1)
  
  end <- rbind(end, rdf)
}

ggplot(end, aes(x = doy, fill = as.factor(year))) +
  geom_density(alpha = 0.1) +
  scale_colour_viridis(discrete = TRUE) +
  theme_bw()

mean(end$doy) # 274, Oct 1st
median(end$doy) # 274, Oct 1st


ends <- end %>% group_by(year) %>% summarise(ten = quantile(doy, 0.9),
                                               quart = quantile(doy, 0.75))
mean(ends$ten) # 291 = Oct 18th
mean(ends$quart) # 283 = Oct 10th

# What does this all tell us?? ----
# Ok, so "spring", aka peak greenup, is around June 16th on average but could start as early as May 27th (10th %ile) or June 6th (25th %ile) 
# the middle of the senescence phase is Oct 1st, but could go as late as Oct 10th (75th %ile) or 18th (90th %ile)

# Winter?
## the ice data from the antler github repo is giving me trouble
# BUT Allegra's figure from that ms has % snow cover by date across years
# Jan 1st - Mar 1st always has > 75% snow cover... feels legit to me?

# for parallelism of 60 days, could do June 16th - August 16th?
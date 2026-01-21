## Exported google earth images of phenological timing on Fogo, one image per year, with three bands
# https://doi.org/10.5067/VIIRS/VNP22Q2.001
# downloaded from Google Earth Engine into the GEE/ folder here
library(raster)
library(terra)
library(tidyverse)
library(sf)
library(viridis)

# set spatial parameters and import data
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

# mean peak-greenup date is June 10th, but the earliest is April 23rd.
# to define "start" of summer, we'd want the beginning of greenup and not the max derivative, yes? Maybe use the 25th percentile of this range?

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

ggplot(all, aes(x = doy, fill = as.factor(year))) +
  geom_density(alpha = 0.1) +
  scale_colour_viridis(discrete = TRUE) +
  theme_bw()

mean(all$doy)

# overall mean from all cells is June 16th

starts <- all %>% group_by(year) %>% summarise(ten = quantile(doy, 0.1),
                                     quart = quantile(doy, 0.25))
mean(starts$ten) # 148 = May 27th
mean(starts$quart) # 157 = June 6th

## What about senescence? ----

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
median(end$doy) # same date


ends <- end %>% group_by(year) %>% summarise(ten = quantile(doy, 0.9),
                                               quart = quantile(doy, 0.75))
mean(ends$ten) # 291 = Oct 18th
mean(ends$quart) # 283 = Oct 10th

# What does this all tell us?

# So "spring", aka peak greenup, is around June 16th on average but could start as early as May 27th (10th %ile), June 6th (25th %ile) 
# the middle of the senescence phase is Oct 1st, but could go as late as Oct 10th (75th %ile) or 18th (90th %ile)
# These dates feel far too late for the end of "summer"

# Winter?
## Using snow cover data presented in doir:10.1002/ecs2.70484
# From the years shown, Jan 1st - Mar 1st always have > 75% snow cover
# two months is a reasonable (if conservative) period to look at winter - in some years, December or April could be relatively mild, so focus on mid-winter seems most prudent

# Using June 16th as start of 'summer', parallel two month period would be to Aug 16th
# again, might miss the tail end of summer, but this would be the core period of peak forage availability and growth
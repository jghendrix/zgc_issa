# Calculating aKDE for winter caribou data
# This takes forever, we already did it and wrote to an .rds file, just read that into the workflow in subsequent analyses

scale_kernel <- function(DT) {
  
pUD <- raster('output/population_UD.tif')

# These PDF values aren't scaled yet though... they max out at like, .00001, mostly 10e-8 
# convert to terra so we can manipulate the values
tUD <- rast(pUD)

tUD %<>% rename(kernel = PDF) %>%
  mutate(kernel = kernel/max(kernel))

return(tUD)

# Ideally we would scale before joining all them together but I've yet to figure that out
# Write all them to rasters, then bring back in and scale them?
# for(i in 1:44) {
#   writeRaster(AKDES[[i]],
#               filename = paste0('output/UDs/population_UD_', i, '.tif'), 
#               format = "GTiff", DF = "PDF")
# }

# f <- list.files('output/UDs')
# ras <- lapply(paste0('output/UDs/', f) ,raster)

}
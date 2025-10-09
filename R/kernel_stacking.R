## Intensity of use of caribou, as a raster


kernel_stacking <- function(DT, crs) {

  # We have each year as a separate raster... and it's just raw counts, ideally we could turn that into a proportion
    
  k17 <- raster('output/kernel_2017.tif')
  t17 <- rast(k17)
  p17 <- app(t17, fun = function(x) x/max(x))
  
  k18 <- raster('output/kernel_2018.tif')
  t18 <- rast(k18)
  p18 <- app(t18, fun = function(x) x/max(x))
  
  k19 <- raster('output/kernel_2019.tif')
  t19 <- rast(k19)
  p19 <- app(t19, fun = function(x) x/max(x))
  
  k20 <- raster('output/kernel_2020.tif')
  t20 <- rast(k20)
  p20 <- app(t20, fun = function(x) x/max(x))
  
  k23 <- raster('output/kernel_2023.tif')
  t23 <- rast(k23)
  p23 <- app(t23, fun = function(x) x/max(x))
  
  k24 <- raster('output/kernel_2024.tif')
  t24 <- rast(k24)
  p24 <- app(t24, fun = function(x) x/max(x))
  
  k25 <- raster('output/kernel_2025.tif')
  t25 <- rast(k25)
  p25 <- app(t25, fun = function(x) x/max(x))
  
  # Standardized all kernels to the highest value within each year, so they all range between 0 and 1. At this point, if we wanted, we could account for the # of collars & total pop'n size in each year... though the "relative to highest within year" sort of accounts for # collars, eh?
  
  stack <- c(p17, p18, p19, p20, p23, p24, p25)
  
  # Now we want a single layer averaging over all these layers
  
  tot <- app(stack, fun = mean)
  
  crs(tot) <- crs$wkt
  
  terra::writeRaster(tot,"output/all_years_kernel.tif", 
                     filetype = "GTiff",
                     overwrite = TRUE)

  # Finally! Prepare the Cladonia data
  
  DT %<>% filter(species == "Cladonia")
  locs <- setDT(DT)
  pt <- c('x', 'y')
  
  if (st_crs(tot) != crs) {
    tot <- raster::projectRaster(tot, crs = crs$wkt, method = 'ngb')
  }
  
  extract_pt(locs, tot, pt)
  return(locs)
  
  
}
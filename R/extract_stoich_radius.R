#' @title Extract landcover
extract_stoich_radius <- function(DT, crs, stoich, radius) {

### For testing purposes, b/c it's taking forever to run on all 4.5million steps
 # DT %<>% dplyr::filter(doy == 120 & yr == 2023)
  # Ok, it does work, it is just reeeeeeeeeally slow
  
  
    setDT(DT)
  # wanted to draw a 100m buffer around locations and calculate average %N of lichen within that buffer
  # downgraded to 30m though just for computational demands
  
  # Do we call NAs zeros here? If only one 5x5m pixel has lichen and the rest are all NAs, is it fair to call that entire zone that value? Or do we average it with a bunch of zeros?
  # I think the latter tbh

  DT_sf <- st_as_sf(DT,
                    coords = c("x2_", "y2_"),
                    crs = crs)

  #DT_sf_reproj <- st_transform(DT_sf, crs = st_crs(lc))

  DT_sf_buffer <- st_buffer(DT_sf, dist = units::as_units(radius, 'm'))
  
  crs(stoich) <- crs$wkt
  
  buffer_stoich <- raster::extract(stoich, DT_sf_buffer)

  lichen_N <- rapply(buffer_stoich, 
                     function(stoich) ifelse(is.na(stoich), 0, stoich),
                     how = "replace")

means <- sapply(lichen_N, FUN = mean)

DTh <- cbind(DT, means) %>%
  rename(lichen_N = means)

return(DTh)

}

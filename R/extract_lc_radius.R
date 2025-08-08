#' @title Extract landcover
extract_lc_radius <- function(DT, crs, lc, legend, radius) {

  #DT %<>% dplyr::filter(doy > 305 | doy < 90)
  
  setDT(DT)
  # want to draw a 100m buffer around locations and extract % forest within that buffer

#	start <- c('x1_', 'y1_')
#	end <- c('x2_', 'y2_')

  DT_sf <- st_as_sf(DT,
                    coords = c("x2_", "y2_"),
                    crs = crs)

  DT_sf_reproj <- st_transform(DT_sf, crs = st_crs(lc))

  DT_sf_buffer <- st_buffer(DT_sf_reproj, dist = units::as_units(radius, 'm'))

  buffer_lc <- raster::extract(lc, DT_sf_buffer)

  prop_forest <- rapply(buffer_lc, function(lc) ifelse(lc %in% c("2", "3", "5"), 1, 0), how = "replace")

means <- sapply(prop_forest, FUN = mean)

DTh <- cbind(DT, means) %>%
  rename(prop_forest = means)

return(DTh)

}

get_coastline <- function(bb, out_CRS) {
  
  # Download osm coastlines in bbox
  coast <- opq(bb) %>%
    add_osm_feature(key = 'natural', value = 'coastline') %>%
    osmdata_sf()
  
  # Grab polygons (small islands)
  polys <- coast$osm_polygons
  
  # Grab lines (large islands including Fogo)
  lns <- coast$osm_lines
  
  # Union -> polygonize -> cast lines = geo set
  castpolys <- st_cast(st_polygonize(st_union(lns)))
  
  # Combine geometries and cast as sf
  islands <- st_as_sf(c(st_geometry(polys), castpolys))
  
  # Project to given out_UTM
  utmislands <- st_transform(islands, out_CRS)
 
  ### Output
  # write_sf(utmislands, 'output/island-polygons.gpkg')
  ## ^ Alec says this is bad, side quests of saving objects that don't remain in the targets workflow is poor form
  
  return(utmislands)
}

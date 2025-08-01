get_roads <- function(bb, out_CRS) {
	## Roads
	routes <- opq(bb) %>%
		add_osm_feature(key = 'highway') %>%
		osmdata_sf()
	
	# Grab roads
	roads <- routes$osm_lines
	
	utmroads <- st_transform(roads, out_CRS)

	return(utmroads)

}



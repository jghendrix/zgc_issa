extract_tod <- function(DT, solar_dep = 6, include_crepuscule = TRUE, coords) {
	# needs st_transform 4326
	pts <- DT[, sunriset(as.matrix(cbind(.SD)), t1_), .SDcols = coords]
}


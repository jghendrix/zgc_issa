#' @title Extract distance to a layer using the distanceto package
#' @author Alec L. Robitaille
extract_distance_to <- function(DT, layer, coords, crs) {
	object_name <- deparse(substitute(layer))

	dists <- distance_to(
		st_as_sf(DT, coords = coords, crs = crs),
		layer
	)

	DT[, (paste0('dist_to_', object_name)) := dists]
}

#' @title Extract layers at point
#' @author Alec L. Robitaille
extract_pt <- function(DT, layer, coords = FALSE) {
	object_name <- deparse(substitute(layer))

#	coords <- c("x", "y")
	
	DT[, (paste0('pt_', object_name)) := extract(layer, cbind(.SD)),
		 .SDcols = c(coords)]
}

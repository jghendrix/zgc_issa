#' @title Extract layers
#' @export
#' @author Alec L. Robitaille, Julie W. Turner
extract_layers <- function(DT, crs, lc, legend) {

	setDT(DT)

	start <- c('x1_', 'y1_')
	end <- c('x2_', 'y2_')

	if (st_crs(lc) != crs) {
		lc <- raster::projectRaster(lc, crs = crs$wkt, method = 'ngb')
	}

	extract_pt(DT, lc, end)
	DT[legend, lc_description := label, on = .(pt_lc = class)]


}

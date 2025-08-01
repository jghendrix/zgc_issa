#' @title Resample tracks
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
resample_tracks <- function(tracks, rate, tolerance) {
	t <- track_resample(tracks, rate = rate, tolerance = tolerance) %>%
		filter_min_n_burst()

	# Cancel if there are not at least three rows after resample
	if (nrow(t) < 3) return()

	t %>% steps_by_burst(., keep_cols = 'start')
}

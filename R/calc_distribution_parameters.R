#' @title Calculate distribution parameters
#' @export
#' @author Alec L. Robitaille, Julie W. Turner
calc_distribution_parameters <- function(steps) {
	# if (is.null(steps)) return()
	data.frame(
		id = steps$Animal_ID[[1]],
		tar_group = steps$tar_group[[1]],
		ta_distr_params(steps),
		sl_distr_params(steps)
	)
}

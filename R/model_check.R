#' @title Check glmmTMB model
#' @export
#' @author Alec L. Robitaille, Julie W. Turner
model_check <- function(model) {

	list(
		# TODO: std error ~10x larger than estimate
		n_na_std_error = sum(is.na(model$sdr$par.fixed)),
		pd_hess = model$sdr$pdHess
	)

}

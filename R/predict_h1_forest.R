#' @title Predict H1 forest
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
predict_h1_forest <- function(DT, model, predictor) {
	N <- 100L

if(predictor == "fire") {
	new_data <- DT[, .(
		sl_ = mean(sl_),
		prop_forest = seq(from = 0, to = 1, length.out = N),
		dist_to_new_burn = median(dist_to_new_burn, na.rm = T),
		dist_to_old_burn = median(dist_to_old_burn, na.rm = T),
		indiv_step_id = NA
	), by = id]
}

else {
	new_data <- DT[, .(
		sl_ = mean(sl_),
		prop_forest = seq(from = 0, to = 1, length.out = N),
		dist_to_tch = median(dist_to_tch, na.rm = T),
		dist_to_minor = median(dist_to_minor, na.rm = T),
		indiv_step_id = NA
	), by = id]
}


	new_data[, h1_forest := predict(model, .SD, type = 'link', re.form = NULL)]

	new_data[, x := seq(from = 0, to = 1, length.out = N), by = id]
}

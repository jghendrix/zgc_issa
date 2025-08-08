#' @title Predict H2
#' @export
#' @author Jack G Hendrix
predict_h2 <- function(DT, model, predictor) {

	if(predictor == "fire")
	new_data <- DT[, .(
		sl_ = mean(sl_),
		prop_forest = mean(prop_forest, na.rm = T),
		dist_to_new_burn = median(dist_to_new_burn, na.rm = T),
		dist_to_old_burn = median(dist_to_old_burn, na.rm = T),
		indiv_step_id = NA),
		by = id]

	if(predictor == "road")
		new_data <-	DT[, .(
			sl_ = mean(sl_),
			prop_forest = mean(prop_forest, na.rm = T),
			dist_to_tch = median(dist_to_tch, na.rm = T),
			dist_to_minor = median(dist_to_minor, na.rm = T),
			indiv_step_id = NA),
			by = id]

	if(predictor == "fire alone")
		new_data <-	DT[, .(
			sl_ = mean(sl_),
			prop_forest = mean(prop_forest, na.rm = T),
			in_group = "alone",
			dist_to_new_burn = median(dist_to_new_burn, na.rm = T),
			dist_to_old_burn = median(dist_to_old_burn, na.rm = T),
			indiv_step_id = NA),
			by = id]

	if(predictor == "fire dyad")
		new_data <-	DT[, .(
			sl_ = mean(sl_),
			prop_forest = mean(prop_forest, na.rm = T),
			in_group = "dyad",
			dist_to_new_burn = median(dist_to_new_burn, na.rm = T),
			dist_to_old_burn = median(dist_to_old_burn, na.rm = T),
			indiv_step_id = NA),
			by = id]

	if(predictor == "road alone")
		new_data <- DT[, .(
			sl_ = mean(sl_),
			prop_forest = mean(prop_forest, na.rm = T),
			in_group = "alone",
			dist_to_tch = median(dist_to_tch, na.rm = T),
			dist_to_minor = median(dist_to_minor, na.rm = T),
			indiv_step_id = NA
		), by = id]

	if(predictor == "road dyad")
		new_data <- DT[, .(
			sl_ = mean(sl_),
			prop_forest = mean(prop_forest, na.rm = T),
			in_group = "dyad",
			dist_to_tch = median(dist_to_tch, na.rm = T),
			dist_to_minor = median(dist_to_minor, na.rm = T),
			indiv_step_id = NA
		), by = id]


	new_data[, h2 := predict(model, .SD, type = 'link', re.form = NULL)]
}

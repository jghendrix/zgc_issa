#' @title Predict H1 forest
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
predict_h1_forest_social <- function(DT, model, predictor, sociality) {

	DT %<>% filter(season == "winter",
								 !is.na(in_group))
	N <- 100L

	# manually inserting coefficient values, seeing as it's giving negative RSS from forest??
	#	new_data[, h1_forest := predict(model, .SD, type = 'link', re.form = NULL)]

if(predictor == "fire") {

	if(sociality == "alone"){
		new_data <- DT[, .(
			sl_ = mean(sl_),
			prop_forest = seq(from = 0, to = 1, length.out = N),
			in_group = "alone",
			dist_to_new_burn = median(dist_to_new_burn, na.rm = T),
			dist_to_old_burn = median(dist_to_old_burn, na.rm = T),
			indiv_step_id = NA
		), by = id]

		sl <- 0.316796
		f <- .763456
		new <- -0.065618
		old <- 0.023413
		falone <- 0.719033
		fsl <- -0.278828
		newsl <- 0.006571
		oldsl <- -0.025328

		new_data[, h1_forest :=
						 	log(sl_)*sl +
						 	prop_forest*f +
						 	log(dist_to_new_burn + 1)*new +
						 	log(dist_to_old_burn + 1)*old +
						 	prop_forest*falone +
						 	log(sl_)*prop_forest*fsl +
						 	log(sl_)*log(dist_to_new_burn + 1)*newsl +
						 	log(sl_)*log(dist_to_old_burn + 1)*oldsl
		]

		new_data[, x := seq(from = 0, to = 1, length.out = N), by = id]

	}

	else {
		new_data <- DT[, .(
			sl_ = mean(sl_),
			prop_forest = seq(from = 0, to = 1, length.out = N),
			in_group = "dyad",
			dist_to_new_burn = median(dist_to_new_burn, na.rm = T),
			dist_to_old_burn = median(dist_to_old_burn, na.rm = T),
			indiv_step_id = NA
		), by = id]

	sl <- 0.316796
	f <- .763456
	new <- -0.065618
	old <- 0.023413
	newdyad <- -0.120320
	olddyad <- -0.016986
	fsl <- -0.278828
	newsl <- 0.006571
	oldsl <- -0.025328

	new_data[, h1_forest :=
					 	log(sl_)*sl +
					 	prop_forest*f +
					 	log(dist_to_new_burn + 1)*new +
					 	log(dist_to_old_burn + 1)*old +
					 	log(dist_to_new_burn + 1)*newdyad +
					 	log(dist_to_old_burn + 1)*olddyad +
					 	log(sl_)*prop_forest*fsl +
					 	log(sl_)*log(dist_to_new_burn + 1)*newsl +
					 	log(sl_)*log(dist_to_old_burn + 1)*oldsl
	]
	new_data[, x := seq(from = 0, to = 1, length.out = N), by = id]
	}
}

	# and if not fire, then it's road
	else {
		if(sociality == "alone"){
			new_data <- DT[, .(
				sl_ = mean(sl_),
				prop_forest = seq(from = 0, to = 1, length.out = N),
				in_group = "alone",
				dist_to_tch = median(dist_to_tch, na.rm = T),
				dist_to_minor = median(dist_to_minor, na.rm = T),
				indiv_step_id = NA
			), by = id]

			sl<- 0.44515
			f <- 0.87392
			tch <- 2.10887
			m <- 0.19497
			falone <- 0.68487
			fsl <- -0.29053
			tchsl <- -0.06906
			msl <- 0.04187

			new_data[, h1_forest :=
							 	log(sl_)*sl +
							 	prop_forest*f +
							 	log(dist_to_tch + 1)*tch +
							 	log(dist_to_minor + 1)*m +
							 	prop_forest*falone +
							 	log(sl_)*prop_forest*fsl +
							 	log(sl_)*log(dist_to_tch + 1)*tchsl +
							 	log(sl_)*log(dist_to_minor + 1)*msl
			]
			new_data[, x := seq(from = 0, to = 1, length.out = N), by = id]
		}

		else {
			new_data <- DT[, .(
				sl_ = mean(sl_),
				prop_forest = seq(from = 0, to = 1, length.out = N),
				in_group = "dyad",
				dist_to_tch = median(dist_to_tch, na.rm = T),
				dist_to_minor = median(dist_to_minor, na.rm = T),
				indiv_step_id = NA
			), by = id]

			sl <- 0.44515
			f <- 0.87392
			tch <- 2.10887
			m <- 0.19497
			tchdyad <- -0.47487
			mdyad <- 0.3187
			fsl <- -0.29053
			tchsl <- -0.06906
			msl <- 0.04187

			new_data[, h1_forest :=
							 	log(sl_)*sl +
							 	prop_forest*f +
							 	log(dist_to_tch + 1)*tch +
							 	log(dist_to_minor + 1)*m +
							 	log(dist_to_tch + 1)*tchdyad +
							 	log(dist_to_minor + 1)*mdyad +
							 	log(sl_)*prop_forest*fsl +
							 	log(sl_)*log(dist_to_tch + 1)*tchsl +
							 	log(sl_)*log(dist_to_minor + 1)*msl
			]
			new_data[, x := seq(from = 0, to = 1, length.out = N), by = id]
			}
}


}

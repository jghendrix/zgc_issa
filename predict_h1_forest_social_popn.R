#' @title Predict H1 open
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
predict_h1_open_social_p <- function(DT, popn, predictor, sociality) {

	DT %<>% filter(season == "winter",
								 !is.na(in_group))
	N <- 100L

	open <- seq(from = 0, to = 1, length.out = N)
	popn %<>% slice(rep(1:n(), each = 100))

	if(predictor == "fire") {

		if(sociality == "alone") {

			new_data <- as.data.frame(open) %>%
				mutate(sl = log(mean(DT$sl_)),
							 dist_new = log(median(DT$dist_to_new_burn, na.rm = T) + 1),
							 dist_old = log(median(DT$dist_to_old_burn, na.rm = T) + 1))

			new <- cbind(new_data, popn)
			setDT(new)

			new[, h1_open_mean :=
						sl*sl_B +
						open*open_B +
						dist_new*dist_new_B +
						dist_old*dist_old_B +
						open*openXalone_mean +
						sl*open*slXopen +
						sl*dist_new*slXnew +
						sl*dist_old*slXold
			]
			new[, h1_open_min :=
							 	sl*sl_B +
							 	open*open_B +
							 	dist_new*dist_new_B +
							 	dist_old*dist_old_B +
							 	open*openXalone_min +
							 	sl*open*slXopen +
							 	sl*dist_new*slXnew +
							 	sl*dist_old*slXold
			]

			new[, h1_open_max :=
							sl*sl_B +
							open*open_B +
							dist_new*dist_new_B +
							dist_old*dist_old_B +
							open*openXalone_max +
							sl*open*slXopen +
							sl*dist_new*slXnew +
							sl*dist_old*slXold
			]

			new[, x := seq(from = 0, to = 1, length.out = N)]

		}

		else {
			new_data <- as.data.frame(open) %>%
				mutate(sl = log(mean(DT$sl_)),
							 dist_new = log(median(DT$dist_to_new_burn, na.rm = T) + 1),
							 dist_old = log(median(DT$dist_to_old_burn, na.rm = T) + 1))

			new <- cbind(new_data, popn)
			setDT(new)



			new[, h1_open_mean :=
						sl*sl_B +
						open*open_B +
						dist_new*dist_new_B +
						dist_old*dist_old_B +
						dist_new*newXdyad_mean +
						dist_old*oldXdyad_mean +
						sl*open*slXopen +
						sl*dist_new*slXnew +
						sl*dist_old*slXold
			]
			new[, h1_open_min :=
						h1_open_mean -
						open*open_B +
						open*open_B_min]

			new[, h1_open_max :=
						h1_open_mean -
						open*open_B +
						open*open_B_max]

			new[, x := seq(from = 0, to = 1, length.out = N)]

		}
	}

	# and if not fire, then it's road
	else {

		if(sociality == "alone") {

			new_data <- as.data.frame(open) %>%
				mutate(sl = log(mean(DT$sl_)),
							 dist_tch = log(median(DT$dist_to_tch, na.rm = T) + 1),
							 dist_minor = log(median(DT$dist_to_minor, na.rm = T) + 1))

			new <- cbind(new_data, popn)
			setDT(new)

			new[, h1_open_mean :=
						sl*sl_B +
						open*open_B +
						dist_tch*dist_tch_B +
						dist_tch*dist_tch_B +
						open*openXalone_mean +
						sl*open*slXopen +
						sl*dist_new*slXnew +
						sl*dist_old*slXold
			]
			new[, h1_open_min :=
						sl*sl_B +
						open*open_B +
						dist_tch*dist_tch_B +
						dist_tch*dist_tch_B +
						open*openXalone_min +
						sl*open*slXopen +
						sl*dist_new*slXnew +
						sl*dist_old*slXold
			]

			new[, h1_open_max :=
						sl*sl_B +
						open*open_B +
						dist_tch*dist_tch_B +
						dist_tch*dist_tch_B +
						open*openXalone_max +
						sl*open*slXopen +
						sl*dist_new*slXnew +
						sl*dist_old*slXold
			]

			new[, x := seq(from = 0, to = 1, length.out = N)]

		}

		else {
			new_data <- as.data.frame(open) %>%
				mutate(sl = log(mean(DT$sl_)),
							 dist_tch = log(median(DT$dist_to_minor, na.rm = T) + 1),
							 dist_minor = log(median(DT$dist_to_minor, na.rm = T) + 1))

			new <- cbind(new_data, popn)
			setDT(new)

			new[, h1_open_mean :=
						sl*sl_B +
						open*open_B +
						dist_tch*dist_tch_B +
						dist_minor*dist_minor_B +
						dist_tch*tchXdyad_mean +
						dist_minor*minorXdyad_mean +
						sl*open*slXopen +
						sl*dist_tch*slXtch +
						sl*dist_minor*slXminor
			]

			new[, h1_open_min :=
						h1_open_mean -
						open*open_B +
						open*open_B_min
					]

			new[, h1_open_max :=
						h1_open_mean -
						open*open_B +
						open*open_B_max]

			new[, x := seq(from = 0, to = 1, length.out = N)]

		}
	}


}

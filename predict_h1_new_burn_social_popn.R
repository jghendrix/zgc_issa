#' @title Predict H1 distance to new burn at population level for social interaction
#' @export
#' @author Jack G Hendrix
predict_h1_new_burn_social_p <- function(DT, popn, social) {

	N <- 100L
	DT %<>% filter(season == "winter",
								 !is.na(in_group))
	popn %<>% slice(rep(1:n(), each = 100))


	distance_new <- seq(from = 0, to = 20000, length.out = N)
	new_data <- as.data.frame(distance_new) %>%
		mutate(dist_new = log(distance_new + 1),
					 sl = log(mean(DT$sl_)),
					 forest = mean(DT$prop_forest, na.rm = T),
					 dist_old = log(median(DT$dist_to_old_burn, na.rm = T) + 1))

	new <- cbind(new_data, popn)
	setDT(new)

	if(social == "alone") {

		new[, h1_new_mean :=
					sl*sl_B +
					forest*forest_B +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					forest*forestXalone_mean +
					sl*forest*slXforest +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]

		new[, h1_new_min :=
					h1_new_mean -
					dist_new*dist_new_B +
					dist_new*new_B_min]

		new[, h1_new_max :=
					h1_new_mean -
					dist_new*dist_new_B +
					dist_new*new_B_max]

		new[, x := seq(from = 0, to = 20, length.out = N)]

	}

	else {
		new[, h1_new_min :=
					sl*sl_B +
					forest*forest_B +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					dist_new*newXdyad_min +
					dist_old*oldXdyad_mean +
					sl*forest*slXforest +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]

		new[, h1_new_mean :=
					sl*sl_B +
					forest*forest_B +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					dist_new*newXdyad_mean +
					dist_old*oldXdyad_mean +
					sl*forest*slXforest +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]

		new[, h1_new_max :=
					sl*sl_B +
					forest*forest_B +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					dist_new*newXdyad_max +
					dist_old*oldXdyad_mean +
					sl*forest*slXforest +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]

		new[, x := seq(from = 0, to = 20, length.out = N)]

	}
}

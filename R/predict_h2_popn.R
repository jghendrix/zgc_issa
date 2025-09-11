#' @title Predict H2
#' @export
#' @author Jack G Hendrix
predict_h2_popn <- function(DT, popn) {

	DT %<>% summarise(sl = log(mean(DT$sl_)),
										prop_forest = mean(DT$prop_forest, na.rm = T),
										N_Cladonia = mean(DT$N_Cladonia, na.rm = T),
										N_vasc = mean(DT$N_vasc, na.rm = T))

	new <- cbind(DT, popn)
	setDT(new)

	new[, h2_mean :=
				sl*sl_B +
				prop_forest*forest_B +
				N_Cladonia*clad_B +
				N_vasc*vasc_B +
				sl*prop_forest*slXforest +
				sl*N_Cladonia*slXclad +
				sl*N_vasc*slXvasc
	]
	
	new[, h2_min :=
	      h2_mean - N_Cladonia*clad_B +
	      N_Cladonia*clad_B_min
	]
	
	new[, h2_max :=
	      h2_mean - N_Cladonia*clad_B +
	      N_Cladonia*clad_B_max
	]
	
	return(new)
}

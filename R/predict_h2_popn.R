#' @title Predict H2
#' @export
#' @author Jack G Hendrix
predict_h2_popn <- function(DT, popn, predictor) {

	DT %<>% summarise(sl = log(mean(DT$sl_)),
	                  ta = cos(mean(DT$ta_)),
										prop_forest = mean(DT$prop_forest, na.rm = T),
										N_Cladonia = mean(DT$N_Cladonia, na.rm = T),
										N_vasc = mean(DT$N_vasc, na.rm = T))

	new <- cbind(DT, popn)
	setDT(new)

	new[, h2_mean :=
				sl*sl_B +
				ta*ta_B +
				prop_forest*forest_B +
				N_Cladonia*clad_B +
				N_vasc*vasc_B +
				sl*prop_forest*slXforest +
				sl*N_Cladonia*slXclad +
				sl*N_vasc*slXvasc
	]
	
	if(predictor == "Cladonia") {
	new[, h2_min :=
	      h2_mean - 
	      N_Cladonia*clad_B +
	      N_Cladonia*clad_B_min -
	      sl*N_Cladonia*slXclad +
	      sl*N_Cladonia*slXclad_max
	]
	
	new[, h2_max :=
	      h2_mean - N_Cladonia*clad_B +
	      N_Cladonia*clad_B_max -
	      sl*N_Cladonia*slXclad +
	      sl*N_Cladonia*slXclad_min
	]
	}
	
	if(predictor == "vasc") {
	  
	  new[, h2_min :=
	        h2_mean - N_vasc*vasc_B +
	        N_vasc*vasc_B_min -
	        sl*N_vasc*slXvasc +
	        sl*N_vasc*slXvasc_max
	  ]
	  
	  new[, h2_max :=
	        h2_mean - N_vasc*vasc_B +
	        N_vasc*vasc_B_max -
	        sl*N_vasc*slXvasc +
	        sl*N_vasc*slXvasc_min
	  ] 
	  
	}
	
	return(new)
}

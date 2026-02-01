
predict_h1_Cladonia_popn <- function(DT, popn) {

	N <- 100L

	N_Cladonia <- seq(from = 0, to = 1, length.out = N)
	popn %<>% slice(rep(1:n(), each = 100))


			new_data <- as.data.frame(N_Cladonia) %>%
				mutate(sl = log(mean(DT$sl_)),
				       ta = cos(mean(DT$ta_)),
				       prop_forest = mean(DT$prop_forest, na.rm = T),
				       N_vasc = mean(DT$N_vasc, na.rm = T))

			new <- cbind(new_data, popn)
			setDT(new)

			new[, h1_Clad_mean :=
			      sl*sl_B +
			      ta*ta_B +
			      prop_forest*forest_B +
			      N_Cladonia*clad_B +
			      N_vasc*vasc_B +
			      sl*prop_forest*slXforest +
			      sl*N_Cladonia*slXclad +
			      sl*N_vasc*slXvasc
			]
		
			new[, h1_Clad_min :=
			      sl*sl_B +
			      ta*ta_B +
			      prop_forest*forest_B +
			      N_Cladonia*clad_B_min +
			      N_vasc*vasc_B +
			      sl*prop_forest*slXforest +
			      sl*N_Cladonia*slXclad_min +
			      sl*N_vasc*slXvasc
			]
			
			new[, h1_Clad_max :=
			      sl*sl_B +
			      ta*ta_B +
			      prop_forest*forest_B +
			      N_Cladonia*clad_B_max +
			      N_vasc*vasc_B +
			      sl*prop_forest*slXforest +
			      sl*N_Cladonia*slXclad_max +
			      sl*N_vasc*slXvasc
			]
		
			new[, x := seq(from = 0, to = 1, length.out = N)]

}

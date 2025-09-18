
predict_h1_vasc_popn <- function(DT, popn) {

	N <- 100L

	N_vasc <- seq(from = 0, to = 4, length.out = N)
	popn %<>% slice(rep(1:n(), each = 100))


			new_data <- as.data.frame(N_vasc) %>%
				mutate(sl = log(mean(DT$sl_)),
				       ta = cos(mean(DT$ta_)),
				       prop_forest = mean(DT$prop_forest, na.rm = T),
				       N_Cladonia = mean(DT$N_Cladonia, na.rm = T))

			new <- cbind(new_data, popn)
			setDT(new)

			new[, h1_vasc_mean :=
			      sl*sl_B +
			      ta*ta_B +
			      prop_forest*forest_B +
			      N_Cladonia*clad_B +
			      N_vasc*vasc_B +
			      sl*prop_forest*slXforest +
			      sl*N_Cladonia*slXclad +
			      sl*N_vasc*slXvasc
			]
		
			new[, h1_vasc_min :=
			      sl*sl_B +
			      ta*ta_B +
			      prop_forest*forest_B +
			      N_Cladonia*clad_B +
			      N_vasc*vasc_B_min +
			      sl*prop_forest*slXforest +
			      sl*N_Cladonia*slXclad +
			      sl*N_vasc*slXvasc_max
			]
			
			new[, h1_vasc_max :=
			      sl*sl_B +
			      ta*ta_B +
			      prop_forest*forest_B +
			      N_Cladonia*clad_B +
			      N_vasc*vasc_B_max +
			      sl*prop_forest*slXforest +
			      sl*N_Cladonia*slXclad +
			      sl*N_vasc*slXvasc_min
			]
		
			new[, x := seq(from = 0, to = 4, length.out = N)]

}

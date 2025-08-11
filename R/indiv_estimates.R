#' @title Individual estimates
#' @export
#' @author Jack G Hendrix
indiv_estimates <- function(model) {

	indiv <- as.data.table(
		coef(model)$cond$id %>% rownames_to_column("id") %>%
			pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
			mutate(method = "ME")
	)

	ran_vals.m <- broom.mixed::tidy(model, effect = 'ran_vals')
	indiv.se.m <- setDT(ran_vals.m)[group == 'id', .(id = level, term, se = std.error)]
	indiv <- left_join(indiv, indiv.se.m, by = c('id', 'term'))

	# for whatever reason, it's including the random and population level step length - distance interactions separately... have to manually combine them

	vasc_pop <- mean(subset(indiv, term == "I(log(sl_)):N_vasc")$estimate)
	clad_pop <- mean(subset(indiv, term == "I(log(sl_)):N_Cladonia")$estimate)

	indiv %<>% mutate(estimate = ifelse(term == "N_vasc:I(log(sl_))",
																			estimate + vasc_pop, estimate),
										estimate = ifelse(term == "N_Cladonia:I(log(sl_))",
																			estimate + clad_pop, estimate)) %>%
		filter(term != "I(log(sl_)):N_vasc" & term != "I(log(sl_)):N_Cladonia") %>%
	  mutate(term = ifelse(term == "I(log(sl_)):prop_forest",
	                       "prop_forest:I(log(sl_))", term))

	indiv %<>% filter(term != "(Intercept)")

	return(indiv)

}

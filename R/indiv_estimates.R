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

	#if(term == "fire") {
	#	new_pop <- mean(subset(indiv, term == "I(log(sl_)):I(log(dist_to_new_burn + 1))")$estimate)
	#	old_pop <- mean(subset(indiv, term == "I(log(sl_)):I(log(dist_to_old_burn + 1))")$estimate)

	#	indiv %<>% mutate(estimate = ifelse(term == "I(log(dist_to_new_burn + 1)):I(log(sl_))",
	#																			estimate + new_pop, estimate),
	#										estimate = ifelse(term == "I(log(dist_to_old_burn + 1)):I(log(sl_))",
	#																			estimate + old_pop, estimate)) %>%
	#		filter(term != "I(log(sl_)):I(log(dist_to_new_burn + 1))" & term != "I(log(sl_)):I(log(dist_to_old_burn + 1))")

	#}

	#else {
	#	tch_pop <- mean(subset(indiv, term == "I(log(sl_)):I(log(dist_to_tch + 1))")$estimate)
	#	minor_pop <- mean(subset(indiv, term == "I(log(sl_)):I(log(dist_to_minor + 1))")$estimate)

	#	indiv %<>% mutate(estimate = ifelse(term == "I(log(dist_to_tch + 1)):I(log(sl_))",
	#																			estimate + tch_pop, estimate),
	#										estimate = ifelse(term == "I(log(dist_to_minor + 1)):I(log(sl_))",
	#																			estimate + minor_pop, estimate)) %>%
	#		filter(term != "I(log(sl_)):I(log(dist_to_tch + 1))" &
	#					 	term != "I(log(sl_)):I(log(dist_to_minor + 1))")

	#}

	indiv %<>% filter(term != "(Intercept)")

	return(indiv)

}

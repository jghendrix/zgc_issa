#' @title Individual estimates w/ SE
#'  @export
#' @author Jack G Hendrix
sum_plot <- function(DT, theme, predictor) {


#		sum <- DT %>%
#		group_by(term) %>%
#		summarise(mean = mean(estimate),
#							mse = sd(estimate, na.rm = T)/sqrt(10))  %>%
#		mutate(source = "avg")

	if(predictor %in% c("social road", "social fire")) {

		fixed <- read.csv('input/popn_est.csv') %>%
			filter(model == predictor) %>%
			dplyr::select(popn = term, mean = estimate, mse = se) %>%
			mutate(source = "fixed")

#		sum <- rbind(sum, fixed)

	gbox <-	ggplot(data = fixed, aes(x = popn, y = mean)) +
			geom_pointrange(data = DT,
											aes(x = term, y = estimate, ymin = estimate - se, ymax = estimate + se,
													colour = id, alpha = 0.5),
											position = position_jitter(width = 0.3),
											linetype = "dotted",
											show.legend = F) +
			geom_pointrange(aes(ymin = mean - mse, ymax = mean + mse)) +
			geom_hline(yintercept = 0, lty = 'dashed') +
			xlab("") +
			ylab("Estimate ± SE") +
			scale_color_viridis(discrete = "TRUE")  +
			scale_fill_viridis(discrete = "TRUE") +
			coord_flip() +
			plot_theme()

		ggsave(
			paste0('graphics/rev/summary/', predictor, '_indiv_selection.png'),
			gbox,
			width = 10,
			height = 10,
			dpi = 320
		)

		return(gbox)
	}

	else {

		fixed <- read.csv('input/popn_est.csv') %>%
		filter(season == "annual" & model == predictor) %>%
		dplyr::select(popn = term, mean = estimate, mse = se) %>%
		mutate(source = "fixed")

#	sum <- rbind(sum, fixed)

gbox <- ggplot(data = fixed, aes(x = popn, y = mean)) +
		geom_pointrange(data = DT,
										aes(x = term, y = estimate, ymin = estimate - se, ymax = estimate + se,
												colour = id, alpha = 0.5),
										position = position_jitter(width = 0.3),
										linetype = "dotted",
										show.legend = F) +
	geom_pointrange(aes(ymin = mean - mse, ymax = mean + mse)) +
	geom_hline(yintercept = 0, lty = 'dashed') +
	xlab("") +
	ylab("Estimate ± SE") +
	scale_color_viridis(discrete = "TRUE")  +
	scale_fill_viridis(discrete = "TRUE") +
		coord_flip() +
		plot_theme()

	ggsave(
		paste0('graphics/rev/summary/', predictor, '_indiv_selection.png'),
		gbox,
		width = 10,
		height = 10,
		dpi = 320
	)

	return(gbox)
	}
}

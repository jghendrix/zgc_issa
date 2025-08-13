#' @title Plot RSS by speed
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
plot_rss_speed <- function(rss, theme) {


	rss %<>% mutate(idspd = paste0(id, "_", speed)) %>%
    mutate(`Speed percentile` = ifelse(speed == 0.25, "25th (18 m/h)", "50th (65 m/h)"),
           `Speed percentile` = ifelse(speed == 0.75, "75th (164 m/h)", `Speed percentile`))


	ggplot(data = rss, aes(x = x, y = rss, colour = `Speed percentile`)) +
		geom_line(aes(group = idspd, alpha = 0.5),
							linewidth = 0.5,
							linetype = "dashed",
							show.legend = F
		) +
		scale_color_viridis(discrete = "TRUE", option = "D", begin = 0.85, end = 0.2) +
		geom_smooth(linewidth = 1.2, method = "lm",
								fill = "darkgrey",
								formula = "y ~ x") +
		geom_hline(
			yintercept = 0,
			colour = "black",
			lty = 2,
			size = .7
		) +
		plot_theme() +
	#	ylim(c(-10, 3)) +
		theme(plot.title = element_text(size = 12, hjust = 0.05),
					axis.title = element_text(size = 12),
					axis.text = element_text(size = 10))

}
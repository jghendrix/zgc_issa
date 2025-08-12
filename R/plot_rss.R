#' @title Plot RSS
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
plot_rss <- function(rss, theme) {

ggplot(data = rss, aes(x, rss)) +
		geom_line(aes(group = id, colour = id, alpha = .0001),
							linetype = 'twodash', show.legend = F
							)+
		geom_smooth(linewidth = 1.2,
								colour = "black",
								fill = "darkgrey",
								method = "lm",
								formula = "y ~ x") +
		geom_hline(
			yintercept = 0,
			colour = "black",
			lty = 2,
			linewidth = .7
		) +
		scale_color_viridis(discrete = "TRUE")  +
		scale_fill_viridis() +
	#	scale_y_continuous(limits = c(-2, 1)) +
		plot_theme()
}

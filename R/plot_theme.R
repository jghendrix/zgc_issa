#' @title Plot theme
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
plot_theme <- function() {
	theme_bw()  + theme(
		panel.border = element_blank(),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.line = element_line(colour = "black", linewidth = .7),
		plot.title = element_text(size = 12, hjust = 0.05),
		axis.title = element_text(size = 15),
		axis.text.x = element_text(size = 12, margin = margin(10, 10, 10, 10, "pt")),
		axis.text.y = element_text(size = 12, margin = margin(10, 10, 10, 10, "pt")),
		axis.ticks.length = unit(-0.25, "cm")
	)
}


#' @title Plot distributions
#' @export
#' @author Alec L. Robitaille, Julie W. Turner
plot_distributions <- function(DT, col) {
	ggplot(DT, aes_string(col)) +
		labs(title = paste0('ID: ', unique(DT$Animal_ID))) +
		geom_density(alpha = 0.4) +
		plot_theme()
}

#' @title Plot boxplot
#' @export
#' @author Julie W. Turner
plot_box_horiz <- function(DT, theme) {

	ggplot(data = DT[term !='(Intercept)'],
				 aes(term, estimate)) +
		geom_boxplot(aes(color = term)) +
		geom_jitter() +
		geom_hline(yintercept = 0, lty = 'dashed') +
		coord_flip() +
		theme
}

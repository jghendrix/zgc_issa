#' @title Calculate RSS
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
calc_rss_popn <- function(pred_h1, h1_col, pred_h2, h2_col) {

	pred_h2 %<>% dplyr::select(c(h2_col)) %>%
		slice(rep(1:n(), each = 100))

	log_rss <- cbind(pred_h1[, .SD, .SDcols = c('x', h1_col)],
									 pred_h2)

	log_rss[, rss := h1 - h2,
					env = list(h1 = h1_col, h2 = h2_col)]
}

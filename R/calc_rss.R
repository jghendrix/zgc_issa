#' @title Calculate RSS
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
calc_rss <- function(pred_h1, h1_col, pred_h2, h2_col) {
	log_rss <- merge(pred_h1[, .SD, .SDcols = c('id', 'x', h1_col)],
									 pred_h2[, .SD, .SDcols = c('id', h2_col)],
									 by = 'id', all.x = TRUE,
									 allow.cartesian=TRUE)
	log_rss[, rss := h1 - h2,
					env = list(h1 = h1_col, h2 = h2_col)]
}

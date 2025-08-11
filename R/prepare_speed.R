#' @title Prepare speed
#' @export
#' @author Jack G Hendrix
prepare_speed <- function(DT, summary, params) {

	sum <- DT[,.(mean_Clad = mean(N_Cladonia, na.rm = T),
							 mean_vasc = mean(N_vasc, na.rm = T),
							 mean_forest = mean(prop_forest, na.rm = T)),
						by = .(id)]

	dat.wide <- dcast(summary[term %like% 'sl'], id ~ term, value.var = 'estimate')

	dat.wide <- setDT(merge(dat.wide, setDT(params)[,.(id = as.character(id),shape, scale, kappa)], by = 'id', all.x = T))
	dat.wide <- setDT(merge(dat.wide, sum, by = 'id', all.x = T))
	dat.wide
}

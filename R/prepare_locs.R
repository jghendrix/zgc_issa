#' @title Prepare locs
#' @export
#' @author Alec L. Robitaille, Julie W. Turner
prepare_locs <- function(DT, id, datetime, tz, split_by) {
  
  setDT(DT)
  
	stopifnot(is.character(datetime))

	DT[, (datetime) := parse_date(datetime_char, default_tz = tz),
		 env = list(datetime_char = datetime)]

	# Make unique and complete
	unique_DT <- unique(DT, by = c(id, datetime))
	na_omit_DT <- na.omit(unique_DT, cols = c(datetime))

	# Make splits
	na_omit_DT[, tar_group := .GRP, by = c(split_by)]

	# Rename columns
	setnames(na_omit_DT, c(datetime), c('t_'))

	setDT(na_omit_DT)

	return(na_omit_DT)
}

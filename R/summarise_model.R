#' @title Summarise and check model
#' @export
#' @author Jack G Hendrix
summarise_model <- function(model, DT) {

	print(unique(DT$species))
	print(summary(model)$r.squared)

}

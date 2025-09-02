#' @title Predict H1 Cladonia
#' @export
#' @author Jack G Hendrix
predict_h1_Cladonia_ratio <- function(DT, model) {
  N <- 100L
  
  setDT(DT)
  
  new_data <- DT[, .(
    sl_ = mean(sl_),
    ta_ = mean(ta_),
    prop_forest = mean(prop_forest, na.rm = T),
    CN_Cladonia = seq(from = 0, to = 1, length.out = N),
    CN_vasc = mean(CN_vasc, na.rm = T),
    indiv_step_id = NA,
    yr = NA
  ), by = id]
  
  new_data[, h1_Cladonia := predict(model, .SD, type = 'link', re.form = NULL)]
  
  new_data[, x := seq(from = 0, to = 1, length.out = N), by = id]
}

#' @title Predict H1 Cladonia for various speed levels
#' @export
#' @author Jack G Hendrix
predict_h1_Cladonia_speed <- function(DT, model, quart) {
  N <- 100L
  
  setDT(DT)
  
  new_data <- DT[, .(
    sl_ = quantile(sl_, quart),
    ta_ = mean(ta_),
    prop_forest = mean(prop_forest, na.rm = T),
    N_Cladonia = seq(from = 0, to = 1, length.out = N),
    N_vasc = mean(N_vasc, na.rm = T),
    indiv_step_id = NA,
    yr = NA
  ), by = id]
  
  new_data[, h1_Cladonia := predict(model, .SD, type = 'link', re.form = NULL)]
  
  new_data[, x := seq(from = 0, to = 1, length.out = N), by = id]
  new_data[, speed := quart]
}

#' @title Predict H1 vascular
#' @export
#' @author Jack G Hendrix
predict_h1_vasc <- function(DT, model) {
  N <- 100L
  
  setDT(DT)
  
  new_data <- DT[, .(
    sl_ = mean(sl_),
    ta_ = mean(ta_),
    prop_forest = mean(prop_forest, na.rm = T),
    N_Cladonia = mean(N_Cladonia, na.rm = T),
    N_vasc = seq(from = 0, to = 4, length.out = N),
    indiv_step_id = NA,
    yr = NA
  ), by = id]
  
  new_data[, h1_vasc := predict(model, .SD, type = 'link', re.form = NULL)]
  
  new_data[, x := seq(from = 0, to = 4, length.out = N), by = id]
}

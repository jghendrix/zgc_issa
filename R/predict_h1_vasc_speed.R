#' @title Predict H1 Cladonia for various speed levels
#' @export
#' @author Jack G Hendrix
predict_h1_vasc_speed <- function(DT, model) {
  N <- 100L
  
  setDT(DT)
  
  new_data_slow <- DT[, .(
    sl_ = quantile(sl_, 0.1),
    ta_ = mean(ta_),
    prop_forest = mean(prop_forest, na.rm = T),
    N_Cladonia = mean(N_Cladonia, na.rm = T),
    N_vasc = seq(from = 0, to = 4, length.out = N),
    indiv_step_id = NA,
    yr = NA
  ), by = id]
  new_data_slow[, h1_vasc := predict(model, .SD, type = 'link', re.form = NULL)]
  new_data_slow[, x := seq(from = 0, to = 4, length.out = N), by = id]
  new_data_slow[, speed := "10th"]
  
  
  new_data_med <- DT[, .(
    sl_ = quantile(sl_, 0.5),
    ta_ = mean(ta_),
    prop_forest = mean(prop_forest, na.rm = T),
    N_Cladonia = mean(N_Cladonia, na.rm = T),
    N_vasc = seq(from = 0, to = 4, length.out = N),
    indiv_step_id = NA,
    yr = NA
  ), by = id]
  new_data_med[, h1_vasc := predict(model, .SD, type = 'link', re.form = NULL)]
  new_data_med[, x := seq(from = 0, to = 4, length.out = N), by = id]
  new_data_med[, speed := "50th"]
  
  
  new_data_fast <- DT[, .(
    sl_ = quantile(sl_, 0.9),
    ta_ = mean(ta_),
    prop_forest = mean(prop_forest, na.rm = T),
    N_Cladonia = mean(N_Cladonia, na.rm = T),
    N_vasc = seq(from = 0, to = 4, length.out = N),
    indiv_step_id = NA,
    yr = NA
  ), by = id]
  
  new_data_fast[, h1_vasc := predict(model, .SD, type = 'link', re.form = NULL)]
  
  new_data_fast[, x := seq(from = 0, to = 4, length.out = N), by = id]
  new_data_fast[, speed := "90th"]
  
  new_data <- rbind(new_data_slow, new_data_med, new_data_fast)
}

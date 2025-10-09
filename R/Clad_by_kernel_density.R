# Tying overall winter caribou relative density kernel to the stoich data

Clad_by_kernel_density <- function(DT) {

 
  ggplot(DT, aes(x = pt_kernel, y = percent_N)) +
    geom_jitter() +
    geom_smooth(method = "lm") +
    theme_bw()
  
  # most of these sites had no caribou present at all... no clear effect
  # if we assort based on the presence of soil???
  # let's bin it into thirds to look
  # lots of soil depth measurements missing
  # one says "no soil cores" though so let's make that a true zero
  
  DT %<>% mutate(depth = ifelse(n_cores == "No Soil Core", 0, depth))
  
  quantile(DT$depth, 0.33, na.rm = T) # 3.5 cm
  quantile(DT$depth, 0.67, na.rm = T) # 10 cm
  
  DT %<>% mutate(soil = ifelse(depth < 3.5, "shallow", "medium"),
                 soil = ifelse(depth >= 10, "deep", soil))
  
  ggplot(subset(DT, !is.na(depth)), aes(x = pt_kernel, y = percent_N, colour = soil)) +
    geom_jitter() +
    geom_smooth(method = "lm") +
    theme_bw()
  
  summary(lm(percent_N ~ depth*pt_kernel, data = DT))
  
  # There's no relationship there
  # If anything, the interaction term is positive... I mean, they're all nothing effects. But it's not even hinting towards "density matters more when there's no dirt"
  
}
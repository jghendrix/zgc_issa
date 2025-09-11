join_rss_popn <- function(min, mean, max) {
  
  
  mean %<>% dplyr::select(x, rss)
  
  min %<>% dplyr::select(x, min_rss = rss)
  max %<>% dplyr::select(x, max_rss = rss)
  
  rss <- left_join(mean, min, by = c("x"))
  rss <- left_join(rss, max, by = c("x"))
  }

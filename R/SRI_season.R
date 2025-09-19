# === SRI ---------------------------------------------------------------------

SRI_season <- function(DT) {
  
  DT %<>% filter(case_ == TRUE) #%>%
  #mutate(Animal_ID = as.factor(id))

  setDT(DT)

  alloc.col(DT)

    ## Temporal grouping 
        group_times(DT, datetime = 't1_', threshold = '5 minutes')

    ## Spatial grouping
        group_pts(
          DT,
          threshold = 50,
          splitBy = 'yr',
          timegroup = 'timegroup',
          id = 'id',
          coords = c('x1_', 'y1_')
        )

        # Run through each year to calculate SRIs
        
    years <- c(2017, 2018, 2019, 2022, 2023, 2024, 2025)
    all <- data.table()
    
    for(i in years){
      dat <- subset(DT, yr == i)
      gbiMtrx <- get_gbi(DT = dat, group = 'group', id = 'id')
  
      net <- asnipe::get_network(gbiMtrx,
                             data_format = "GBI",
                             association_index = "SRI")
  
      diag(net) <- NA
  
      g <- igraph::graph_from_adjacency_matrix(net, 'undirected', 
                                           diag = FALSE, weighted = TRUE)
  
      sri <- data.table(ID1 = rep(colnames(net), each = nrow(net)), 
                    ID2 = rep(rownames(net), ncol(net)), 
                    sri = as.vector(net),
                    yr = i)
  
  all <- rbind(all, sri)
  
}

SRIs <- data.frame(all) %>% dplyr::filter(!is.na(sri))

  idSRI <- SRIs %>% group_by(ID1, yr) %>%
    summarise(nonzero = sum(sri > 0),
            n = n(),
            SRI = mean(sri),
            mean_nonzero = SRI * (n/nonzero)
            ) %>%
  dplyr::rename(id = ID1)

#write.csv(idSRI, paste0('output/', DT, '_mean_SRI_by_year.csv'))

}
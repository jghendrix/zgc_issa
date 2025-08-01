#' Project locs
#' @inheritParams prep_dates
#' @param EPSG numeric; EPSG code
#'
#' @return
#' @export
#' @author Alec L. Robitaille
#'
#' @examples
project_locs <- function(DT, epsgin, epsgout) {
  data.table::setalloccol(DT)
  if (!is.numeric(epsgout)) stop("epsgout must be numeric")
  
  coords <- c('x_long', 'y_lat')
  projcoords <- c('x_proj', 'y_proj')
  
  lapply(coords, function(x) check_col(DT, x))
  lapply(coords, function(x) check_type(DT, x, 'double'))
  lapply(projcoords, function(x) overwrite_col(DT, x))
  
  if (is.na(as.numeric(epsgin))) {
    if (!epsgin %in% colnames(DT)) {
      stop('if epsgin is a character, it must refer to column name in DT')
    }
    check_type(DT, epsgin, 'integer')
    
    DT[, (projcoords) :=
         data.table::as.data.table(
           sf::sf_project(
             pts = as.matrix(.SD, ncol = 2),
             from = sf::st_crs(.BY[[1]]),
             keep = TRUE,
             to = sf::st_crs(epsgout))
         ),
       .SDcols = coords,
       by = epsgin]
    DT[, epsg_proj := epsgout]
    data.table::set(DT, j = epsgin, value = NULL)
    DT
  } else if (!is.na(as.numeric(epsgin))) {
    if (!sf::st_is_longlat(sf::st_crs(as.numeric(epsgin)))) {
      stop('epsgin must be long lat (unprojected)')
    }
    
    if (sf::st_is_longlat(sf::st_crs(epsgout))) {
      stop('epsgout must not be long lat (unprojected)')
    }
    
    if (epsgin == epsgout) {
      stop('epsgin is equal to epsgout')
    }
    
    DT[, (projcoords) :=
         data.table::as.data.table(
           sf::sf_project(
             pts = as.matrix(.SD, ncol = 2),
             from = sf::st_crs(as.numeric(epsgin)),
             keep = TRUE,
             to = sf::st_crs(epsgout))
         ),
       .SDcols = coords]
    DT[, epsg_proj := as.numeric(epsgout)]
    return(DT)
  }
}

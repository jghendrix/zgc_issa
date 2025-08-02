# === Targets: Fogo stoich mapping ----------------------
# Jack G Hendrix
# 24 February 2025

# Source ------------------------------------------------------------------
targets::tar_source('R')

# Options -----------------------------------------------------------------
tar_option_set(format = 'qs')

# Data --------------------------------------------------------------------
# Path to land cover rasters
# lc = Canadian Forest service, sdss = provincial product
#cfs_path <- file.path('input', 'cfs.tif')
sdss_path <- file.path('input', 'sdss.tif')
#legend_path <- file.path('input', 'cfs_legend.csv')
sdss_legend_path <- file.path('input', 'legend_sdss.csv')

# Stoich layer paths
lichen_path <- file.path('input', 'Cladonia_N.tif')
vascular_path <- file.path('input', 'Vascular_N.tif')

# Caribou data path
gps_path <- file.path('input', 'all_Fogo_caribou_locs_20June2025.csv')
id_path <- file.path('input', 'collar-animal-ids.csv')

# Variables ---------------------------------------------------------------
bb <- c(
  xmin = -54.3533,
  ymin = 49.5194,
  xmax = -53.954220,
  ymax = 49.763834
)
epsg <- 32621
crs <- st_crs(epsg)
crs_sp <- CRS(crs$wkt)


id_col <- 'Animal_ID'
datetime_col <- 'datetime'
x_col <- 'x_long'
y_col <- 'y_lat'
tz <- 'America/St_Johns'

# Split by: within which column or set of columns (eg. c(id, yr))
#  do we want to split our analysis?
split_by <- id_col
seasonal_split <- "season"

# For iSSA tracks:
# Resampling rate
rate <- minutes(120)
# Tolerance
tolerance <- minutes(5)
# Number of random steps
n_random_steps <- 10


# Targets: data -----------------------------------------------------------
targets_data <- c(
  
   tar_file_read(
    caribou,
    gps_path,
    fread(!!.x)
  ),
  tar_file_read(
    ids,
    id_path,
    fread(!!.x)
  ),
  
 # tar_file_read(
#    cfs,
 #   cfs_path,
  #  raster(!!.x)
  #),
#  tar_file_read(
#    cfs_legend,
#    legend_path,
#    fread(!!.x)
#  ),
  
  tar_file_read(
    sdss,
    sdss_path,
    raster(!!.x)
  ),
  tar_file_read(
    sdss_legend,
    sdss_legend_path,
    fread(!!.x)
  ),
  
  tar_file_read(
    lichen,
    lichen_path,
    raster(!!.x)
  ),
  tar_file_read(
    vascular,
    vascular_path,
    raster(!!.x)
  ),
  
  tar_target(
    coast,
    get_coastline(bb, crs)
  ),
  
  tar_target(
    roads,
    get_roads(bb, crs)
  ),
  tar_target(
    water,
    get_lakes(bb, crs)
  )
  
)


# Targets: preparing caribou data ---------------------------
targets_prep <- c(
  
  tar_target(
    tidy_GPS,
    GPS_tidying(caribou, ids)
  ),
  tar_target(
    locs_prep,
    prepare_locs(tidy_GPS, id_col, datetime_col, tz, x_col, y_col, split_by),
    iteration = 'group'
  ),
  tar_target(
    split_key,
    unique(locs_prep[, .SD, .SDcols = c(split_by, 'tar_group')])
  )
  
)

# Targets: tracks ---------------------------------------------------------
targets_tracks <- c(
  tar_target(
    tracks,
    make_track(locs_prep, x_, y_, t_, all_cols = TRUE, crs = 4326) |>
      transform_coords(crs_to = crs),
    pattern = map(locs_prep)
  ),
  tar_target(
    tracks_resampled,
    resample_tracks(tracks, rate = rate, tolerance = tolerance),
    pattern = map(tracks)
  ),
  tar_target(
    tracks_random,
    random_steps(tracks_resampled, n = n_random_steps),
    pattern = map(tracks_resampled)
  )
)
## Extracting data for tracks ------------------------------

targets_extract <- c(
  
  tar_target(
    tracks_cleaned,
    remove_NA_steps(tracks_random)
    )
  ,
  
  tar_target(
    tracks_extract,
    extract_layers(
      tracks_cleaned,
      crs,
      sdss,
      sdss_legend
    )
  ),
  
  tar_target(
    tracks_lichen,
    extract_stoich(
      tracks_extract,
      crs,
      lichen
    )
  ),
  
  # less than 10% of clusters have a lichen value for each cell. lots of NAs... do we consider NA for lichen to be a zero?
  
  tar_target(
    tracks_lichen_fixed,
    tracks_lichen %>% 
      rename(Cladonia_N = pt_stoich) %>%
      mutate(Cladonia_N = ifelse(is.na(Cladonia_N), 0, Cladonia_N))
  ),
  
  tar_target(
    tracks_vasc,
    extract_stoich(
      tracks_lichen_fixed,
      crs,
      vascular
    )
  )
  ## Lots of NAs in the stoich maps. Maybe using a radius around the point instead of that specific 5m cell would be better?
    ,

  tar_target(
    tracks_lichen_radius,
    extract_stoich_radius(
      tracks_extract,
      crs,
      lichen,
      100
    )
  )

)
# Targets: all ------------------------------------------------------------
# Automatically grab and combine all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)

#' @title Prepare model data
#' @export
#' @author Jack G Hendrix
prepare_model <- function(DT, dyads) {
	DT[, indiv_step_id := paste(Animal_ID, step_id_, sep = '_')]

	# Grouping together landcover categories into more meaningful levels
	# Herbs and Rock/Rubble are extremely rare (0.005%), group together w/ NAs in other?

	DT[lc_description %in% c('Coniferous', 'Mixedwood'), lc_adj := 'forest']
	DT[lc_description %in% c('Shrubs'), lc_adj := 'scrub']
	DT[lc_description %in% c('Exposed/Barren Land', 'Rock/Rubble', 'Herbs'), lc_adj := 'open']
	DT[lc_description == 'Wetland', lc_adj := 'wetland']
	DT[lc_description == 'Water', lc_adj := 'water']

	DT <- DT[!is.na(pt_lc)]
	# some points are outside the raster, generally one animal that ventured dozens of km southeast, ignore these for now

	DT[, forest := ifelse(lc_adj == 'forest', 1, 0)]
	DT[, open := ifelse(lc_adj %in% c('open', 'wetland', 'scrub'), 1, 0)]

	DT[, pt_lc := as.factor(pt_lc)]
	DT[, lc_adj := as.factor(lc_adj)]
	DT[, indiv_step_id := as.factor(indiv_step_id)]
	DT[, id := as.factor(Animal_ID)]

	# one step length is exactly 0, errors out when running the model
	# next shortest step is 1.072510e-06, substitute that?
	DT[, sl_ := ifelse(sl_ == 0, 1.072510e-06, sl_)]

	# getting season from season-id
	DT[, season := str_extract(ANIMAL_SN, "(?<=_).*")]
	DT[, season := str_extract(season, "(?<=_).*")]

	DT[, season := ifelse(season %in% c("FM", "FR"), "autumn migration", season)]
	DT[, season := ifelse(season %in% c("C", "PC", "PCR"), "calving", season)]
	DT[, season := ifelse(season == "W", "winter", season)]
	DT[, season := ifelse(season == ("SM"), "spring migration", season)]

	DT[, season := as.factor(season)]

	DT %<>% mutate(Animal_ID = as.factor(Animal_ID))

	# could move this to nn()
	dyads %<>% dplyr::select(c(Animal_ID, t1_ = t_, NN_ID = NN, NN_dist = distance, in_group)) %>%
		mutate(Animal_ID = as.factor(Animal_ID))

	DT <- left_join(DT, dyads, by = c("Animal_ID", "t1_"))

	}

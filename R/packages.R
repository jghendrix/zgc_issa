# Packages ----------------------------------------------------------------
library(targets)
library(tarchetypes)
library(qs)
library(visNetwork)

library(renv)
library(conflicted)
conflicts_prefer(raster::extract)

library(data.table)
library(lme4)

library(amt)
library(sf)
library(raster)
library(parsedate)

library(ggplot2)
library(ggthemes)

library(distanceto)
#library(spatsoc)

library(glmmTMB)
require(betareg)
require(broom.mixed)
require(tidyr)
require(dplyr)
conflicts_prefer(dplyr::filter)
require(tibble)
require(magrittr)
require(viridis)
require(stringr)
require(osmdata)

require(terra)
require(neonUtilities)
require(rhdf5)

# ------------------------------------------------------------------------------------------------ #
#' Script to get worldpop estimates of age for Tana 
# ------------------------------------------------------------------------------------------------ #

# Load libraries
library(raster)
library(fasterize)
library(sf)
library(tidyverse)
library(foreach)

# First get World Pop age estimates (these are here: https://www.worldpop.org/geodata/summary?id=16870)
# They have finer scale age brackets & are at a high resolution
# You will need to have GNU parallel and curl installed to use this script (try homebrew if on mac)
# But only need to do it once to get it pulled down
# Or can manually download them from the link
system("bash wp-age.sh data/raw MDG 2020")

# read in one raster for rasterizing
wp_2020 <- raster("data/raw/mada_100m_2020/mdg_f_0_2020.tif")
values(wp_2020) <- 1:ncell(wp_2020)

# Then rasterize to shapefile of tana only
mada_districts <- st_read("data/raw/mada_districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")
mada_districts <- mutate(mada_districts, tana = ifelse(grepl("MG1110100*", ADM2_PCODE), 1, 0)) # the six arrondisements
wp_2020 <- fasterize(mada_districts, wp_2020, "tana")
wp_2020 <- as.logical(wp_2020[]) # spits out indices

directory <- "data/raw/mada_100m_2020"
files <- list.files(directory, recursive = TRUE, full.names = TRUE)
ages <- as.numeric(unlist(lapply(strsplit(files, "_"), function(x) x[[5]])))
sex <- unlist(lapply(strsplit(files, "_"), function(x) x[[4]]))

# takes a minute or so
sapply(files, function(x) {
  pop <- raster(x)
  pop_tana <- pop[wp_2020]
  return(sum(pop_tana, na.rm = TRUE))
}) -> pop_out

# Make the dataframe and proccess
tana_df <- data.frame(pop = pop_out, age_lower = ages, sex = sex)

tana_df %>%
  group_by(age_lower) %>%
  summarize(pop = sum(pop)) %>%
  mutate(age_upper = case_when(age_lower >= 10 & age_lower < 80 ~ age_lower + 4, 
                               age_lower == 0 ~ 0, 
                               age_lower == 1 ~ 9), 
         prop_pop = pop/sum(pop)) -> tana_df_wp

write_csv(tana_df_wp, "data/processed/TNR_age_wp.csv")
         
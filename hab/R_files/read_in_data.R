# This script reads in all data used in the habitat model ----

# Large river length multipliers, joined to flowline using "Reach" ----
lr_length_raw <- read.csv("hab/Inputs/LR_Length.csv") %>%
  rename(Reach = reach)

# Subbasin names, Subbasin number, and EcoRegion ----
subbasin_names <- read.csv("hab/Inputs/Subbasin_names.csv")

# EDT summer and winter wetted widths.  Joined to flowline using "Reach".  Contains current, historical, 2040 and 2080 widths ----
edt_width <- list.files(path = Inputs, pattern = "edt_width.csv", full.names = T) %>%
  read.csv(.) %>%
  mutate(Reach_low = tolower(Reach)) %>%
  rename(width_w = X1,
         width_s = X8) %>%
  select(Reach_low, width_w, width_s, year)

# EDT temperatures.  This file comes from the Temperature repo.  Joined to the flowline by "Reach" ----
edt_temps <- list.files(path = Inputs, pattern = "edt_temps", full.names = T) %>% 
  read.csv(.) %>%
  select(-X)

# PSU temps.  This file comes from the Temperature repo.  Joined to the flowline by "Seg" which it gets from a spatial join from a PSU spatial layer. ----
psu_temps <- list.files(path = Inputs, pattern = "psu_temps", full.names = T) %>%
  read.csv(.) %>%
  select(-X)

# Temperature gaps.  This file was created manually and fills any gaps in psu/edt temperature data.  Joined to flowline using edt "Reach" layer ----
temp_gaps <- list.files(path = Inputs, pattern = "temp_gaps", full.names = T) %>% 
  read.csv(.) %>%
  select(-notes)

# Culverts.  This file reads in the most recent Chehalis obstructions layer from the spatial model outputs. ----
culvs <- list.files(path = file.path(Inputs, "spatial_model_outputs"), pattern = "culvs_gsu_", full.names = T) %>%
  read.csv(.) %>%
  mutate(noaa_culv = as.numeric(row.names(.)),
         FishPass = as.numeric(as.character(FishPass))/100) %>%
  select(noaa_culv, FeatureTyp,FishPass,OBS_UNIQ, GSU)

# Flowline layer created in spatial model ----
flowline <- list.files(path = file.path(Inputs, "spatial_model_outputs"), pattern = "flowline", full.names = T) %>%
  read.csv(.)

# Large river spawning riffles.  Hand digitized ----
riff <- list.files(path = file.path(Inputs, "spatial_model_outputs"), pattern = "Riffles", full.names = T) %>%
  read.csv(.)

# Large river habitat data ----
LgRiver_raw <- list.files(path = file.path(Inputs, "spatial_model_outputs"), pattern = "LgRiver", full.names = T) %>% 
  read.csv(.) 

# Backwater Habitat data ----
Backwater_raw = list.files(path = file.path(Inputs, "spatial_model_outputs"), pattern = "Backwater", full.names = T) %>% 
  read.csv  %>%
  mutate(Period = "Both")

# Floodplain habitat data ----
Floodplain_raw <- list.files(path = file.path(Inputs, "spatial_model_outputs"), pattern = "Floodplain", full.names = T) %>% 
  read.csv(.) 

# ASRP scenarios ----
asrp_scenarios <- read.csv('hab/Inputs/ASRP_scenarios.csv')

# Scenarios.  Read in list of all scenarios ----
scenarios <- read.csv('hab/Inputs/scenarios.csv') 
  


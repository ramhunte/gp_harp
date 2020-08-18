# This script reads in all data used in the habitat model ----

# Large river length multipliers, joined to flowline using "Reach" ----
lr_length_raw <- read.csv("hab/Inputs/LR_Length.csv") %>%
  rename(Reach = reach)

# Subbasin names, Subbasin number, and EcoRegion ----
subbasin_names <- read.csv("hab/Inputs/Subbasin_names.csv")

# EDT summer and winter wetted widths.  Joined to flowline using "Reach".  Contains current, historical, 2040 and 2080 widths ----
edt_width <- list.files(path = Inputs, pattern = "edt_width.csv", full.names = T) %>%
  read.csv(.) %>%
  mutate(Reach_low = tolower(Reach))


if (fishtype == 'chum') {
  edt_width %<>%
    rename(width_w = X1,
           width_s = X3)
} else if (fishtype %in% c('spring_chinook', 'fall_chinook')) {
  edt_width %<>%
    rename(width_w = X1,
           width_s = X5)
} else {
  edt_width %<>%
    rename(width_w = X1,
         width_s = X8)
}


edt_width %<>%
  select(Reach_low, width_w, width_s, year) %>%
  group_by(Reach_low) %>%
  mutate(width_s_curr = ifelse(year == 2019,
                               width_s,
                               0),
         width_s_curr = sum(width_s_curr),
         width_s = ifelse(year %in% c(2040, 2080),
                          width_s_curr * .95,
                          # width_s,
                          width_s)) %>%
  select(-width_s_curr) %>%
  gather(stage, width, width_w:width_s) %>%
  mutate(stage = case_when(year == 2019 & stage == "width_s" ~ "width_s",
                           year == 2019 & stage == "width_w" ~ "width_w",
                           year == 1900 & stage == "width_s" ~ "width_s_hist",
                           year == 1900 & stage == "width_w" ~ "width_w_hist",
                           year == 2040 & stage == "width_s" ~ "width_s_2040",
                           year == 2040 & stage == "width_w" ~ "width_w_2040",
                           year == 2080 & stage == "width_s" ~ "width_s_2080",
                           year == 2080 & stage == "width_w" ~ "width_w_2080")) %>%
  select(-year) %>%
  spread(stage, width)

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
  read.csv(.) %>%
  mutate(source_hab = "LgRiver",
         Habitat = case_when(HabUnit %in% c("Bank", "Bank-TM") ~ "Bank",
                             HabUnit %in% c("Bank HM", "Bank HM-TM") ~ "HM_Bank",
                             HabUnit %in% c("Bar-boulder", "Bar-boulder-TM") ~ "Bar_boulder",
                             HabUnit %in% c("Bar-gravel", "Bar-gravel-TM") ~ "Bar_gravel",
                             HabUnit %in% c("Bar-sand", "Bar-sand-TM") ~ "Bar_sand"),
         Period = ifelse(Period == " Hist", 
                         "Hist", 
                         as.character(Period)))

# Backwater Habitat data ----
Backwater_raw = list.files(path = file.path(Inputs, "spatial_model_outputs"), pattern = "Backwater", full.names = T) %>% 
  read.csv  %>%
  mutate(Period = "Both")

# Floodplain habitat data ----
Floodplain_raw <- list.files(path = file.path(Inputs, "spatial_model_outputs"), pattern = "Floodplain", full.names = T) %>% 
  read.csv(.) 

# ASRP scenarios ----
asrp_scenarios_raw <- read.csv('hab/Inputs/ASRP_scenarios.csv')

# Scenarios.  Read in list of all scenarios ----
scenarios <- read.csv('lcm/data/scenarios.csv') 
  
# Future impervious area ----
fut_imperv <- read.csv('hab/Inputs/future_impervious.csv') %>%
  rename(mid_century_imperv = Mid.century.Added.Impervious.Area,
         late_century_imperv = Late.century.Added.Impervious.Area) %>%
  select(GSU, mid_century_imperv, late_century_imperv) %>%
  mutate(
    mid_century_imperv = as.numeric(gsub("%", "", mid_century_imperv)) / 100,
    late_century_imperv = as.numeric(gsub("%", "", late_century_imperv)) / 100) %>% 
  gather(year, future_imperv, mid_century_imperv, late_century_imperv) %>%
  mutate(year = case_when(
    year == 'mid_century_imperv' ~ 2040,
    year == 'late_century_imperv' ~ 2080
  )) %>%
  group_by(year, GSU) %>%
  summarize(future_imperv = sum(future_imperv, na.rm = T))

# FP temperature reduction
ss_fp_reconnect <- Floodplain_raw %>%
  # read.delim('hab/inputs/floodplain_5m.txt', header = TRUE, sep = ",") %>%
  filter(HabUnit == 'Marsh', 
         NEAR_DIST < 5) %>%
  group_by(noaaid, Period) %>%
  summarize(Area = sum(Area_ha, na.rm = T)) %>%
  spread(Period, Area) %>%
  filter(Hist > 0,
         # Curr != Hist)
         is.na(Curr) | (Hist - Curr)/Hist > .50) %>%
  pull(noaaid)
    


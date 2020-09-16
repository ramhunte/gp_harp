# Purpose: Read in the hand digitized riffle data
# The riffle polygons are used to denote large river reaches that have one or more riffles
# Using a bankfull width and spawnable length (scaled to bankfull width) calculate a spawning 
# area per noaaid

# Read in data ----

# Hand digitized riffles in the large rivers
# Summarize as number of riffles per noaaid
riff_count <- riff %>%
  rename(Subbasin = noaa_sub) %>%
  left_join(subbasin_names %>%
              select(Subbasin, Subbasin_num)) %>%
  select(noaaid, Subbasin_num, Shape_Area) %>%
  group_by(Subbasin_num, noaaid) %>%
  summarize(riff_count = n()) %>%
  ungroup()


# Flowline in the large river segments
lgr <- flowline %>%
  left_join(., read.csv('misc/width.csv') %>%
              select(-X)) %>%
  filter(Habitat == 'LgRiver',
         spawn_dist == 'Yes',
         width_s < 200,
         year %in% c(2019, 1900)) %>%
  select(noaaid, Subbasin_num, Spawn_Survey, Shape_Length, 
         # spawn_dist, species, both_chk, width_w, width_s, year, chino_mult) %>% ### use this once bugfix is fixed
         spawn_dist, species, both_chk, width_w, width_s, year) %>%
  gather(Period, width, width_w:width_s) %>%
  filter(ifelse(species == 'spring_chinook', 
                Period == 'width_s',
                Period == 'width_w')) %>%
  mutate(area_bf_m2 = Shape_Length * width,
         Period = ifelse(year == 1900,
                         'Historical',
                         'Current'))  %>%
  select(-year)

# If it is a reach with overlapping spring and fall chinook spawning, 
# reduce width by chino_mult. This reduction is later transferred to spawning area

if (fishtype %in% c("spring_chinook", "fall_chinook")) {
  lgr <- lgr %>%
    mutate(width = ifelse(both_chk == "Yes",
                          width * chino_mult,
                          width))
}

# This is used to fill in riffle area values for reaches where we could not see the river (Spawn_Survey == N)
ave_riff_per_reach <- inner_join(riff_count, lgr) %>%
  group_by(Subbasin_num) %>%
  summarize(ave_riff_per_reach  = mean(riff_count)) 

# Calculate the spawning capacity ----

#To calculate minimum spawning area we use the BF width and a length to represent the riffle crest
#The lengths are scaled with BF width as follows: 

w40 <- 20 #For rivers > 40 m wet width use 20 m
w30 <- 15 #For rivers 20< x <40 m wet width use 15 m
w20 <- 10 #For rivers < 20 m wet width use 10 m


lgr_spawning_area <- lgr %>%
  left_join(riff_count) %>%
  left_join(ave_riff_per_reach) %>% # add riffle areas to reaches
  mutate(riff_count = ifelse(is.na(riff_count), 0, riff_count),
         riff_count = ifelse(Spawn_Survey == 'N',
                             ave_riff_per_reach,
                             riff_count)) %>%
  mutate(riff_length = ifelse(width > 40,
                              w40,   
                              ifelse(width < 40 & width > 20,
                                     w30,
                                     w20)),
         spawn_area = width * riff_length * riff_count) %>% 
  select(spawn_area, Period, Subbasin_num, noaaid) %>%
  spread(Period, spawn_area) %>%
  rename(spawn_area = Current,
         spawn_area_hist = Historical)

assign("lgr_sp_area_asrp", lgr_spawning_area , envir = .GlobalEnv)

if (fishtype == "spring_chinook") {
  lgr_spawning_area <- lgr_spawning_area %>%
    filter(Subbasin_num %in% schino_subs)
  
  lgr_sp_area_asrp <- lgr_sp_area_asrp %>%
    filter(Subbasin_num %in% schino_subs)
}




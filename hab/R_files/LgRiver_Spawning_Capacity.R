library(ggplot2)
library(dplyr)
library(tidyr)


options(scipen = 999) #removed scientific notation

# Read in data ----

# Hand digitized riffles in the large rivers
# Summarize as area per noaaid reach
riff <- list.files(path = "Inputs", pattern = "Riffles", full.names = T) %>%
  read.csv(.) %>%

  rename(Subbasin = noaa_sub) %>%
  left_join(subbasin_names %>%
              select(Subbasin, Subbasin_num)) %>%
  select(noaaid, Subbasin_num, Shape_Area) %>%
  group_by(Subbasin_num, noaaid) %>%
  summarize(riff_area_m2 = sum(Shape_Area)) %>%
  ungroup()


# SWIFD line in the large river segments
lgr <- flowline %>%
  filter(Habitat == 'LgRiver') %>%
  select(noaaid, Subbasin_num, BF_width, wet_width, Spawn_Survey, pass_tot, pass_tot_natural, Shape_Length, spawn_dist, species, both_chk) %>%
  mutate(area_bf_m2 = Shape_Length * BF_width)

# This is used to fill in riffle area values for reaches where I could not see the river (Spawn_Survey == N)
ave_prcnt_riff <- inner_join(riff, lgr) %>%
  mutate(prcnt_riff = riff_area_m2 / area_bf_m2) %>%
  filter(prcnt_riff < 1) %>% # Remove a few with percents over 100%
  group_by(Subbasin_num) %>%
  summarize(ave_prcnt_riff = mean(prcnt_riff)) 
  
# Calculate the spawning capacity ----

#To calculate minimum spawning area we use the BF width and a length to represent the riffle crest
#The lengths are scaled with BF width as follows: 

bkf40 <- 20 #For rivers > 40 m BF width use 20 m
bkf30 <- 15 #For rivers 20< x <40 m BF width use 15 m
bkf20 <- 10 #For rivers < 20 m BF width use 10 m

#Threshold river sizes for each species
coho_sp_thresh <- 45 #Coho don't spawn in rivers larger than 45 m bf width
sthd_sp_thresh <- 55 #Steelhead don't spawn in rivers larger than 55 m bf width
#Chinook and chum do not have a maximum river size

#Scaling factor for chinook spawning areas
fchnk_scale = 0.81 # Fall chinook
schnk_scale = 0.19 # Spring chinook


spawning_area <- lgr %>%
  left_join(., ave_prcnt_riff) %>% # add riffle areas to reaches
  filter(spawn_dist == "Yes",
         !(BF_width > sthd_sp_thresh & species == 'steelhead'),
         !(BF_width > coho_sp_thresh & species == 'coho')) %>%
  mutate(riff_area_m2B = ifelse(Spawn_Survey %in% 'N',
                                area_bf_m2*ave_prcnt_riff,
                                area_bf_m2)) %>%
  filter(!is.na(area_bf_m2)) %>%
  mutate(min_length = ifelse(BF_width > 40,
                             bkf40,   
                             ifelse(BF_width < 40 & BF_width > 20,
                                    bkf30,
                                    bkf20)),
         spawn_area = ifelse(BF_width*min_length > area_bf_m2,
                             area_bf_m2,
                             BF_width*min_length), #Multiply BF width and minimum lengths
         spawn_area_passable = spawn_area*pass_tot,
         spawn_area_passable_nat = spawn_area * pass_tot_natural) %>% #Apply culvert passablility correction to the min spawnable area
  assign("lgr_sp_area_asrp", . , envir = .GlobalEnv) %>%
  group_by(Subbasin_num, species) %>%
  summarize(spawn_area = sum(spawn_area, na.rm = T),
            spawn_area_passable = sum(spawn_area_passable, na.rm = T),
            spawn_area_passable_nat = sum(spawn_area_passable_nat, na.rm = T)) %>%
  ungroup()

if (fishtype == "spring_chinook") {
  spawning_area <- spawning_area %>%
    filter(Subbasin_num %in% schino_subs)
  
  lgr_sp_area_asrp <- lgr_sp_area_asrp %>%
    filter(Subbasin_num %in% schino_subs)
}

    


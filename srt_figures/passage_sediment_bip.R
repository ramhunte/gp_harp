# Create habitat metrics for SRT meeting

# Barriers, Beaver, Finesediment
# Final dataframe with necessary columns is called df_gsu

# Barriers ---- 
Inputs <- 'hab/Inputs'
source('hab/R_files/2-read_in_data.R')

gsu_list <- flowline %>%
  distinct(GSU)

fl <- flowline %>% 
  select(noaaid, GSU, Reach,culv_list) %>%
  filter(culv_list != "") %>%
  mutate(culv_list = str_split(culv_list,',')) %>%
  unnest(culv_list) %>%
  filter(culv_list != 'None') %>%
  mutate(noaa_culv = as.numeric(culv_list)) %>%
  left_join(., culvs %>%
              select(-GSU)) %>%
  group_by(GSU, noaaid) %>%
  summarize(pass_tot = prod(FishPass),
            pass_tot_natural = prod(ifelse(FeatureTyp == 'Natural',FishPass,1))) %>%
  replace_na(list(pass_tot = 1, pass_tot_natural = 1))


gsu_pass <- fl %>% 
  group_by(GSU) %>%
  summarize(pass_tot = mean(pass_tot, na.rm = TRUE),
            pass_tot_nat = mean(pass_tot_natural, na.rm = TRUE)) %>%
  mutate(pass_diff = pass_tot - pass_tot_nat) %>%
  right_join(gsu_list) %>%
  mutate(pass_diff = ifelse(is.na(pass_diff), 0, pass_diff),
         pass_diff = ifelse(GSU == "Upper Skookumchuck", NA, pass_diff))



# Finesediment

gsu_sed <- flowline %>%
  group_by(GSU) %>%
  summarize(sed_current = mean(sed_current, na.rm = TRUE),
            sed_hist = mean(sed_hist, na.rm = TRUE)) %>%
  mutate(sed_diff = abs(sed_hist - sed_current))


# Or if do we want road density?


# Beaver

gsu_bip <- flowline %>%
  mutate(score_slope = 
           case_when(
             slope < .01 ~ 4,
             between(slope, .01, .02) ~ 3,
             between(slope, .02, .04) ~ 2,
             between(slope, .04, .06) ~ 1,
             between(slope, .06, .10) ~ 0.5,
             slope > .10 ~ 0
           ),
         score_width = case_when(
           BF_width < 7 ~ 4,
           between(BF_width, 7, 10) ~ 3,
           between(BF_width, 10, 18) ~ 2,
           between(BF_width, 18, 24) ~ 1,
           BF_width > 24 ~ 0),
         score = score_slope + score_width) %>%
  #select(GSU, noaaid, slope, BF_width, score_slope, score_width, score)
  group_by(GSU) %>%
  summarize(BIP = mean(score, na.rm = TRUE))




# Ouput
df_gsu <- gsu_list %>%
  left_join(gsu_pass) %>%
  left_join(gsu_sed) %>%
  left_join(gsu_bip) %>%
  left_join(spawn_area_gsu) %>%
  left_join(avg_temp_gsu) %>%
  left_join(fp_areas_gsu)





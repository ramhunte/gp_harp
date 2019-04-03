edt_width <- list.files(path = Inputs, pattern = "edt_width.csv", full.names = T) %>%
  read.csv(.) %>%
  mutate(Reach_low = tolower(Reach)) %>%
  rename(width_w = X1,
         width_s = X8) %>%
  select(Reach_low, width_w, width_s, year)

edt_temps <- list.files(path = Inputs, pattern = "edt_temps", full.names = T) %>% 
  read.csv(.) %>%
  select(-X)

psu_temps <- list.files(path = Inputs, pattern = "psu_temps", full.names = T) %>%
  read.csv(.) %>%
  select(-X)

temp_gaps <- list.files(path = Inputs, pattern = "temp_gaps", full.names = T) %>% 
  read.csv(.) %>%
  select(-notes)
  
# Attribute table of chehalis_obstructs_20181013_NAD83
culvs <- list.files(path = Inputs, pattern = "culvs_gsu_", full.names = T) %>%
  read.csv(.) %>%
  mutate(noaa_culv = as.numeric(row.names(.)),
         FishPass = as.numeric(as.character(FishPass))/100) %>%
  select(noaa_culv, FeatureTyp,FishPass,OBS_UNIQ)


flowline <- list.files(path = Inputs, pattern = "flowline", full.names = T) %>%
  read.csv(.) %>%
  mutate(Reach_low = tolower(Reach),
         Subbasin_num = ifelse(Habitat == "LgRiver", noaa_sub_num, noaa_sub_nofp_num),
         Habitat = case_when(!Reach %in% mainstem_reaches ~ as.character(Habitat),
                             Reach %in% mainstem_reaches & Subbasin_num == 49 ~ "Tidal",
                             Reach %in% mainstem_reaches & !Subbasin_num == 49 ~ "LgRiver")) %>%
  left_join(., edt_temps, by = "Reach") %>%
  left_join(., psu_temps, by = "Seg") %>%
  left_join(., temp_gaps, by = "Reach") %>%
  rename(coho = cohospawn,
         fall_chinook = fallspawn,
         chum = chumspawn,
         spring_chinook = sprspawn,
         steelhead = steelspawn) %>%
  mutate(both_chk = ifelse(fall_chinook == "Yes" & spring_chinook == "Yes",
                           "Yes",
                           "No")) %>%
  gather(species, spawn_dist, coho:steelhead) %>%
  filter(species == fishtype) %>%
  left_join(., edt_width %>%
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
              spread(stage, width),
            by = "Reach_low") %>%
  mutate(mwmt = ifelse(Habitat == "LgRiver",
                       mwmt,
                       NA),
         mdm = ifelse(Habitat == "LgRiver",
                      mdm,
                      NA),
         width_s = ifelse(is.na(width_s), 
                          wet_width, 
                          width_s),
         width_w = ifelse(is.na(width_w),
                          wet_width,
                          width_w),
         area_s = (Shape_Length * width_s) / 10000,
         area_w = (Shape_Length * width_w) / 10000,
         temp_diff_2040 = F2040_temp + cc_2040 - curr_temp,
         temp_diff_2080 = F2080_temp + cc_2080 - curr_temp,
         temp_diff_2040_cc_only = cc_2040,
         temp_diff_2080_cc_only = cc_2080,
         temp_diff = curr_temp - hist_temp,
         curr_temp = ifelse(!is.na(mwmt), 
                            mwmt,
                            ifelse(!is.na(edt_temp), 
                                   edt_temp, 
                                   ifelse(!is.na(gap_temp_mwmt),
                                          gap_temp_mwmt,
                                          -9999))), #set temp to -9999 if no temp exists to help us flag down gaps if we receive new temp data
         hist_temp = curr_temp - temp_diff,
         tm_2040 = curr_temp + temp_diff_2040,
         tm_2080 = curr_temp + temp_diff_2080,
         tm_2040_cc_only = curr_temp + cc_2040,
         tm_2080_cc_only = curr_temp + cc_2080,
         curr.tempmult = temp_func(curr_temp),
         hist.tempmult = temp_func(hist_temp),
         can_ang = ifelse(is.na(can_ang), # We are still working on calculating canopy opening angle in some reaches.  For reaches without canopy opening angle measurements, 
                          0, # we set canopy opening angle to 0 because the majority of these reaches are far upstream and few of them are clearcut.
                          can_ang))

flowline <- flowline %>% 
  select(noaaid,Reach,culv_list) %>%
  filter(culv_list != "") %>%
  mutate(culv_list = str_split(culv_list,',')) %>%
  unnest(culv_list) %>%
  filter(culv_list != 'None') %>%
  mutate(noaa_culv = as.numeric(culv_list)) %>%
  left_join(culvs) %>%
  assign('asrp_culvs_raw', . , envir = .GlobalEnv) %>%
  group_by(noaaid) %>%
  summarize(pass_tot = prod(FishPass),
            pass_tot_natural = prod(ifelse(FeatureTyp == 'Natural',FishPass,1))) %>%
  right_join(flowline) %>%
  replace_na(list(pass_tot = 1, pass_tot_natural = 1)) %>%
  ungroup()

edt_temps <- list.files(path = "Inputs", pattern = "edt_temps", full.names = T) %>% 
  read.csv(.) %>%
  select(-X)

psu_temps <- list.files(path = "Inputs", pattern = "psu_temps", full.names = T) %>%
  read.csv(.) %>%
  select(mdm, Seg)
  
# Attribute table of chehalis_obstructs_20181013_NAD83
culvs <- list.files(path = "Inputs", pattern = "culvs_gsu_", full.names = T) %>%
  read.csv(.)%>%
  mutate(noaa_culv = as.numeric(row.names(.)),
         FishPass = as.numeric(as.character(FishPass))/100) %>%
  select(noaa_culv, FeatureTyp,FishPass,OBS_UNIQ)


flowline <- list.files(path = Inputs, pattern = "flowline", full.names = T) %>% 
  read.csv(.) %>%
  mutate(Subbasin_num = ifelse(Habitat == "LgRiver", noaa_sub_num, noaa_sub_nofp_num),
         MWMT = ifelse(Habitat == "LgRiver", MWMT, NA)) %>%
  rename(psu_temp = MWMT) %>%
  left_join(., edt_temps, by = "Reach") %>%
  left_join(., psu_temps, by = "Seg") %>%
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
  mutate(Area_ha = (Shape_Length * wet_width) / 10000, # 10000 is a conversion factor between hectares and m^2.  10000 m^2 per hectare
         temp_diff_2040 = F2040_temp + cc_2040 - curr_temp,
         temp_diff_2080 = F2080_temp + cc_2080 - curr_temp,
         temp_diff_2040_cc_only = cc_2040,
         temp_diff_2080_cc_only = cc_2080,
         temp_diff = curr_temp - hist_temp,
         curr_temp = ifelse(!is.na(psu_temp), psu_temp,
                            ifelse(!is.na(edt_temp), edt_temp, 18)), # we are using 18° as a filler in reaches with no psu or edt data.  Therefore capacity will not be decreased in these reaches due to temp
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
library(openxlsx)
#### Length per GSU, Subbasin and EDR ----
flowline_edr <- flowline %>%
  left_join(., anadromous_network, by = 'noaaid') %>%
  filter(anadromous_network == 'Yes') %>%
  left_join(., subbasin_names, by = 'Subbasin_num') %>%
  group_by(EcoRegion) %>%
  summarise(length_km = sum(Shape_Length / 1000, na.rm = T))

flowline_subbasin <- flowline %>%
  left_join(., anadromous_network, by = 'noaaid') %>%
  filter(anadromous_network == 'Yes') %>%
  left_join(., read.csv('lcm/data/Subbasin_names.csv'), by = 'Subbasin_num') %>%
  group_by(Subbasin, Subbasin_num) %>%
  summarise(length_km = sum(Shape_Length/1000, na.rm = T))

flowline_gsu <- flowline %>%
  left_join(., anadromous_network, by = 'noaaid') %>%
  filter(anadromous_network == 'Yes') %>%
  group_by(GSU) %>%
  summarize(length_km = sum(Shape_Length / 1000, na.rm = T))

#### Wood spawning area ----
# Before running this script, first run the habitat model up through '14-fp.R'.  Skip '15-capacity_and_productivity.R' and '16-movement.R', and then 
# run '17-lr_spawn_cap.R', and lines 42-74 of the '18-spawn.R' script, which creates 'asrp_spawn_fp', but does stops before area and eggs are calculated

spawn_area_ss <- flowline %>%
  filter(slope < .03,
         Habitat == 'SmStream') %>%
  mutate(scenario = 'low_wood') %>%
  bind_rows(., flowline %>%
              filter(slope < .03,
                     Habitat == 'SmStream') %>%
              mutate(scenario = 'high_wood')) %>%
  mutate(psp = case_when(slope < .01 ~ ifelse(scenario == 'high_wood',
                                              psp_hwls,
                                              psp_lwls),
                         slope >= .01 ~ ifelse(scenario == 'high_wood',
                                               psp_hwhs,
                                               psp_lwhs)),
         spawn_area = (Shape_Length / (width_w * psp)) * width_w * (width_w * .5)) %>%
  select(noaaid, GSU, scenario, spawn_area, Subbasin_num) %>%
  # group_by(GSU ,scenario) %>%
  # summarize(spawn_area = sum(spawn_area, na.rm = T)) %>%
  spread(scenario, spawn_area) 

spawn_area_lr <- lgr_sp_area_asrp %>%
  left_join(., asrp_reach_data %>%
              filter(Scenario_num == 'Wood')) %>%
  left_join(., asrp_culvs %>%
              filter(Scenario_num == 'Wood')) %>%
  mutate(spawn_area = spawn_area * wood_spawn_mult * pass_tot_asrp) %>%
  bind_rows(lgr_sp_area_asrp %>%
              left_join(., asrp_reach_data %>%
                          filter(Scenario_num == 'Current')) %>%
              left_join(., asrp_culvs %>%
                          filter(Scenario_num == 'Current')) %>%
              mutate(spawn_area = spawn_area * pass_tot_asrp)) %>%
  select(noaaid, GSU, Scenario_num, spawn_area, Subbasin_num) %>%
  # group_by(GSU, Scenario_num) %>%
  # summarize(spawn_area = sum(spawn_area, na.rm = T)) %>%
  spread(Scenario_num, spawn_area) %>%
  rename(high_wood = Wood,
         low_wood = Current) 


spawn_area_fp <- asrp_spawn_fp %>%
  filter(Scenario_num %in% c('Current', 'Wood')) %>%
  mutate(spawn_area = ifelse(Scenario_num == 'Current',
                             (Length_sc / (2 * psp_lwls)) * 2 * (2 * .5),
                             (Length_sc / (2 * psp_hwls)) * 2 * (2 * .5))) %>%
  select(noaaid, GSU, Scenario_num, spawn_area, Subbasin_num) %>%
  # group_by(GSU, Scenario_num) %>%
  # summarize(spawn_area = sum(spawn_area, na.rm = T)) %>%
  spread(Scenario_num, spawn_area) %>%
  rename(high_wood = Wood,
         low_wood = Current)

spawn_area_wood <- spawn_area_ss %>%
  bind_rows(spawn_area_lr) %>%
  bind_rows(spawn_area_fp) %>% 
  ungroup()

spawn_area_gsu <- spawn_area_wood %>%
  group_by(GSU) %>%
  summarize(high_wood = round(sum(high_wood, na.rm = T), digits = 2),
            low_wood = round(sum(low_wood, na.rm = T), digits = 2)) %>%
  ungroup() %>%
  mutate(spawn_diff = high_wood - low_wood,
         rank_spawn_diff = rank(spawn_diff, ties.method = 'first')) %>%
  left_join(., flowline_gsu) 

spawn_area_subbasin <- spawn_area_wood %>%
  left_join(., subbasin_names) %>%
  group_by(Subbasin_num, Subbasin, EcoRegion) %>%
  summarize(high_wood = round(sum(high_wood, na.rm = T), digits = 2),
            low_wood = round(sum(low_wood, na.rm = T), digits = 2)) %>%
  ungroup() %>%
  mutate(spawn_diff = high_wood - low_wood,
         rank_spawn_diff = rank(spawn_diff, ties.method = 'first')) %>%
  left_join(., flowline_subbasin)

spawn_area_edr <- spawn_area_wood %>%
  left_join(., subbasin_names) %>%
  group_by(EcoRegion) %>%
  summarize(high_wood = round(sum(high_wood, na.rm = T), digits = 2),
            low_wood = round(sum(low_wood, na.rm = T), digits = 2)) %>%
  ungroup() %>%
  mutate(spawn_diff = high_wood - low_wood,
         rank_spawn_diff = rank(spawn_diff, ties.method = 'first')) %>%
  left_join(., flowline_edr)

#### Shade (average temperature) ----
# Run the habitat model through 10-reach_level_data before this script.

avg_temp_gsu <- asrp_reach_data %>%
  filter(Scenario_num %in% c('Current', 'Shade')) %>%
  select(noaaid, GSU, Scenario_num, asrp_temp) %>%
  spread(Scenario_num, asrp_temp) %>%
  group_by(GSU) %>%
  summarize(Current_temp = round(mean(Current, na.rm = T), digits = 2),
            Shade_temp = round(mean(Shade, na.rm = T), digits = 2)) %>%
  mutate(temp_diff = Current_temp - Shade_temp,
         rank_temp = rank(temp_diff, ties.method = 'first')) %>%
  left_join(., flowline_gsu) 

avg_temp_subbasin <- asrp_reach_data %>%
  filter(Scenario_num %in% c('Current', 'Shade')) %>%
  select(noaaid, Subbasin_num, Scenario_num, asrp_temp) %>%
  spread(Scenario_num, asrp_temp) %>%
  group_by(Subbasin_num) %>%
  summarize(Current_temp = round(mean(Current, na.rm = T), digits = 2),
            Shade_temp = round(mean(Shade, na.rm = T), digits = 2)) %>%
  mutate(temp_diff = Current_temp - Shade_temp,
         rank_temp = rank(temp_diff, ties.method = 'first')) %>%
  left_join(., flowline_subbasin) %>%
  left_join(., subbasin_names %>%
              select(-Area_km2)) 

avg_temp_edr <- asrp_reach_data %>%
  filter(Scenario_num %in% c('Current', 'Shade')) %>%
  left_join(., subbasin_names %>%
              select(Subbasin_num, EcoRegion)) %>%
  select(noaaid, EcoRegion, Scenario_num, asrp_temp) %>%
  spread(Scenario_num, asrp_temp) %>%
  group_by(EcoRegion) %>%
  summarize(Current_temp = round(mean(Current, na.rm = T), digits = 2),
            Shade_temp = round(mean(Shade, na.rm = T), digits = 2)) %>%
  mutate(temp_diff = Current_temp - Shade_temp,
         rank_temp = rank(temp_diff, ties.method = 'first')) %>%
  left_join(., flowline_edr) 

#### Floodplain ----
fp_areas <- asrp_fp %>%
  filter(Scenario_num %in% c('Current', 'Floodplain')) %>%
  select(Scenario_num, Subbasin_num, GSU, Area) %>%
  distinct() 
  

fp_areas_gsu <- fp_areas %>%
  group_by(GSU, Scenario_num) %>%
  summarize(Area = sum(Area, na.rm = T)) %>%
  spread(Scenario_num, Area) %>%
  summarize(curr_area_fp = sum(Current, na.rm = T),
            hist_area_fp = sum(Floodplain, na.rm = T)) %>%
  mutate(fp_area_diff = hist_area_fp - curr_area_fp,
         rank_fp = rank(fp_area_diff, ties.method = 'first'))

fp_areas_subbasin <- fp_areas %>%
  group_by(Subbasin_num, Scenario_num) %>%
  summarize(Area = sum(Area, na.rm = T)) %>%
  spread(Scenario_num, Area) %>%
  summarize(curr_area_fp = sum(Current, na.rm = T),
            hist_area_fp = sum(Floodplain, na.rm = T)) %>%
  mutate(fp_area_diff = hist_area_fp - curr_area_fp,
         rank_fp = rank(fp_area_diff, ties.method = 'first')) %>%
  left_join(., subbasin_names %>%
              select(Subbasin_num, Subbasin, EcoRegion))

fp_areas_edr <- fp_areas %>%
  left_join(., subbasin_names %>%
              select(Subbasin_num, EcoRegion)) %>%
  group_by(EcoRegion, Scenario_num) %>%
  summarize(Area = sum(Area, na.rm = T)) %>%
  spread(Scenario_num, Area) %>%
  summarize(curr_area_fp = sum(Current, na.rm = T),
            hist_area_fp = sum(Floodplain, na.rm = T)) %>%
  mutate(fp_area_diff = hist_area_fp - curr_area_fp,
         rank_fp = rank(fp_area_diff, ties.method = 'first'))

# Barriers ----

pass = asrp_culvs %>%
  filter(Scenario_num %in% c('Current', 'Barriers')) %>%
  left_join(., asrp_reach_data %>%
              select(noaaid, GSU, Subbasin_num)) %>%
  replace_na(list(passs_tot = 1, pass_tot_natural = 1))

gsu_pass <- pass %>%
  group_by(GSU, Scenario_num) %>%
  summarize(pass_tot = mean(pass_tot_asrp, na.rm = T)) %>%
  spread(Scenario_num, pass_tot) %>%
  rename(pass_tot_curr = Current,
         pass_tot_hist = Barriers) %>%
  mutate(pass_diff = pass_tot_hist - pass_tot_curr,
         pass_diff = ifelse(is.na(pass_diff), 0, pass_diff),
         pass_diff = ifelse(GSU == "Upper Skookumchuck", NA, pass_diff)) %>%
  ungroup() %>%
  mutate(rank_pass = rank(pass_diff, ties.method = 'first'))

subbasin_pass <- pass %>%
  group_by(Subbasin_num, Scenario_num) %>%
  summarize(pass_tot = mean(pass_tot_asrp, na.rm = T)) %>%
  spread(Scenario_num, pass_tot) %>%
  left_join(., subbasin_names %>%
              select(Subbasin_num, Subbasin, EcoRegion)) %>%
  rename(pass_tot_curr = Current,
         pass_tot_hist = Barriers) %>%
  mutate(pass_diff = pass_tot_hist - pass_tot_curr,
        pass_diff = ifelse(is.na(pass_diff), 0, pass_diff),
        pass_diff = ifelse(Subbasin == "Upper Skookumchuck", NA, pass_diff)) %>%
  ungroup() %>%
  mutate(rank_pass = rank(pass_diff, ties.method = 'first'))

edr_pass <- pass %>%
  left_join(., subbasin_names %>%
              select(EcoRegion, Subbasin_num)) %>%
  group_by(EcoRegion, Scenario_num) %>%
  summarize(pass_tot = mean(pass_tot_asrp, na.rm = T)) %>%
  spread(Scenario_num, pass_tot) %>%
  mutate(pass_tot_hist = Barriers,
         pass_tot_curr = Current,
         pass_diff = pass_tot_hist - pass_tot_curr,
         pass_diff = ifelse(is.na(pass_diff), 0, pass_diff),
         pass_diff = ifelse(EcoRegion == "Upper Skookumchuck", NA, pass_diff)) %>%
  ungroup() %>%
  mutate(rank_pass = rank(pass_diff, ties.method = 'first'))


#### BIP ----
bip <- flowline %>%
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
         score = score_slope + score_width) 
  #select(GSU, noaaid, slope, BF_width, score_slope, score_width, score)
  
bip_gsu <- bip %>%
  group_by(GSU) %>%
  summarize(BIP = mean(score, na.rm = TRUE)) %>%
  mutate(rank_bip = rank(BIP, ties.method = 'first'))

bip_subbasin <- bip %>%
  group_by(Subbasin_num) %>%
  summarize(BIP = mean(score, na.rm = TRUE)) %>%
  mutate(rank_bip = rank(BIP, ties.method = 'first'))

bip_edr <- bip %>%
  left_join(., subbasin_names %>%
              select(Subbasin_num, EcoRegion)) %>%
  group_by(EcoRegion) %>%
  summarize(BIP = mean(score, na.rm = TRUE)) %>%
  mutate(rank_bip = rank(BIP, ties.method = 'first'))

#### Fine sed ----

basinwide_sed <- flowline %>%
  summarize(sed_current = mean(sed_current,na.rm = TRUE),
            sed_hist = mean(sed_hist, na.rm = TRUE))
gsu_sed <- flowline %>%
  group_by(GSU) %>%
  summarize(sed_current = mean(sed_current, na.rm = TRUE),
            sed_hist = mean(sed_hist, na.rm = TRUE)) %>%
  mutate(sed_diff = abs(sed_hist - sed_current),
         rank_sed = rank(sed_diff, ties.method = 'first')) 

subbasin_sed <- flowline %>%
  group_by(Subbasin_num) %>%
  summarize(sed_current = mean(sed_current, na.rm = TRUE),
            sed_hist = mean(sed_hist, na.rm = TRUE)) %>%
  mutate(sed_diff = abs(sed_hist - sed_current),
         rank_sed = rank(sed_diff, ties.method = 'first')) %>%
  left_join(., subbasin_names %>%
              select(Subbasin_num, Subbasin, EcoRegion))

edr_sed <- flowline %>%
  left_join(., subbasin_names %>%
              select(Subbasin_num, EcoRegion)) %>%
  group_by(EcoRegion) %>%
  summarize(sed_current = mean(sed_current, na.rm = TRUE),
            sed_hist = mean(sed_hist, na.rm = TRUE)) %>%
  mutate(sed_diff = abs(sed_hist - sed_current),
         rank_sed = rank(sed_diff, ties.method = 'first')) 



#### LR bank armoring ----
bank_raw <- list.files(path = file.path(Inputs, "spatial_model_outputs"), pattern = "LgRiver", full.names = T) %>% 
  read.csv(.) %>%
  mutate(source_hab = "LgRiver",
         Habitat = case_when(HabUnit %in% c("Bank", "Bank-TM") ~ "Bank",
                             HabUnit %in% c("Bank HM", "Bank HM-TM") ~ "HM_Bank",
                             HabUnit %in% c("Bar-boulder", "Bar-boulder-TM") ~ "Bar_boulder",
                             HabUnit %in% c("Bar-gravel", "Bar-gravel-TM") ~ "Bar_gravel",
                             HabUnit %in% c("Bar-sand", "Bar-sand-TM") ~ "Bar_sand"),
         Period = ifelse(Period == " Hist", 
                         "Hist", 
                         as.character(Period))) %>%
  select(Length_m, Period, noaaid, Habitat) %>%
  filter(Habitat %in% c('Bank', 'HM_Bank'))

bank <- bank_raw %>%
  filter(Period != 'Curr') %>%
  mutate(Period = ifelse(Period == 'Both',
                         'Hist',
                         Period)) %>%
  bind_rows(bank_raw %>%
              filter(Period != 'Hist') %>%
              mutate(Period = ifelse(Period == 'Both',
                                     'Curr',
                                     Period))) %>%
  group_by(Period, noaaid, Habitat) %>%
  summarize(Length_m = sum(Length_m, na.rm = T)) %>%
  spread(Period, Length_m)
  
  
  

#### Create excel workbook ----
hab_attributes_gsu = spawn_area_gsu %>%
  left_join(., avg_temp_gsu) %>%
  left_join(., fp_areas_gsu) %>%
  left_join(., gsu_pass) %>%
  left_join(., bip_gsu) %>%
  left_join(., gsu_sed) %>%
  select(GSU, length_km, BIP, rank_bip, pass_tot_curr, pass_tot_hist, pass_diff, rank_pass, curr_area_fp, hist_area_fp, fp_area_diff, rank_fp, Current_temp, Shade_temp, temp_diff, rank_temp, low_wood, high_wood, spawn_diff, rank_spawn_diff, 
         sed_current, sed_hist, sed_diff, rank_sed)

colnames(hab_attributes_gsu) <- c('GSU', 'Total Km', 'Beaver intrinsic potential', 'Rank (beaver intrinsic potential)', 'Percent passage (current)', 'Percent passage (historical)', 'Restoration potential (percent passage)', 
                              'Rank (passage restoration potential)', 'Floodplain area (current)', 'Floodplain area (historical)',
                              'Floodplain restoration potential (area, Ha)', 'Rank (floodplain restoration potential)', 'Average temperature (Current, °C)','Average temperature (Historical, °C)',
                              'Shade restoration potential (average temperature, °C)', 'Rank (shade restoration potential)', 'Spawning area (low wood, m^2)','Spawning area (high wood, m^2)',
                              'Spawning gravel restoration potential ( area, m^2)', 'Rank (spawning gravel restoration potential)','Percent fine sediment (current)', 'Percent fine sediment (historical)', 
                              'Restoration potential (percent fine sediment', 'Rank (fine sediment restoraion potential)')

hab_attributes_subbasin = spawn_area_subbasin %>%
  left_join(., avg_temp_subbasin) %>%
  left_join(., fp_areas_subbasin) %>%
  left_join(., subbasin_pass) %>%
  left_join(., bip_subbasin) %>%
  left_join(., subbasin_sed) %>%
  select(Subbasin, Subbasin_num, EcoRegion, length_km, BIP, rank_bip, pass_tot_curr, pass_tot_hist, pass_diff, rank_pass, curr_area_fp, hist_area_fp, fp_area_diff, rank_fp, Current_temp, Shade_temp, temp_diff, rank_temp, low_wood, high_wood, spawn_diff, rank_spawn_diff, 
         sed_current, sed_hist, sed_diff, rank_sed)

colnames(hab_attributes_subbasin) <- c('Subbasin', 'Subbasin_num', 'EcoRegion', 'Total Km', 'Beaver intrinsic potential', 'Rank (beaver intrinsic potential)', 'Percent passage (current)', 'Percent passage (historical)', 'Restoration potential (percent passage)', 
                                       'Rank (passage restoration potential)', 'Floodplain area (current)', 'Floodplain area (historical)',
                                       'Floodplain restoration potential (area, Ha)', 'Rank (floodplain restoration potential)', 'Average temperature (Current, °C)','Average temperature (Historical, °C)',
                                       'Shade restoration potential (average temperature, °C)', 'Rank (shade restoration potential)', 'Spawning area (low wood, m^2)','Spawning area (high wood, m^2)',
                                       'Spawning gravel restoration potential ( area, m^2)', 'Rank (spawning gravel restoration potential)','Percent fine sediment (current)', 'Percent fine sediment (historical)', 
                                       'Restoration potential (percent fine sediment', 'Rank (fine sediment restoraion potential)')

hab_attributes_edr = spawn_area_edr %>%
  left_join(., avg_temp_edr) %>%
  left_join(., fp_areas_edr) %>%
  left_join(., edr_pass) %>%
  left_join(., bip_edr) %>%
  left_join(., edr_sed) %>%
  select(EcoRegion, length_km, BIP, rank_bip, pass_tot_curr, pass_tot_hist, pass_diff, rank_pass, curr_area_fp, hist_area_fp, fp_area_diff, rank_fp, Current_temp, Shade_temp, temp_diff, rank_temp, low_wood, high_wood, spawn_diff, rank_spawn_diff, 
         sed_current, sed_hist, sed_diff, rank_sed)

colnames(hab_attributes_edr) <- c('EcoRegion', 'Total Km', 'Beaver intrinsic potential', 'Rank (beaver intrinsic potential)', 'Percent passage (current)', 'Percent passage (historical)', 'Restoration potential (percent passage)', 
                                  'Rank (passage restoration potential)', 'Floodplain area (current)', 'Floodplain area (historical)',
                                  'Floodplain restoration potential (area, Ha)', 'Rank (floodplain restoration potential)', 'Average temperature (Current, °C)','Average temperature (Historical, °C)',
                                  'Shade restoration potential (average temperature, °C)', 'Rank (shade restoration potential)', 'Spawning area (low wood, m^2)','Spawning area (high wood, m^2)',
                                  'Spawning gravel restoration potential ( area, m^2)', 'Rank (spawning gravel restoration potential)','Percent fine sediment (current)', 'Percent fine sediment (historical)', 
                                  'Restoration potential (percent fine sediment', 'Rank (fine sediment restoraion potential)')

wb_hab <- loadWorkbook('srt_figures/hab_attributes_template.xlsx')

writeData(wb_hab, sheet = 1, hab_attributes_gsu)
writeData(wb_hab, sheet = 2, hab_attributes_subbasin)
writeData(wb_hab, sheet = 3, hab_attributes_edr)

saveWorkbook(wb_hab, 'srt_figures/hab_attributes_workbook.xlsx', overwrite = TRUE)
  
  
  
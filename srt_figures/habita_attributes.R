#### Length per GSU ----
flowline_gsu <- flowline %>%
  left_join(., anadromous_network, by = 'noaaid') %>%
  filter(anadromous_network == 'Yes') %>%
  group_by(GSU) %>%
  summarize(length_km = sum(Shape_Length / 1000, na.rm = T))

#### Wood spawning area ----
# Before running this script, first run the habitat model up through '14-fp.R'.  Skip '15-capacity_and_productivity.R' and '16-movement.R', and then 
# run '17-lr_spawn_cap.R', and lines 42-74 of the '18-spawn.R' script, which creates 'asrp_spawn_fp', but does stops before area and eggs are calculated

# Calculate large river spawning area
# lr_spawn <- lgr_sp_area_asrp %>%
#   left_join(., asrp_culvs %>%
#               filter(Scenario_num == 'Wood')) %>%
#   left_join(., asrp_reach_data %>% filter(Scenario_num == 'Wood'))

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
  left_join(., subbasin_names) %>%
  group_by(GSU, Subbasin_num,scenario) %>%
  summarize(spawn_area = sum(spawn_area, na.rm = T)) %>%
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
  group_by(GSU, Subbasin_num, Scenario_num) %>%
  summarize(spawn_area = sum(spawn_area, na.rm = T)) %>%
  spread(Scenario_num, spawn_area) %>%
  rename(high_wood = Wood,
         low_wood = Current) 


spawn_area_fp <- asrp_spawn_fp %>%
  filter(Scenario_num %in% c('Current', 'Wood')) %>%
  mutate(spawn_area = ifelse(Scenario_num == 'Current',
                             (Length_sc / (2 * psp_lwls)) * 2 * (2 * .5),
                             (Length_sc / (2 * psp_hwls)) * 2 * (2 * .5))) %>%
  group_by(GSU, Subbasin_num, Scenario_num) %>%
  summarize(spawn_area = sum(spawn_area, na.rm = T)) %>%
  spread(Scenario_num, spawn_area) %>%
  rename(high_wood = Wood,
         low_wood = Current)

spawn_area_wood <- spawn_area_ss %>%
  bind_rows(spawn_area_lr) %>%
  bind_rows(spawn_area_fp) %>%
  group_by(GSU, Subbasin_num) %>%
  summarize(high_wood = round(sum(high_wood, na.rm = T), digits = 2),
            low_wood = round(sum(low_wood, na.rm = T), digits = 2)) %>%
  ungroup() %>%
  mutate(spawn_diff = high_wood - low_wood,
         # perc = diff / high_wood * 100,
         # perc = ifelse(is.na(perc), 
         #               0,
         #               as.numeric(perc))
         ) %>%
  left_join(., subbasin_names %>%
              select(-Area_km2), by = 'Subbasin_num') %>%
  left_join(., flowline_gsu) 

#### Shade (average temperature) ----
# Run the habitat model through 10-reach_level_data before this script.

avg_temp <- asrp_reach_data %>%
  filter(Scenario_num %in% c('Current', 'Shade')) %>%
  select(noaaid, GSU, Scenario_num, asrp_temp, Subbasin_num) %>%
  spread(Scenario_num, asrp_temp) %>%
  group_by(GSU, Subbasin_num) %>%
  summarize(Current_temp = round(mean(Current, na.rm = T), digits = 2),
            Shade_temp = round(mean(Shade, na.rm = T), digits = 2)) %>%
  mutate(temp_diff = Current_temp - Shade_temp) %>%
  left_join(., subbasin_names %>%
              select(-Area_km2)) %>%
  left_join(., flowline_gsu) 

#### Floodplain ----
raw_floodplain <- list.files(path = file.path(Inputs, "spatial_model_outputs"), pattern = "Floodplain", full.names = T) %>% 
  read.csv(.) %>%
  select(Area_ha, Period, noaaid)
  

fp_areas <- raw_floodplain %>%
  filter(Period %in% c('Hist', 'Both')) %>%
  mutate(Period = case_when(
    Period == 'Both' ~ 'Hist',
    Period == 'Hist' ~ 'Hist')) %>%
  bind_rows(., raw_floodplain %>%
              filter(Period %in% c('Curr', 'Both')) %>%
              mutate(Period = case_when(
                Period == 'Both' ~ 'Curr',
                Period == 'Curr' ~ 'Curr'))) %>% 
  group_by(Period, noaaid) %>%
  summarize(Area_ha = sum(Area_ha, na.rm = T)) %>%
  ungroup() %>%
  spread(Period, Area_ha) %>%
  left_join(., flowline %>%
              select(noaaid, Subbasin_num, GSU)) %>%
  left_join(., anadromous_network) %>%
  filter(anadromous_network == 'Yes') %>%
  left_join(., subbasin_names) %>%
  group_by(Subbasin_num, GSU, Subbasin, EcoRegion) %>%
  summarize(curr_area_fp = sum(Curr, na.rm = T),
            hist_area_fp = sum(Hist, na.rm = T)) %>%
  mutate(fp_area_diff = hist_area_fp - curr_area_fp)
  



#### Create excel workbook ----
hab_attributes = spawn_area_wood %>%
  left_join(., avg_temp) %>%
  left_join(., fp_areas) %>%
  select(GSU, Subbasin, Subbasin_num, EcoRegion, length_km, curr_area_fp, hist_area_fp, fp_area_diff, Current_temp, Shade_temp, temp_diff, low_wood, high_wood, spawn_diff)

colnames(hab_attributes) <- c('GSU', 'Subbasin', 'Subbasin_num', 'EcoRegion', 'Total Km', 'Floodplain area (current)', 'Floodplain area (historical)',
                              'Floodplain restoration potential (area, Ha)', 'Average temperature (Current, °C)','Average temperature (Historical, °C)',
                              'Shade restoration potential (average temperature, °C)', 'Spawning area (low wood, m^2)','Spawning area (high wood, m^2)',
                              'Spawning gravel restoration potential ( area, m^2)')

wb_hab <- loadWorkbook('srt_figures/hab_attributes_template.xlsx')

writeData(wb_hab, sheet = 1, hab_attributes)

saveWorkbook(wb_hab, 'srt_figures/hab_attributes_workbook.xlsx', overwrite = TRUE)
  
  
  
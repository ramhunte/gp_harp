flowline_temps <- asrp_reach_data %>%
  filter(Scenario_num == 'cc_only') %>%
  select(noaaid, year, asrp_temp) %>%
  spread(year, asrp_temp) %>%
  rename(cc_only_mid = '2040',
         cc_only_late = '2080') %>%
  left_join(., asrp_reach_data %>%
              filter(Scenario_num == 'rip_and_climate') %>%
              select(noaaid, year, asrp_temp) %>%
              spread(year, asrp_temp) %>%
              rename(rip_mid = '2040',
                     rip_late = '2080')) %>%
  left_join(., asrp_reach_data %>%
              filter(Scenario_num == 'fp_temp') %>%
              select(noaaid, year, asrp_temp) %>%
              spread(year, asrp_temp) %>%
              rename(fp_mid = '2040',
                     fp_late = '2080')) %>%
  left_join(., asrp_reach_data %>%
              filter(Scenario_num == 'rip_and_flp') %>%
              select(noaaid, year, asrp_temp) %>%
              spread(year, asrp_temp) %>%
              rename(rip_flp_mid = '2040',
                     rip_flp_late = '2080')) %>% 
  left_join(., asrp_reach_data %>%
              filter(Scenario_num == 'Current') %>%
              select(noaaid, year, asrp_temp) %>%
              spread(year, asrp_temp) %>%
              rename(current = '2019')) %>%
  left_join(., asrp_reach_data %>%
              filter(Scenario_num == 'Historical') %>%
              select(noaaid, year, asrp_temp) %>%
              spread(year, asrp_temp) %>%
              rename(historical = '2019'))
  write.csv(flowline_temps,  'temp_paper/flowline_temps.csv')


View(flowline_temps %>% filter(cc_only_late <18))
View(flowline_temps %>% filter(cc_only_late >18 & cc_only_mid < 24))
View(flowline_temps %>% filter(cc_only_late >24))

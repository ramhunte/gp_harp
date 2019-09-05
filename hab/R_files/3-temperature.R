# EDT Temperatures ----

# Create csv with single 'clean' edt layer ----

edt_lyr <- read.csv("hab/Inputs/temperature_inputs/DailyData_Summarized_14Sept2018_Sheet4.csv") %>%
  rbind(read.csv("hab/Inputs/temperature_inputs/DailyData_Summarized_14Sept2018_Sheet1.csv")) %>%
  mutate(year = year(mdy(DATE)),
         month = month(mdy(DATE)),
         week = week(mdy(DATE)),
         day = day(mdy(DATE)))

scatter_reaches <- c('Scatter Cr-1', 'Scatter Cr-2', 'Scatter Cr-3', 'Trib 0719-1', 'RB Trib 2282 (Trib 0719)-1', 'Trib 0720-1', 'Trib 0718-1',
                     'LB Trib 2281 (Scatter)-1', 'Trib 0721-1', 'LB Trib 2283 (Trib 0719)-1', 'LB Trib 2279 (Scatter)-1', 
                     'RB Trib 2280 (LB Trib 2279)-1', 'RB Trib 2278 (Scatter)-1')

# Calculate mean weekly maximum temperatures to be used in rearing life stages ----

edt_mwmt_lyr <- edt_lyr %>%
  mutate(species = fishtype) %>%
  filter(ifelse(species == 'spring_chinook',
                month == 6,
                month == 8)) %>%
  group_by(Reach, year) %>%
  mutate(temp = zoo::rollmean(Temperature, 7, na.pad = TRUE, align = "right")) %>%
  summarize(edt_temp = max(temp, na.rm = T)) %>%
  ungroup() %>%
  mutate(year = paste0("y.", year)) %>%
  spread(year, edt_temp) %>%
  mutate(edt_temp = ifelse(Reach %in% scatter_reaches, y.1994, 
                           ifelse(!is.na(y.1902), y.1902,
                                  ifelse(!is.na(y.2013), y.2013, y.1993))))

proxy_mwmt <- read.csv("hab/Inputs/temperature_inputs/TemperatureProxies_Chehalis.csv")
colnames(proxy_mwmt) <- c('proxy', 'Reach')
proxy_mwmt <- proxy_mwmt %>%
  mutate(Reach = trimws(Reach)) %>%
  left_join(., edt_mwmt_lyr) %>%
  select(proxy, edt_temp) %>%
  rename(Reach = proxy,
         edt_proxy_mwmt = edt_temp)

edt_mwmt_lyr <- edt_mwmt_lyr %>%
  full_join(., proxy_mwmt) %>%
  mutate(edt_temp = ifelse(is.na(edt_temp), edt_proxy_mwmt, edt_temp))


# Calculate mean of daily maxima to be used in prespawn survival ----

edt_mdm_lyr <- edt_lyr %>%
  filter(month %in% c(7, 8)) %>%
  group_by(year,Reach) %>%
  summarize(edt_temp = mean(Temperature, na.rm = T)) %>% 
  ungroup() %>%
  mutate(year = paste0("y.", year)) %>%
  spread(year,edt_temp) %>%
  mutate(edt_mdm_temp = ifelse(Reach %in% scatter_reaches, y.1994, 
                               ifelse(!is.na(y.1902), y.1902,
                                      ifelse(!is.na(y.2013), y.2013, y.1993))))

proxy_mdm <- read.csv("hab/Inputs/temperature_inputs/TemperatureProxies_Chehalis.csv")
colnames(proxy_mdm) <- c('proxy', 'Reach')
proxy_mdm <- proxy_mdm %>%
  mutate(Reach = trimws(Reach)) %>%
  left_join(., edt_mdm_lyr) %>%
  select(proxy, edt_mdm_temp) %>%
  rename(Reach = proxy,
         edt_proxy_mdm = edt_mdm_temp)

edt_mdm_lyr <- edt_mdm_lyr %>%
  full_join(., proxy_mdm) %>%
  mutate(edt_mdm_temp = ifelse(is.na(edt_mdm_temp), edt_proxy_mdm, edt_mdm_temp))


#Calculate the mean of daily maxima from June 1 - June 21
  # Used for chinook rearing temperatures
edt_mdm_lyr_june <- edt_lyr %>%
  filter(month == 6,
         day %in% 1:21) %>%
  group_by(year,Reach) %>%
  summarize(edt_temp = mean(Temperature, na.rm = T)) %>% 
  ungroup() %>%
  mutate(year = paste0("y.", year)) %>%
  spread(year,edt_temp) %>%
  mutate(edt_mdm_temp_june = ifelse(Reach %in% scatter_reaches, y.1994, 
                                    ifelse(!is.na(y.1902), y.1902,
                                           ifelse(!is.na(y.2013), y.2013, y.1993)))) %>%
  select(Reach, edt_mdm_temp_june)


# Join all EDT temperature metrics together
edt_temps <- edt_mwmt_lyr %>%
  select(Reach, edt_temp) %>%
  left_join(., edt_mdm_lyr %>%
              select(Reach, edt_mdm_temp), by = "Reach") %>%
  left_join(edt_mdm_lyr_june, by = "Reach") %>%
  mutate(
    rearing_temp_edt = case_when(
      fishtype %in% c('spring_chinook', 'fall_chinook') ~ edt_mdm_temp_june,
      fishtype %in% c('coho', 'steelhead') ~ edt_temp),
    prespawn_temp_edt  = edt_mdm_temp)

# write.csv(edt_lyr, 'hab/Inputs/edt_temps.csv')


# PSU temperatures ----

# Calculate mean weekly maximum temperature to be used in rearing life stages ----

psu_temps_mwmt <- read.csv("hab/Inputs/temperature_inputs/PSU_Modeled_Temperatures_current.csv") %>%
  separate(col = JDAY, c("month", "day"), sep = "/") %>%
  gather(reach_pt, Temperature, X1:X322) %>%
  mutate(species = fishtype) %>%
  filter(month == 8,
         Temperature > 0) %>%
  group_by(year,reach_pt) %>%
  mutate(mwmt = zoo::rollmean(Temperature, 7, na.pad = TRUE, align = "right")) %>%
  group_by(year,reach_pt) %>%
  summarize(mwmt = max(mwmt, na.rm = T)) %>%
  group_by(reach_pt) %>%
  summarize(mwmt = max(mwmt, na.rm = T))

psu_temps_mean_daily_max <- read.csv("hab/Inputs/temperature_inputs/PSU_Modeled_Temperatures_current.csv") %>%
  separate(col = JDAY, c("month", "day"), sep = "/") %>%
  gather(reach_pt, Temperature, X1:X322) %>%
  filter(month %in% c(7, 8),
         Temperature > 0) %>%
  group_by(reach_pt) %>%
  summarize(mdm = mean(Temperature, na.rm = T))

# June 1-21 mean daily maximum used for chinook downstream migration rearing
psu_temps_mean_daily_max_june <- read.csv("hab/Inputs/temperature_inputs/PSU_Modeled_Temperatures_current.csv") %>%
  separate(col = JDAY, c("month", "day"), sep = "/") %>%
  gather(reach_pt, Temperature, X1:X322) %>%
  filter(month == 6,
         day %in% 1:21,
         Temperature > 0) %>%
  group_by(reach_pt) %>%
  summarize(mdm_june = mean(Temperature, na.rm = T))


psu_temps <- psu_temps_mwmt %>%
  left_join(., psu_temps_mean_daily_max, by = "reach_pt") %>%
  left_join(., psu_temps_mean_daily_max_june, by = "reach_pt") %>%
  mutate(Seg = as.numeric(sub('.', '', reach_pt))) %>%
  select(-reach_pt) %>%
  mutate(
    rearing_temp_psu = case_when(
      fishtype %in% c('spring_chinook', 'fall_chinook') ~ mdm_june,
      fishtype %in% c('coho', 'steelhead') ~ mwmt),
    prespawn_temp_psu  = mdm)

# write.csv(psu_lyr, 'hab/Inputs/psu_temps.csv')

# Create temperature data frame and fill data gaps ----

# Create single temperature dataframe by combining edt_temps and psu_temps dataframes.  Where psu temperature data exist, we use the psu temperatures.
# Elsewhere we use EDT temperatures.  Where no PSU or EDT temperatures exist we use temperatures from analog reaches:
# For mainstem reaches below PSU data points (Chehalis-27, Chehalis-28), we use the mean of the reaches upstream and downstream of those reaches.  
# For mainstem reaches in the upper mainstem (Chehalis-86 through Chehalis-93), we sequentially move from the temperature below these reaches 
#(Chehalis-86) to the temperature above these reaches (Chehalis-94)
# For reaches that are outside of the mainstem, we use a nearby analog reach that share similar characteristics including canopy, landcover, and river size
# Black River-8 does not have a temperature gap analog because it is a lake and will not be consiidered in calculation here


all_temps <- flowline %>%
  select(noaaid, Reach, Seg, Habitat) %>%
  left_join(., edt_temps, by = 'Reach') %>%
  left_join(., psu_temps, by = 'Seg') %>%
  mutate(
    rearing_temp_psu = ifelse(Habitat == 'LgRiver',
                              rearing_temp_psu,
                              NA),
    prespawn_temp_psu = ifelse(Habitat == 'LgRiver',
                               prespawn_temp_psu,
                               NA),
    rear_temp = case_when(
      !is.na(rearing_temp_psu) ~ rearing_temp_psu,
      is.na(rearing_temp_psu) & !is.na(rearing_temp_edt) ~ rearing_temp_edt),
    prespawn_temp = case_when(
      !is.na(prespawn_temp_psu) ~ prespawn_temp_psu,
      is.na(prespawn_temp_psu) & !is.na(prespawn_temp_edt) ~ prespawn_temp_edt)
    ) %>%
  gather(type, temp, c(rear_temp, prespawn_temp)) 
temp_type <- c('rear_temp', 'prespawn_temp')
all_temps <- lapply(temp_type, function(t){
  all_temps %>% 
    filter(type == t) %>%
    mutate(
      temp = case_when(
        !is.na(temp) ~ temp,
        is.na(temp) ~ case_when(
          Reach %in% c('Chehalis-27', 'Chehalis-28') ~ 
            (mean(temp[Reach == 'Chehalis-26'], na.rm = T) + mean(temp[Reach == 'Chehalis-28'], na.rm = T)) /2, 
          Reach  == 'Chehalis-86'~ mean(temp[Reach == 'Chehalis-86'], na.rm = T) - 
            (mean(temp[Reach == 'Chehalis-86'], na.rm = T)- mean(temp[Reach == 'Chehalis-94'], na.rm = T)) * 0/7, 
          Reach  == 'Chehalis-87'~ mean(temp[Reach == 'Chehalis-86'], na.rm = T) -  
            (mean(temp[Reach == 'Chehalis-86'], na.rm = T)- mean(temp[Reach == 'Chehalis-94'], na.rm = T)) * 1/7,
          Reach  == 'Chehalis-88'~ mean(temp[Reach == 'Chehalis-86'], na.rm = T) - 
            (mean(temp[Reach == 'Chehalis-86'], na.rm = T)- mean(temp[Reach == 'Chehalis-94'], na.rm = T)) * 2/7,
          Reach  == 'Chehalis-89'~ mean(temp[Reach == 'Chehalis-86'], na.rm = T) - 
            (mean(temp[Reach == 'Chehalis-86'], na.rm = T)- mean(temp[Reach == 'Chehalis-94'], na.rm = T)) * 3/7,
          Reach  == 'Chehalis-90'~ mean(temp[Reach == 'Chehalis-86'], na.rm = T) - 
            (mean(temp[Reach == 'Chehalis-86'], na.rm = T)- mean(temp[Reach == 'Chehalis-94'], na.rm = T)) * 4/7,
          Reach  == 'Chehalis-91'~ mean(temp[Reach == 'Chehalis-86'], na.rm = T) - 
            (mean(temp[Reach == 'Chehalis-86'], na.rm = T)- mean(temp[Reach == 'Chehalis-94'], na.rm = T)) * 5/7,
          Reach  == 'Chehalis-92'~ mean(temp[Reach == 'Chehalis-86'], na.rm = T) - 
            (mean(temp[Reach == 'Chehalis-86'], na.rm = T)- mean(temp[Reach == 'Chehalis-94'], na.rm = T)) * 6/7,
          Reach  == 'Chehalis-93'~ mean(temp[Reach == 'Chehalis-86'], na.rm = T) - 
            (mean(temp[Reach == 'Chehalis-86'], na.rm = T)- mean(temp[Reach == 'Chehalis-94'], na.rm = T)) * 7/7,
          Reach == 'Drop Cr-1' ~ mean(temp[Reach == 'Skookumchuck-16'], na.rm = T),
          Reach == 'Hanaford-8' ~ mean(temp[Reach == 'RB Trib 2327 (Hanaford)-1'], na.rm = T),
          Reach == 'LB Trib 0209 (EF Wishkah)-2' ~ mean(temp[Reach == 'LB Trib 0209 (EF Wishkah)-1'], na.rm = T),
          Reach == 'LB Trib 0209 (EF Wishkah)-3' ~ mean(temp[Reach == 'LB Trib 0209 (EF Wishkah)-4'], na.rm = T),
          Reach == 'RB Trib 0040 (Hump)-1' ~ mean(temp[Reach == 'RB Trib 0041 (Hump)-1'], na.rm = T),
          Reach == 'RB Trib 0891-1' ~ mean(temp[Reach == 'LB Trib 2359 (RB Trib 0890)_1'], na.rm = T),
          Reach == 'RB Trib 2025 (LB Trib 2023)-1' ~ mean(temp[Reach == 'LB Trib 2023 (Hump)-1'], na.rm = T),
          Reach == 'RB Trib 2061 (Hump)-1' ~ mean(temp[Reach == 'Chester-1'], na.rm = T),
          Reach == 'RB Trib 2295 (Skookumchuck)-1' ~ mean(temp[Reach == 'Hanaford-1'], na.rm = T),
          Reach == 'Shaw (Bunker)' ~ mean(temp[Reach == 'RB Trib 0968-1'], na.rm = T),
          Reach == 'Shaw-3' ~ mean(temp[Reach == 'Shaw-2'], na.rm = T),
          Reach == 'Chehalis-39' ~ mean(temp[Reach == 'Chehalis-39'], na.rm = T),
          Reach == 'Chehalis-62' ~ mean(temp[Reach == 'Chehalis-62'], na.rm = T),
          Reach == 'Black River-8' ~ 15
        )
      )
    )
    
}) %>%
  do.call('rbind',.) %>%
  spread(type, temp) %>%
  select(noaaid, prespawn_temp, rear_temp)
  

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
  filter(month %in% c(7, 8, 9)) %>%
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

edt_temps <- edt_mwmt_lyr %>%
  select(Reach, edt_temp) %>%
  left_join(., edt_mdm_lyr %>%
              select(Reach, edt_mdm_temp), by = "Reach")

# write.csv(edt_lyr, 'hab/Inputs/edt_temps.csv')

# PSU temperatures ----

# Calculate mean weekly maximum temperature to be used in rearing life stages ----

psu_temps_mwmt <- read.csv("hab/Inputs/temperature_inputs/PSU_Modeled_Temperatures_current.csv") %>%
  separate(col = JDAY, c("month", "day"), sep = "/") %>%
  gather(reach_pt, Temperature, X1:X322) %>%
  mutate(species = fishtype) %>%
  filter(ifelse(species == 'spring_chinook',
                month == 6,
                month == 8),
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
  filter(month %in% c(7, 8, 9),
         Temperature > 0) %>%
  group_by(reach_pt) %>%
  summarize(mdm = mean(Temperature, na.rm = T))


psu_temps <- psu_temps_mwmt %>%
  left_join(., psu_temps_mean_daily_max, by = "reach_pt") %>%
  mutate(Seg = as.numeric(sub('.', '', reach_pt))) %>%
  select(-reach_pt)

# write.csv(psu_lyr, 'hab/Inputs/psu_temps.csv')

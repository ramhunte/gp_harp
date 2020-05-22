# To create spawners_workbook.xlxs source this script

# 5/21 removed need to run full model first

###################################

library(openxlsx)


# Species to run
spp <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')



# Calculate total anadroumous length by EDR and subbasin

flowline <- list.files(path = 'hab/Inputs/spatial_model_outputs/', 
                       pattern = "flowline", 
                       full.names = T) %>%
  read.csv %>%
  mutate(anadromous_network = ifelse(
    (cohospawn == 'Yes' | fallspawn == 'Yes' | chumspawn == 'Yes' |
     sprspawn == 'Yes' | steelspawn == 'Yes'),
    'Yes',
    'No')) %>%
  rename(Subbasin_num = noaa_sub_num) %>%
  select(Subbasin_num, noaaid, anadromous_network, Shape_Length)


# Subbasin names and numbers
subbasin_names <- read.csv('lcm/data/Subbasin_names.csv') %>%
  mutate(EcoRegion = ifelse(str_detect(Subbasin, 'Estuary'),
                            'Estuary', 
                            as.character(EcoRegion)))


# Final calcs
flowline_edr <- flowline %>%
  filter(anadromous_network == 'Yes') %>%
  left_join(., subbasin_names, by = 'Subbasin_num') %>%
  group_by(EcoRegion) %>%
  summarise(length_km = sum(Shape_Length / 1000, na.rm = T))

flowline_subbasin <- flowline %>%
  filter(anadromous_network == 'Yes') %>%
  left_join(., subbasin_names, by = 'Subbasin_num') %>%
  group_by(Subbasin, Subbasin_num) %>%
  summarise(length_km = sum(Shape_Length/1000, na.rm = T))


# Plot colors and labels
plot.params <- read.csv('lcm/data/scenarios.csv') %>% 
  select(scenario, scenario.label, color) %>%
  mutate_if(is.factor, as.character) %>%
  rowid_to_column()


# Set up groups of scenarios
scenarios <- plot.params$scenario

run_scenarios_diag <- str_subset(scenarios, 'ASRP|Hist', negate = TRUE)
#run_scenarios_asrp <- str_subset(scenarios, 'ASRP') # moved ASRP list to EDR and subbasin script

# Create each tab for the Excel workbook
source('srt_figures/spawners_edr.R')
source('srt_figures/spawners_subbasin.R')



# Write tabs to workbook
wb <- loadWorkbook('srt_figures/spawners_template.xlsx')

writeData(wb, sheet = 1, spawners_edr)
writeData(wb, sheet = 2, spawners_edr_diagnostics)
writeData(wb, sheet = 3, spawners_edr_asrp)
writeData(wb, sheet = 4, spawners_sub)
writeData(wb, sheet = 5, spawners_sub_diagnostics)
writeData(wb, sheet = 6, spawners_sub_asrp)



saveWorkbook(wb, 'srt_figures/spawners_workbook.xlsx', overwrite = TRUE)

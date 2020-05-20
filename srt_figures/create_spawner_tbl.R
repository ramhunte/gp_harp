# To run this script first run the full model for coho
# Untested with other species


library(openxlsx)


# Species to run
spp <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')

# Calculate total anadroumous length by EDR and subbasin
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
writeData(wb, sheet = 6, spawners_edr_asrp)



saveWorkbook(wb, 'srt_figures/spawners_workbook.xlsx', overwrite = TRUE)

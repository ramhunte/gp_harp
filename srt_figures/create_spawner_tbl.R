flowline_edr <- flowline %>%
  left_join(., subbasin_names) %>%
  group_by(EcoRegion) %>%
  summarize(length_km = sum(Shape_Length/1000, na.rm = T))
flowline_subbasin <- flowline %>%
  left_join(., read.csv('lcm/data/Subbasin_names.csv'), by = 'Subbasin_num') %>%
  group_by(Subbasin, Subbasin_num) %>%
  summarize(length_km = sum(Shape_Length/1000, na.rm = T))
source('srt_figures/spawners_edr.R')
source('srt_figures/spawners_subbasin.R')
source('srt_figures/spawners_diagnostic_scenarios.R')


wb <- loadWorkbook('srt_figures/spawners_template.xlsx')
writeData(wb, sheet = 1, spawners_edr)
writeData(wb, sheet = 2, spawners_subbasin)
writeData(wb, sheet = 3, spawners_diagnostics)


saveWorkbook(wb, 'srt_figures/spawners_workbook.xlsx', overwrite = TRUE)

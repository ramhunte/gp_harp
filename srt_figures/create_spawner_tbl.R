
species_list <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')

habmodel_results<- lapply(species_list, function(s){
  read.csv(paste0('outputs/', s, '/hab.scenarios/outputs_long/habmodel_outputs.csv')) %>% 
    mutate(species = s)
}) %>%
  do.call('rbind',.) %>%
  select(-year, -X) %>%
  gather('metric', 'data', capacity, survival) %>%
  unite(life.stage, metric, col = 'metric', sep = '.') %>%
  spread(metric, data) %>%
  select(-adults.capacity, -adults.survival)

write.csv(habmodel_results, 'srt_figures/results/habmodel.csv')

lcm_species <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')
lcm_results <- lapply(lcm_species, function(l) {
  
  read.csv(paste0('outputs/', l, '/lcm/', str_replace(l, '_', '.'), '_abundance_by_subbasin.csv')) %>%
    mutate(species = l) %>%
    rename(Subbasin = natal.basin) %>%
    select(Subbasin, scenario,spawners, species) %>%
    filter(scenario %in% diag_scenarios) %>%
    group_by(Subbasin, species) %>%
    mutate(Current = spawners[scenario == 'Current'],
           diff = spawners - Current) %>%
    select(-Current) %>%
    group_by(scenario) %>%
    mutate(rank = rank(diff))
}) %>%
  do.call('rbind',.) %>%
  gather('group', 'data', spawners, diff, rank) %>%
  unite(scenario, group, col = 'scenario_group') %>%
  ungroup() %>%
  spread(scenario_group, data) %>%
  left_join(., subbasin_names) %>%
  mutate(species = case_when(
    species == 'coho' ~ 'coho',
    species == 'spring_chinook' ~ 'spring chinook',
    species == 'fall_chinook' ~ 'fall chinook',
    species == 'steelhead' ~ 'steelhead',
  )) %>%
  rename(BasinNum = Subbasin_num) 

results_spawners <- lcm_results[c('species', 'Subbasin', 'BasinNum', 'EcoRegion', 
                                  'Barriers_spawners', 'Current_spawners', 'Barriers_diff', 'Barriers_rank',
                                  'Beaver_spawners', 'Current_spawners', 'Beaver_diff', 'Beaver_rank',
                                  'Floodplain_spawners', 'Current_spawners', 'Floodplain_diff', 'Floodplain_rank',
                                  'Historical_spawners', 'Current_spawners', 'Historical_diff', 'Historical_rank',
                                  'Shade_spawners', 'Current_spawners', 'Shade_diff', 'Shade_rank',
                                  'Wood_spawners', 'Current_spawners', 'Wood_diff', 'Wood_rank')]

write.csv(results_spawners, 'srt_figures/results/spawners.csv')
  

# Create excel workbook with results for srt
read_filename <- function(fname) {
  read_csv(fname, col_names = TRUE) %>%
    mutate(filename = fname)
}

tbl <- list.files(path = "srt_figures/results/",
                  pattern = '*.csv',
                  full.names = TRUE) %>%
  map_df(~read_filename(.))

tbl$filename <- gsub("Data/", "", tbl$filename)
tbl$filename <- gsub('.csv', '', tbl$filename)

mylist <- tbl %>%
  split(.$filename)
names(mylist)


wb <- createWorkbook() 
lapply(seq_along(mylist), function(i) {
  addWorksheet(wb = wb, sheetName = names(mylist[i]))
  writeData(wb, sheet = i, mylist[[i]][-
                                         length(mylist[[i]])])
})

saveWorkbook(wb, 'srt_figures/test.xlsx', overwrite = TRUE)





library(xlsx)
library(openxlsx)


  # rename(Subbasin = natal.basin) %>%
  # select(Subbasin, scenario,spawners, species) %>%
  # filter(scenario %in% diag_scenarios) %>%
  # group_by(Subbasin, species) %>%
  # mutate(Current = spawners[scenario == 'Current'],
  #        diff = spawners - Current) %>%
  # select(-Current) %>%
  # mutate(rank = rank(diff))
  # gather('group', 'data', spawners, diff, rank) %>%
  # unite(scenario, group, col = 'scenario_group') %>%
  # ungroup() %>%
  # group_by(Subbasin, scenario_group, species) %>%
  # mutate(rank = order(data, decreasing = TRUE)) %>%
  # spread(scenario_group, data)
  # 
  # 
  # spread(scenario, spawners)

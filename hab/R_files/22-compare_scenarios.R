# Script to compare two habitat scenarios

# show dev version of outputs
shell(paste0("git show dev:", outputs_hab, "/outputs_long/habmodel_outputs.csv>", outputs_hab, "/outputs_long/habmodel_outputs_dev.csv"))

save.compare <- file.path('outputs', fishtype,'hab.scenarios', 'diagnostics')
if (dir.exists(save.compare) == F) {dir.create(save.compare,recursive = T)}

# read in dev version
dev_file <- paste0(outputs_hab, "/outputs_long/habmodel_outputs_dev.csv") %>%
  read.csv(.) %>%
  select(-X) %>%
  gather(life_stage_2, dev_value, capacity:survival)

# read in feature version
feature_file <- paste0(outputs_hab, "/outputs_long/habmodel_outputs.csv") %>%
  read.csv(.) %>%
  select(-X) %>%
  gather(life_stage_2, feature_value, capacity:survival)


# store version #s
ver_dev <- shell(cmd = "git describe dev --tags", intern = TRUE) 
ver_feat <- shell(cmd = "git describe --tags", intern = TRUE) 


# Create single dataframe
hab_compare <- rbind(
  paste0(outputs_hab, "/outputs_long/habmodel_outputs_dev.csv") %>%
    read.csv(.) %>%
    select(-X, -year) %>%
    gather(parameter, value, capacity:survival) %>%
    mutate(version = 'dev')
  ,
  # read in feature version
  paste0(outputs_hab, "/outputs_long/habmodel_outputs.csv") %>%
    read.csv(.) %>%
    select(-X, -year) %>%
    gather(parameter, value, capacity:survival) %>%
    mutate(version = branch)
) %>%
  left_join(read.csv('hab/Inputs/Subbasin_names.csv'), by = 'Subbasin_num') %>%
  mutate(hab.param = paste(life.stage, parameter, sep = '_')) %>%
  filter(life.stage != 'adults')


# Create csv output
hab_compare_csv <- hab_compare %>%
  spread(version, value) %>%
  mutate(diff = get(branch) - dev,
         prcnt_diff = scales::percent(diff / dev)) %>%
  filter(abs(diff) > 0.000001) %>%
  select(hab.scenario, life.stage, parameter, Subbasin_num, Subbasin, EcoRegion, dev, branch, diff) 


if (nrow(hab_compare_csv) != 0) {
  
  write.csv(hab_compare_csv, file.path(save.compare, paste0(fishtype, "_hab_scenario_compare.csv")))
  
  if (fishtype != 'spring_chinook') {
    
    hab_compare %<>% 
      filter(value > 0, 
             !is.na(value)) %>%
      group_by(version, hab.scenario, hab.param, parameter, EcoRegion) %>%
      summarize(value_sum = sum(value, na.rm = T),
                value_mean = mean(value, na.rm = T)) %>%
      mutate(value = ifelse(parameter == 'survival', value_mean, value_sum)) %>%
      rename(Subbasin = EcoRegion)
  }
  
  # Remove ASRP scenarios if not running ASRP
  if (run_asrp == "no") {
    hab_compare %<>% 
      filter(hab.scenario %in% diag_scenarios)
  }
  
  
  
  
  
  # Create the percent diff labels
  
  labs_df <- hab_compare %>%
    ungroup() %>%
    mutate(version = ifelse(version == branch, 'value_feat', 'value_dev')) %>%
    spread(version, value) %>%
    mutate(prcnt_diff = (value_feat - value_dev) / value_dev) %>%
    group_by(hab.param) %>%
    mutate(max_dev = max(value_dev, na.rm = T),
           max_feat = max(value_feat, na.rm = T),
           max = ifelse(max_dev > max_feat, max_dev, max_feat)) %>%
    filter(!is.na(prcnt_diff)) %>%
    group_by(hab.scenario, hab.param, max) %>%
    summarize(prcnt_diff = mean(prcnt_diff, na.rm = T)) %>%
    mutate(mean_prcnt_diff = scales::percent(prcnt_diff),
           y = max * 1.15)
  
  # Remove ASRP scenarios if not running ASRP
  if (run_asrp == "no") {
    labs_df %<>% 
      filter(hab.scenario %in% diag_scenarios)
  }
  
  
  # Loop through the plots
  for (h in labs_df$hab.param %>% unique) {
    
    hab_compare %>%
      filter(hab.param == h) %>%
      ggplot +
      theme_bw() +
      geom_bar(aes(Subbasin, value, fill = version),
               stat = 'identity',position = 'dodge') +
      geom_text(data = labs_df %>% filter(hab.param == h), 
                aes(y = y, label = paste('Ave. % diff',' = ', mean_prcnt_diff)),
                x = 1, 
                size = 3,
                hjust = "inward") +
      facet_wrap(~hab.scenario) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5)) +
      labs(title = h,
           caption = paste0('dev = ', ver_dev, ' | ', branch, ' = ', ver_feat))
    
    ggsave(file.path(save.compare,
                     paste0('hab_params_difference_',
                            h,
                            '.jpg')
    ),
    width = 9, 
    height = 7, 
    dpi = 300)
  }
  
} else {print("No changes to habmodel results")}

# Delete the dev version
unlink(paste0(outputs_hab, "/outputs_long/habmodel_outputs_dev.csv"))

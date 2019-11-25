
# This script can be used to compare two different model runs
# Inside of each edr_plots folder is a csv containg the spawners per scenario per EDR
# This script reaches into those folders, joins the two documnets together, and creates a plot



# Create comparison folder ----
out.path.compare <- file.path(outputs_lcm, 'comparison')
if (dir.exists(out.path.compare) == F) {dir.create(out.path.compare, recursive = T)}



# Path to csv with spawners by EDR ----
path_to_data <- file.path('outputs', fishtype, 'lcm') %>% 
  list.files( ., pattern = "raw.csv", full.names = T) 


# Bring file back from dev ----

shell_cmd <- paste0('git show dev:', path_to_data, ' > dev_spawners.csv')

if (os == 'windows') {
  shell(cmd = shell_cmd)
} else{
  system(shell_cmd)
}

dev_spawners <- read.csv('dev_spawners.csv')
unlink('dev_spawners.csv') # Delete dev version

if (os == 'windows') {
  ver_dev <- shell(cmd = paste0(ver_dev_cmd), intern = TRUE)
} else{
  ver_dev <- system(paste0(ver_dev_cmd), intern = TRUE)
}


df <- path_to_data %>%
  read.csv() %>%
  rename(spawners.feature = spawners) %>%
  full_join(dev_spawners %>%
              rename(spawners.dev = spawners),
            by = c('scenario','natal.basin')
  ) %>%
  select(scenario, 
         natal.basin,
         spawners.feature,
         spawners.dev
  ) %>%
  gather(version, spawners, spawners.feature:spawners.dev) %>%
  mutate(version = ifelse(version == 'spawners.feature',
                          branch,
                          'dev')
  ) %>%
  mutate(version = factor(version, levels = c('dev', branch)))

if (species != 'spring.chinook') {
  df %<>%
    left_join(read.csv('lcm/data/Subbasin_names.csv') %>%
                rename(natal.basin = Subbasin)) %>%
    group_by(version, scenario, EcoRegion) %>%
    summarize(spawners = sum(spawners, na.rm = TRUE))
} else {
  df %<>%
    rename(EcoRegion = natal.basin) %>%
    filter(spawners > 1)
}
  
labs_df <- df %>%
  group_by(scenario, version) %>%
  summarize(n = prettyNum(sum(spawners, na.rm = T), big.mark = ',', digits = 0),
            max_edr = max(spawners, na.rm = T)) %>%
  ungroup %>%
  mutate(max = max(max_edr, na.rm = T),
         y = ifelse(version == branch, max + max * .15, max)) %>%
  bind_rows(df %>%
              group_by(scenario,version) %>%
              summarize(n = sum(spawners)) %>%
              mutate(prcnt_diff = (n[version == branch] - n) / n,
                     n = ifelse(abs(prcnt_diff) > 0, scales::percent(prcnt_diff), '0%')) %>%
              filter(version == 'dev') %>%
              mutate(version = 'percent diff')
            
  ) %>%
  mutate(y = ifelse(version == 'percent diff', 
                    max[version == branch] - max[version == branch] * .15, 
                    y))

labs_df_test <- labs_df %>%
  filter(abs(prcnt_diff) > 0)

if (nrow(labs_df_test) != 0) {
  
  print(
    ggplot(df) +
      theme_bw() +
      geom_bar(aes(EcoRegion,spawners,fill = version),
               stat = 'identity',position = 'dodge') +
      facet_wrap(~scenario) +
      geom_text(data = labs_df, 
                x = 5, 
                aes(y = y, label = paste(version,' - ', n)),
                size = 2) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
      labs(caption = paste0(pop, ' - dev version = ', ver_dev),
           y = 'spawners')
  )
  
  
  ggsave(file.path(out.path.compare,
                   paste('Comparison_Total_run',
                         pop,
                         paste0(format(Sys.time(), "%Y%m%d"),'.jpg'),
                         sep = "_")),
         width = 10, 
         height = 8, 
         dpi = 300)
  
  # Print summary metrics to the screen
  print(paste0(pop, " --------------- summary of percent differences from HEAD of dev branch -----------------"))
  print(df %>%
          group_by(scenario,version) %>%
          summarize(n = sum(spawners, na.rm = T)) %>%
          spread(version, n) %>%
          mutate(prcnt_diff = (get(branch) - dev) / dev,
                 prcnt_diff = ifelse(abs(prcnt_diff) > 0, scales::percent(prcnt_diff), '0%'))
  )
} else {print("No changes to lcm results")}

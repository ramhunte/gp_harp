
# This script can be used to compare two different model runs
# Inside of each edr_plots folder is a csv containg the spawners per scenario per EDR
# This script reaches into those folders, joins the two documnets together, and creates a plot



# Create comparison folder ----
out.path.compare <- file.path(outputs_lcm, 'comparison')
if (dir.exists(out.path.compare) == F) {dir.create(out.path.compare, recursive = T)}



# Path to csv with spawners by EDR ----
path_to_edr <- file.path('outputs', fishtype, 'lcm', 'edr_plots') %>% 
  list.files( ., pattern = ".csv", full.names = T) 


# Bring file back from dev ----
shell_cmd <- paste0('git show dev:', path_to_edr, ' > dev_spawners_edr.csv')
shell(cmd = shell_cmd)
dev_edr <- read.csv('dev_spawners_edr.csv')
unlink('dev_spawners_edr.csv') # Delete dev version

no_asrp_scenarios <- as.character(str_replace_all(diag_scenarios, "_", "\\."))
if (run_asrp == "no") {
  dev_edr %<>% 
    filter(scenario %in% no_asrp_scenarios)
}


df <- path_to_edr %>%
  read.csv() %>%
  rename(total.run.feature = spawners,
         file.feature = habitat.file) %>%
  mutate(scenario = ifelse(scenario == 'Temperature','Shade',as.character(scenario))) %>%
  full_join(dev_edr %>%
              rename(total.run.dev = spawners,
                     file.dev = habitat.file) %>%
              mutate(scenario = ifelse(scenario == 'Temperature','Shade',as.character(scenario)))
            ,by = c('scenario','EcoRegion')
  ) %>%
  select(scenario, 
         EcoRegion,
         total.run.feature,
         total.run.dev
  ) %>%
  gather(version, total.run, total.run.feature:total.run.dev) %>%
  mutate(version = ifelse(version == 'total.run.feature',
                          branch
                          ,
                          'dev'
  )
  )

labs_df <- df %>%
  group_by(scenario, version) %>%
  summarize(n = prettyNum(sum(total.run), big.mark = ','),
            max_edr = max(total.run)) %>%
  ungroup %>%
  mutate(max = max(max_edr),
         y = ifelse(version == branch, max + max * .15, max)) %>%
  bind_rows(df %>%
              group_by(scenario,version) %>%
              summarize(n = sum(total.run)) %>%
              mutate(prcnt_diff = (n[version == branch]- n) / n,
                     n = scales::percent(prcnt_diff)) %>%
              filter(version == 'dev') %>%
              mutate(version = 'percent diff')
            
  ) %>%
  mutate(y = ifelse(version == 'percent diff', 
                    max[version == branch] - max[version == branch] * .15, 
                    y))

print(
  ggplot(df) +
    theme_bw() +
    geom_bar(aes(EcoRegion,total.run,fill = version),
             stat = 'identity',position = 'dodge') +
    facet_wrap(~scenario) +
    geom_text(data = labs_df, 
              x = 5, 
              aes(y = y, label = paste(version,' - ', n)),
              size = 2) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
    labs(caption = paste0(pop, ' - Habitat file version = ', hab.ver),
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

# Create output csv
# df %>%
#   spread(version, total.run) %>%
#   write.csv(file.path(out.path.compare,
#                       paste('LCM_comparison',
#                             pop,
#                             paste0(format(Sys.time(), "%Y%m%d"),'.csv'),
#                             sep = "_")))

# Print summary metrics to the screen
print(paste0(pop, " --------------- summary of percent differences from HEAD of dev branch -----------------"))
print(df %>%
        group_by(scenario,version) %>%
        summarize(n = sum(total.run, na.rm = T)) %>%
        spread(version, n) %>%
        mutate(prcnt_diff = (get(branch) - dev) / dev,
               prcnt_diff = scales::percent(prcnt_diff))
)


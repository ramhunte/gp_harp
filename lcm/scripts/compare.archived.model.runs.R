
# This script allows you to create a change graph for an arbitrary commit, compared to its parent commit
# Running this will create a new folder outputs/compare_runs
# Note hat before early March 2019 the files needed to create these figures were not version controlled


# Which commit would you like to run? This commit will be compared to its parent commit
commit <- '1281464'

# Write a short description. This will appear as the title of the figure
ver_desc <- 'Switch to EDT widths' # short descriptive name

# Comment out species you don't want to run
compare_spp <- c('coho', 'spring.chinook', 'fall.chinook', 'steelhead')




# Below here should run -------

out.path.compare <- file.path('outputs', 'compare_runs', commit)
if (dir.exists(out.path.compare) == F) {dir.create(out.path.compare, recursive = T)}

no_asrp_scenarios <- as.character(str_replace_all(diag_scenarios, "_", "\\."))

for (s in compare_spp) {
  
  
  # Path to csv with spawners by EDR ----
  path_to_edr <- file.path('outputs', gsub('\\.','_',s), 'lcm', 'edr_plots') %>% 
    list.files( ., pattern = ".csv", full.names = T) 
  
  # Desired version ----
  shell_cmd <- paste0('git show ', commit, ':', path_to_edr, ' > child_spawners_edr.csv')
  shell(cmd = shell_cmd)
  child_edr <- read.csv('child_spawners_edr.csv') %>% filter(scenario %in% no_asrp_scenarios)
  unlink('child_spawners_edr.csv') # Delete dev version
  ver_child <- shell(cmd = paste0("git describe ", commit, " --tags"), intern = TRUE) # store version num
  
  # Parent of desired version ----
  shell_cmd <- paste0('git show ', commit, '~1:', path_to_edr, ' > parent_spawners_edr.csv')
  shell(cmd = shell_cmd)
  parent_edr <- read.csv('parent_spawners_edr.csv') %>% filter(scenario %in% no_asrp_scenarios)
  unlink('parent_spawners_edr.csv') # Delete dev version
  ver_parent <- shell(cmd = paste0("git describe ", commit, "~1 --tags"), intern = TRUE) # store version num
  
  # no_asrp_scenarios <- as.character(str_replace_all(diag_scenarios, "_", "\\."))
  # if (run_asrp == "no") {
  #   dev_edr %<>% 
  #     filter(scenario %in% no_asrp_scenarios)
  # }
  
  
  df <- child_edr %>%
    rename(total.run.feature = spawners,
           file.feature = habitat.file) %>%
    mutate(scenario = ifelse(scenario == 'Temperature','Shade',as.character(scenario))) %>%
    full_join(parent_edr %>%
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
                            ver_child
                            ,
                            ver_parent
    )
    )
  
  labs_df <- df %>%
    group_by(scenario, version) %>%
    summarize(n = prettyNum(sum(total.run), big.mark = ','),
              max_edr = max(total.run)) %>%
    ungroup %>%
    mutate(max = max(max_edr),
           y = ifelse(version == ver_child, max + max * .15, max)) %>%
    bind_rows(df %>%
                group_by(scenario,version) %>%
                summarize(n = sum(total.run)) %>%
                mutate(prcnt_diff = (n[version == ver_child] - n) / n,
                       n = scales::percent(prcnt_diff)) %>%
                filter(version == ver_parent) %>%
                mutate(version = 'percent diff')
              
    ) %>%
    mutate(y = ifelse(version == 'percent diff', 
                      max[version == ver_child] - max[version == ver_child] * .15, 
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
      labs(caption = s,
           y = 'spawners',
           title = ver_desc)
  )
  
  
  ggsave(file.path(out.path.compare,
                   paste0(s,
                          '_compare_model_runs_',
                          ver_child,
                          '.jpg')
  ),
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
  print(paste0(s, " --------------- summary of percent differences from HEAD of dev branch -----------------"))
  print(df %>%
          group_by(scenario,version) %>%
          summarize(n = sum(total.run, na.rm = T)) %>%
          spread(version, n) %>%
          mutate(prcnt_diff = (get(ver_child) - get(ver_parent)) / get(ver_parent),
                 prcnt_diff = scales::percent(prcnt_diff))
  )

}
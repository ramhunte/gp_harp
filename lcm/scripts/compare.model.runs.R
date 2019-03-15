
# This script can be used to compare two different model runs
# Inside of each edr_plots folder is a csv containg the spawners per scenario per EDR
# This script reaches into those folders, joins the two documnets together, and creates a plot


date_v1 <- '20190312'
date_v2 <- '20190313'


######## Source the file and choose a species #########
# species_for_comparison <-  species[menu(species, title = "Choose a species", graphics = TRUE)]

######## Everything below here should run #########

v1 <- file.path("lcm/lcm_outputs", date_v1,species_for_comparison, 'edr_plots')
v2 <- file.path("lcm/lcm_outputs", date_v2,species_for_comparison, 'edr_plots')


df <- list.files(v1, pattern = ".csv", full.names = T) %>%
  read.csv() %>%
  rename(total.run.v1 = spawners,
         file.v1 = habitat.file) %>%
  mutate(scenario = ifelse(scenario == 'Temperature','Shade',as.character(scenario))) %>%
  full_join(list.files(v2,pattern = ".csv",full.names = T) %>%
              read.csv() %>%
              rename(total.run.v2 = spawners,
                     file.v2 = habitat.file) %>%
              mutate(scenario = ifelse(scenario == 'Temperature','Shade',as.character(scenario)))
            ,by = c('scenario','EcoRegion')
            ) %>%
  select(scenario, 
         EcoRegion,
         total.run.v1,
         total.run.v2
         ) %>%
  gather(version, total.run, total.run.v1:total.run.v2) %>%
  mutate(version = ifelse(version == 'total.run.v1',
                          date_v1
                          ,
                          date_v2
                          )
         )
  
labs_df <- df %>%
  group_by(scenario, version) %>%
  summarize(n = prettyNum(sum(total.run), big.mark = ','),
            max_edr = max(total.run)) %>%
  ungroup %>%
  mutate(max = max(max_edr),
         y = ifelse(version == date_v1, max + max * .15, max)) %>%
  bind_rows(df %>%
              group_by(scenario,version) %>%
              summarize(n = sum(total.run)) %>%
              mutate(prcnt_diff = (n - n[version == date_v1]) / n[version == date_v1],
                     n = scales::percent(prcnt_diff)) %>%
              filter(version == date_v2) %>%
              mutate(version = 'percent diff')
              
  ) %>%
  mutate(y = ifelse(version == 'percent diff', 
                    max[version == date_v1] - max[version == date_v1] * .15, 
                    y))

ggplot(df) +
  theme_bw() +
  geom_bar(aes(EcoRegion,total.run,fill = version),
           stat = 'identity',position = 'dodge') +
  facet_wrap(~scenario) +
  geom_text(data = labs_df, 
            x = 5, 
            aes(y = y, label = paste("Total = ",n))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))


out.path <- file.path('lcm/lcm_outputs', date_v2, species_for_comparison, 'comparison')
if (dir.exists(out.path) == F) {dir.create(out.path, recursive = T)}

ggsave(file.path(out.path,
                 paste('Comparison_Total_run',
                       species_for_comparison,
                       paste0(format(Sys.time(), "%Y%m%d"),'.jpg'),
                       sep = "_")),
       width = 10, 
       height = 8, 
       dpi = 300)


df %>%
  spread(version, total.run) %>%
  write.csv(file.path(out.path,
                      paste('LCM_comparison',
                            species_for_comparison,
                            paste0(format(Sys.time(), "%Y%m%d"),'.csv'),
                            sep = "_")))


ave_prcnt_diff <- df %>%
  group_by(scenario,version) %>%
  summarize(n = sum(total.run)) %>%
  mutate(prcnt_diff = (n - n[version == date_v1]) / n[version == date_v1]) %>%
  ungroup() %>%
  summarize(prcnt_diff = scales::percent(mean(prcnt_diff))) %>%
  pull(prcnt_diff)

print(paste0(species_for_comparison, " -- Average Percent Difference = " , ave_prcnt_diff))

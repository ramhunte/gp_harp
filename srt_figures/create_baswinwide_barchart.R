
# Recreate standard plots for V12.1

## Figure X - 4 panel Diagnostic scenario bar charts

results <- 'srt_figures/outputs_v13.1_w_Hist_future/'


# Plot colors and labels
plot.params <- read.csv('lcm/data/scenarios.csv') %>% 
  select(scenario, scenario.label, color) %>%
  mutate_if(is.factor, as.character) %>%
  rowid_to_column()


# List all basinwide results files
basinwide_paths <- list.files(results,
                              recursive = TRUE,
                              pattern = 'abundance_basinwide',
                              full.names = TRUE)

# Create list of species to attach to each file
spp <- basinwide_paths %>%
  dirname %>%
  dirname %>%
  basename()

basinwide_df <- basinwide_paths %>%
  map_dfr(.id = 'species', read.csv) %>%
  select(-X) %>%
  mutate(species = spp[as.numeric(species)],
         species = case_when(
           species == 'coho'  ~ 'Coho',
           species == 'fall_chinook'  ~ 'Fall Chinook',
           species == 'spring_chinook'   ~ 'Spring Chinook',
           species == 'steelhead' ~ 'Steelhead',
           species == 'chum'  ~ 'Chum'
         )) 




# Join data and plot parameters, create labels for percent change
df_fig <- basinwide_df %>%
  filter(species != 'Chum',
         str_detect(scenario, 'ASRP|Historical.2', negate =  TRUE)) %>%
  left_join(plot.params) %>%
  rename(n = basinwide_spawners) %>%
  group_by(species) %>%
  mutate(curr = n[scenario == 'Current'],
         prcnt.change = (n - curr)/curr,
         prcnt.change = ifelse(prcnt.change >= 0,
                               paste0('+', scales::label_percent(1)(prcnt.change)),
                               scales::label_percent(1)(prcnt.change))) %>%
  arrange(rowid)



# Plot it
ggplot(df_fig) +
  theme_classic() +
  facet_wrap(~species, scales = 'free_y') +
  geom_bar(aes(reorder(scenario.label, rowid), 
               n, 
               fill = color),
           color = 'black',
           stat = "identity") +
  geom_bar(aes(scenario.label, 
               curr),
           color = 'black',
           stat = "summary", 
           fun.y = "mean",
           fill = 'grey',
           alpha = .6) +
  geom_text(data = df_fig %>% filter(scenario != 'Current'), 
            aes(x = scenario.label , 
                y = n, 
                label = prcnt.change),
            vjust = -0.3,
            size = 2.5) +
  scale_fill_identity(guide = F) +
  scale_y_continuous(labels = comma, 
                     expand = c(0, 0,.1,0)) +
  labs(x = NULL,
       y = paste0('Spawners')) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1),
        text = element_text(size = 10),
        strip.background = element_blank())


ggsave("Fig_basinwide_diag_barchart.tiff", width = 8, height = 6, dpi = 300, compression = "lzw")



# Create ASRP plots with future natural potential scenarios added in

asrp_basinwide <- basinwide_df %>%
  filter(!species == 'Chum',
         str_detect(scenario, 'ASRP|Historical.2|Current')) %>%
  left_join(plot.params) %>%
  rename(n = basinwide_spawners) %>%
  group_by(species) %>%
  mutate(curr = n[scenario == 'Current'])  %>%
  ungroup() %>%
  mutate(year = ifelse(scenario.label %in% c("ASRP 1 - 2040", "ASRP 2 - 2040", "ASRP 3 - 2040", 'ASRP No action, with future development 2040',
                                             'Natural potential - 2040'),
                       'mid-century',
                       ifelse(scenario.label %in% c("ASRP 1 - 2080", "ASRP 2 - 2080", "ASRP 3 - 2080", 
                                                    'ASRP No action, with future development 2080', 'Natural potential - 2080'),
                              'late-century',
                              'current')),
         scenario.label.nm = case_when(
    scenario.label %in% c("ASRP 1 - 2040", "ASRP 1 - 2080") ~ "ASRP 1",
    scenario.label %in% c("ASRP 2 - 2040", "ASRP 2 - 2080") ~ "ASRP 2",
    scenario.label %in% c("ASRP 3 - 2040", "ASRP 3 - 2080") ~ "ASRP 3",
    scenario.label %in% c('ASRP No action, with future development 2040', 'ASRP No action, with future development 2080') ~ "No action",
    scenario.label %in% c('Natural potential - 2040', 'Natural potential - 2080') ~ 'Natural potential',
    scenario.label %in% c("Current") ~ "Current"))

asrp_basinwide$scenario.label.nm <- factor(asrp_basinwide$scenario.label.nm, levels = c('No action', 'Current', 'ASRP 1', 'ASRP 2', 'ASRP 3', 'Natural potential'))
asrp_basinwide$year <- factor(asrp_basinwide$year, levels = c('current', 'mid-century', 'late-century'))

colors.asrp.basinwide <- asrp_basinwide %>%
  distinct(scenario.label, color) %>%
  slice(c(9, 1, 3, 5 ,7, 11)) %>%
  select(color) %>%
  unlist(use.names = FALSE)

# for (sp in c('Coho', 'Spring Chinook', 'Fall Chinook', 'Steelhead')) {
  # a <- asrp_basinwide %>% filter(species == sp)
ggplot() +
  theme_bw() +
  facet_wrap(~species, scales = 'free_y') +
  geom_bar(data = asrp_basinwide %>% filter(scenario.label.nm %in% c('Current', 'Natural potential')),
           aes(year, n, fill = scenario.label.nm),
           color = 'black',
           stat = 'summary',
           fun.y = 'mean') +
  geom_bar(data = asrp_basinwide %>% filter(scenario.label.nm %in% c("Current", 'ASRP 3')), 
           aes(year, n, fill = scenario.label.nm),
           color = 'black',
           stat = "summary", 
           fun.y = "mean") +
  geom_bar(data = asrp_basinwide %>% filter(scenario.label.nm %in% c("Current", 'ASRP 2')), 
           aes(year, n, fill = scenario.label.nm),
           color = 'black',
           stat = "summary", 
           fun.y = "mean") +
  geom_bar(data = asrp_basinwide %>% filter(scenario.label.nm %in% c("Current", 'ASRP 1')), 
           aes(year, n, fill = scenario.label.nm),
           color = 'black',
           stat = "summary", 
           fun.y = "mean") +
  geom_bar(data = asrp_basinwide %>% filter(scenario.label.nm %in% c("Current", 'No action')), 
           aes(year, n, fill = scenario.label.nm),
           color = 'black',
           stat = "summary", 
           fun.y = "mean") +
  scale_x_discrete(drop = T) +
  scale_fill_manual(values = c('grey60', 'dodgerblue', 'chartreuse4', 'darkorange2', 'deeppink3', 'grey40'), drop = F, name = 'scenario') +
  #colors.asrp.basinwide, drop = F, name = "Scenario") +
  scale_y_continuous(labels = comma, 
                     expand = c(0, 0,.05,0)) +
  labs(x = NULL,
       y = paste0('Spawners')) +
       # caption = paste0('Model version = ',hab.ver)
  # ) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1),
        text = element_text( size = 16))

ggsave(paste0('srt_figures/asrp_spawners', '.jpg'), width = 10, height = 10, dpi = 300)


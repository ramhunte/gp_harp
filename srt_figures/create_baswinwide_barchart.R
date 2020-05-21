
# Recreate standard plots for V12.1

## Figure X - 4 panel Diagnostic scenario bar charts

results <- 'outputs_v13.1_w_Hist_future/'


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


spawners_paper_spring_chinook <- read.csv('temp_paper/spawners_figure/spawners_paper_spring_chinook.csv', header = TRUE) %>%
  mutate(species = 'Spring chinook')
spawners_paper_fall_chinook <- read.csv('temp_paper/spawners_figure/spawners_paper_fall_chinook.csv', header = TRUE) %>%
  mutate(species = 'Fall chinook')
spawners_paper_coho <- read.csv('temp_paper/spawners_figure/spawners_paper_coho.csv', header = TRUE) %>%
  mutate(species = 'Coho')
spawners_paper_steelhead <- read.csv('temp_paper/spawners_figure/spawners_paper_steelhead.csv', header = TRUE) %>%
  mutate(species = 'Steelhead')

species_order <- c('Spring chinook', 'Coho', 'Steelhead', 'Fall chinook')
year_order <- c('Current', 'Mid-century', 'Late-century')

spawners.paper.all <- bind_rows(spawners_paper_spring_chinook, spawners_paper_coho, spawners_paper_fall_chinook, spawners_paper_steelhead)
spawners.paper.all$species <- factor(spawners.paper.all$species, levels = species_order)
spawners.paper.all$year <- factor(spawners.paper.all$year, levels = year_order)


label_df_spring_chinook <- read.csv('temp_paper/spawners_figure/label_df_paper_spring_chinook.csv', header = TRUE) %>%
  mutate(species = 'Spring chinook')
label_df_fall_chinook <- read.csv('temp_paper/spawners_figure/label_df_paper_fall_chinook.csv', header = TRUE) %>%
  mutate(species = 'Fall chinook')
label_df_coho <- read.csv('temp_paper/spawners_figure/label_df_paper_coho.csv', header = TRUE) %>%
  mutate(species = 'Coho')
label_df_steelhead <- read.csv('temp_paper/spawners_figure/label_df_paper_steelhead.csv', header = TRUE) %>%
  mutate(species = 'Steelhead')

label.df.all <- bind_rows(label_df_spring_chinook, label_df_fall_chinook, label_df_coho, label_df_steelhead)
label.df.all$species <- factor(label.df.all$species, levels = species_order)

colors.paper.all <- read.csv('temp_paper/spawners_figure/colors_paper_coho.csv', header = TRUE) %>%
  select(-1)



ggplot() +
  theme_bw() +
  geom_bar(data = spawners.paper.all %>% filter(!scenario.label == 'Current'),
           aes(scenario.label.nm, prcnt.change, fill = scenario.label),
           color = 'black',
           stat = 'summary',
           fun.y = 'mean') +
  ylim(-100, 100) +
  facet_grid(species~year) +
  theme(panel.spacing = unit(1.5, 'lines'))+
  geom_text(data = label.df.all, 
            aes(x = scenario.label.nm , 
                y = prcnt.change + 10 * sign(prcnt.change), 
                label = spawners.change)) + #,
            # vjust = -0.5) +
  # scale_fill_grey() +

  scale_x_discrete(drop = T) + 
  scale_fill_manual(values = colors.paper, drop = F, name = 'Scenario') +

  # scale_y_continuous(labels = comma, 
  #                    expand = c(0, 0,.05,0)) +
  labs(x = 'Scenario',
       y = paste0('Percent Change in Spawners from Current Conditons')
  ) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1),
        text = element_text( size = 16)) +
  theme(legend.position = 'none')
ggsave(file.path(paste0('temp_paper/spawners_figure/spawners_basinwide_all', '_temp_paper','.jpg')), width = 10, height = 15, dpi = 300)

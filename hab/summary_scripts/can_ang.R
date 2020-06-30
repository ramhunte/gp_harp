# This script summarizes the proportion of reaches within each canopy opening angle bin.  It is used to create figure 5 in the habitat paper.  To run, first run the model through the flowline script.  This script uses the attributed flowline produced in the flowline script.

total_reaches = as.numeric(tally(flowline))

angle_levels <- c('< 10°', '10° - 25°', '25° - 50°', '50° - 100°', '> 100°')

can_ang_summary <- flowline %>%
  mutate(
    can_ang_bin_curr = case_when(
      can_ang < 10 ~ '< 10°',
      can_ang < 25 & can_ang >= 10 ~ '10° - 25°',
      can_ang < 50 & can_ang >= 25 ~ '25° - 50°',
      can_ang < 100 & can_ang >= 50 ~ '50° - 100°',
      can_ang >= 100 ~ '> 100°'
    ) 
  ) %>% 
  group_by(can_ang_bin_curr) %>%
  tally() %>% 
  mutate(prop = n / total_reaches)
    
hist_ang_summary <- flowline %>%    
    mutate(
      can_ang_bin_hist = case_when(
        hist_ang < 10 ~ '< 10°',
        hist_ang < 25 & hist_ang >= 10 ~ '10° - 25°',
        hist_ang < 50 & hist_ang >= 25 ~ '25° - 50°',
        hist_ang < 100 & hist_ang >= 50 ~ '50° - 100°',
        hist_ang >= 100 ~ '> 100°'
      )
    ) %>%
  group_by(can_ang_bin_hist) %>%
  tally() %>%
  filter(!is.na(can_ang_bin_hist)) %>%
  mutate(prop = n / total_reaches)

ang_summary <- full_join(can_ang_summary, hist_ang_summary) %>%
  gather(period, ang_bin, c(can_ang_bin_curr, can_ang_bin_hist)) %>%
  filter(!is.na(ang_bin)) %>%
  mutate(period = ifelse(period == 'can_ang_bin_curr', 
                         'Current',
                         'Historical'))

ang_summary$ang_bin <- factor(ang_summary$ang_bin, levels = angle_levels)
ang_summary$period <- factor(ang_summary$period, levels = c('Historical', 'Current'))


ggplot() +
  theme_bw() +
  geom_bar(data = ang_summary,
           stat = 'identity',
           aes(ang_bin, prop, fill = period),
           color = 'black',
           # fill = 'blue',
           position = 'dodge') +
  scale_fill_manual(values = c('blue', 'deepskyblue'), drop = F, name = 'period') +
  xlab('Canopy Opening Range') +
  ylab("Proportion of Reaches") +
  theme(legend.position = c(.9, .9),  legend.title = element_blank())
ggsave('hab/summary_scripts/can_angle_bins.jpg', width = 10, height = 8, dpi = 300)

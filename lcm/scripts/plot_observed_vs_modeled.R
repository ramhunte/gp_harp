# Purpose: Read in WDFW observed spawner (escapement) abundance by trib (subbasin)
# Plot the WDFW observed data vs the NOAA LCM output data
# WDFW data are from 5-13-14 Revised escapment including 2013 spring chinook_CM.xlxs
# A copy can be found here: N:\watershed\Chehalis\Data\9-LCM\Chehalis_specific_data



# Read in WDFW data ----
wdfw_data <- read.csv('lcm/data/wdfw_observed_by_trib/WDFW_escapement_by_trib.csv') %>%
  gather(year, spawners, X1991:X2013) %>%
  left_join(read.csv('lcm/data/wdfw_observed_by_trib/WDFW_reach_name_conversion.csv')) %>%
  rename(natal.basin = NOAA_Reach) %>%
  mutate(source = 'wdfw',
         year = sub('X','',year),
         natal.basin = ifelse(natal.basin %in% c("", NA),
                              as.character(WDFW_Reach),
                              as.character(natal.basin)))


# Read in NOAA LCM data ----
spp <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')

noaa_model <- lapply(spp, function(s) {
  fp <- file.path('outputs', s, 'lcm')
  
  dat <- list.files(fp, 
                    pattern = 'abundance_by_subbasin_raw.csv', # Name of csv with LCM spawner data
                    full.names = TRUE) %>%
    read.csv %>%
    mutate(species = s)
  
  return(dat)
}) %>%
  do.call('bind_rows', .) %>%
  filter(scenario == 'Current') %>%
  select(species, natal.basin, spawners) %>%
  mutate(source = 'lcm')



# Subset data for plotting ----
# 1. only subbasins where there is WDFW data
# 2. observations after year 2000
dat_plot <- bind_rows(wdfw_data, noaa_model) %>%
  mutate(species = sub("_", " ", species)) %>%
  filter(spawners >= 1,
         #natal.basin != "",
         natal.basin %in% unique(wdfw_data$natal.basin),
         year > 2000 | is.na(year))



# Create plot ----
ggplot() +
  theme_bw() +
  geom_boxplot(data = dat_plot %>%
                 filter(source == 'wdfw'),
               aes(natal.basin, spawners),
               outlier.shape = NA) +
  geom_jitter(data = dat_plot, #%>%
               #filter(source == 'lcm'),
             aes(natal.basin, 
                 spawners, 
                 shape = source, 
                 fill = source),
             width = .2,
             #shape = 4,
             size = 2.5) +
             #color = 'red') +
  scale_shape_manual(values = c(22, 43)) +
  facet_wrap(~species, scales = 'free_y') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL,
       y = 'Escapement / Spawners',
       title = " WDFW observed (2000 - current) and NOAA LCM outputs",
       caption = paste0('Note: boxes correspond to WDFW observed data \n', hab.ver))


# Save plot ----
ggsave("outputs/Observed_v_LCM.jpg", width = 10, height = 8, dpi = 300)
  

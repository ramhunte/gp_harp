library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
# file_loc <- 'nwcfile/FE/watershed/Chehalis/Data/10-Habitat data layers/SpatialModel_Archive/20191204'
gdb_in <- '//nwcfile/FE/watershed/Chehalis/Data/10-Habitat data layers/SpatialModel_Archive/20191204/Inputs.gdb'
gdb_out <- '//nwcfile/FE/watershed/Chehalis/Data/10-Habitat data layers/SpatialModel_Archive/20191204/Outputs.gdb'

sub <- st_read(gdb_in, 'NOAA_subbasins_w_fp') %>% st_geometry()
fl <- st_read(gdb_out, 'flowline')

streamlines <- fl %>%
  select(noaaid, noaa_sub_num, cohospawn:steelspawn, wet_width) %>%
  gather(species, presence, cohospawn:steelspawn) %>%
  mutate(species = case_when(
    species == 'cohospawn' ~ 'Coho',
    species == 'fallspawn' ~ 'Fall Chinook',
    species == 'sprspawn' ~ 'Spring Chinook',
    species == 'steelspawn' ~ 'Steelhead'
  )) %>%
  filter(species != 'chumspawn') %>%
  mutate(distribution = case_when(
    (noaa_sub_num %in% 52:63) & presence == 'No' ~ 'Rearing and outmigration',
    (noaa_sub_num %in% 52:63) & presence == 'Yes' ~ 'Spawning and rearing',
    !(noaa_sub_num %in% 52:63) & presence == 'Yes' ~ 'Spawning and rearing',
    !(noaa_sub_num %in% 52:63) & presence ==  'No' ~ 'No'),
    width_bin = cut(wet_width, c(-Inf, 5, 10, 20, 30, 50, Inf))) %>%
  filter(!is.na(width_bin))

streamlines$species <- factor(streamlines$species, levels = c('Spring Chinook', 'Coho', 'Fall Chinook', 'Steelhead'))

distributions <- ggplot() + 
  theme_void() + 
  geom_sf(data = st_union(sub), size = .2, color = 'grey20', fill =NA) +
  geom_sf(data = streamlines, aes(color = distribution, size = width_bin), show.legend = 'line')+
  facet_wrap(~species) +
  scale_color_manual(breaks = c('Spawning and rearing', 'Rearing and outmigration'),
                     values = c('gray70', '#CC79A7', '#0072B2')) +
  scale_size_manual(values = seq(0.1, 1, length.out = 6), guide = FALSE) +
  theme(legend.position = 'bottom',
        strip.text.x = element_text(margin = margin(0, 0, .1, 0, 'cm'), size = 12),
        legend.text = element_text( size = 12)) +  
  labs(color = NULL)
ggsave('distributions.tiff', dpi = 300, height = 7, width = 6, compression = 'lzw')

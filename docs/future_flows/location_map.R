
# Location map for peak flow paper
library(tidyverse)
library(sf)
library(cowplot) 
library(ggspatial)



# Chehalis boundary
#gdb <- '//nwcfile/FE/watershed/Chehalis/Data/10-Habitat data layers/SpatialModel_Archive/20200410' #20200414
gdb <- 'C://01_Projects/Chehalis/Spatial_Model/spatial_model_gitlab/Inputs.gdb'
gdb_out <- 'C://01_Projects/Chehalis/Spatial_Model/spatial_model_gitlab/Outputs.gdb'

sub <- st_read(gdb, 'NOAA_subbasins_w_fp')
stream <- st_read(gdb, 'reaches_20181029_NAD83')
fl <- st_read(gdb_out, 'flowline')
wa  <- st_read('C://01_Projects/Chehalis/Chehalis_GIS/washington/washington.shp')

# Porter gage
usgs_porter <- dataRetrieval::readNWISsite(12031000)
porter_sf <- st_as_sf(usgs_porter, coords = c("dec_long_va", "dec_lat_va"), crs = 4269)

# Chehalis bbox
sub_bb <- st_as_sfc(st_bbox(sub) + c(-8000, -8000, 8000, 8000))

##

fl_plot <- fl %>% 
  mutate(width_bin = cut(wet_width, c(-Inf, 5, 10, 20, 30, 50, Inf))) %>%
  filter(!is.na(width_bin))

p1 <- ggplot() +
  theme_void() +
  
  geom_sf(data = fl_plot,
          aes(size = width_bin), 
              color = 'gray60') + #'dodgerblue4'
  geom_sf(data = sub, 
          #size = 1, 
          color = 'gray20',
          fill = NA) +
  geom_sf(data = porter_sf, 
          shape = 24, #24, 
          fill = 'black', 
          size = 5) +
  scale_size_manual(values = seq(0.1, 1, length.out = 6)) +
  theme(legend.position = "none") +
  annotation_scale(location = "bl", width_hint = .4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_minimal) 


p2 <- ggplot() +
  theme_void() +
  geom_sf(data = wa %>% filter(CARTO_SYMB == 1), fill = NA, color = 'gray50') +
  geom_sf(data = sub , color = 'black', fill = 'black') +
  #geom_sf(data = sub_bb, fill = NA, color = "gray20", size = 1.2) +
  labs(caption = "Washington State, USA") + 
  theme(plot.caption = element_text(hjust = 0.5))


p_full <- ggdraw() +
  draw_plot(p1) +
  draw_plot(p2, x = 0.52, y = 0.65, width = 0.3, height = 0.3)

save_plot('docs/future_flows/Fig1_sitemap.tiff', p_full, compression = 'lzw', dpi = 300)





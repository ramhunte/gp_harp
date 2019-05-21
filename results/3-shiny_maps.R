library(sf)
library(plotly)

# Subbasins shapefile
sub <- st_read(dsn = '//nwcfile/FE/watershed/Chehalis/Data/10-Habitat data layers/SpatialModel_Archive/20190327/Inputs.gdb', layer = 'NOAA_subbasins_w_fp')

buffer <- 1e4 # buffer of X meters
bbox <- st_bbox(sub) + c(-buffer, -buffer, buffer, buffer) # subtract from the xmin and ymin, add to the xmax and ymax

# Path to WA state shapefile
wa_state <- '//nwcfile/FE/watershed/Chehalis/Data/Map files/washington.shp'

# State of Washington -- land only
wa <- st_read(wa_state) %>%
  filter(CARTO_SYMB == 1) %>% # This field means 'land'
  st_transform(crs = st_crs(sub)) %>% # Reproject into the coordinate refrence system of the layer 'sub' (NAD83 Zone 10)
  st_crop(bbox)

# State of Washington -- water only
water <- st_read(wa_state) %>%
  filter(CARTO_SYMB == 4) %>%
  st_transform(st_crs(sub)) %>%  
  st_crop(bbox)

basemap <- ggplot() +
  theme_void() +
  geom_sf(data = water, fill = 'steelblue3') +
  geom_sf(data = wa, fill = 'white') +
  geom_sf(data = sub, fill = 'grey80', color = 'grey50') 

mapping_levels <- as.factor(c(c('< 18°', '18° - 24°','> 24°'), 
                            c('< 10°', '10° - 25°','25° - 50°','50° - 100°','100° - 170°','>170°'),
                            c('0%','0% - 33%','33% - 66%','66% - 100%','100%'), 
                            c('< -2.5°','-2.5° - -1.5°','-1.5° - -0.5°','-0.5° - 0.5°','0.5° - 1.5°','1.5° - 2.5°','> 2.5°')))

fl <- st_read(dsn = '//nwcfile/FE/watershed/Chehalis/Data/10-Habitat data layers/SpatialModel_Archive/20190327/Outputs.gdb', layer = 'flowline') %>%
  gather(species, presence, cohospawn:steelspawn) %>%
  filter(presence == 'Yes') %>% 
  select(-curr_temp, -hist_temp) %>%
  left_join(., flowline %>%
              select(noaaid, temp_diff_2040, temp_diff_2080, curr_temp, hist_temp), by = "noaaid") %>% 
  select(Reach, noaaid, Habitat, GSU,can_ang, hist_ang, pass_tot, temp_diff_2040, temp_diff_2080, curr_temp, hist_temp) %>%
  gather(param, value, can_ang:hist_temp) %>%
  rename(geometry = Shape) %>%
  mutate(value = as.numeric(value),
         level = case_when(
           param %in% c('curr_temp', 'hist_temp') ~
             case_when(
               value < 18 ~  '< 18°',
               value >= 18 & value < 24 ~ '18° - 24°',
               value >= 18 ~ '> 24°'),
           param %in% c('can_ang', 'hist_ang') ~
             case_when(
               value < 10 ~ '< 10°',
               value >= 10 & value < 25 ~ '10° - 25°',
               value >= 25 & value < 50 ~ '25° - 50°',
               value >= 50 & value < 100 ~ '50° - 100°',
               value >= 100  ~ '> 100°'
             ),
           param == 'pass_tot' ~
             case_when(
               value == 0 ~ '0%',
               value > 0 & value <= .33 ~ '0% - 33%',
               value > .33 & value <= .66 ~ '33% - 66%',
               value > .66 & value < 1 ~ '66% - 100%',
               value == 1 ~ '100%'
             ),
           param %in% c('temp_diff_2040', 'temp_diff_2080') ~
             case_when(
               value < -2.5 ~ '< -2.5°',
               value >= -2.5 & value < -1.5 ~ '-2.5° - -1.5°',
               value >= -1.5 & value < -.5 ~ '-1.5° - -0.5°',
               value >= -.5 & value < .5 ~ '-0.5° - 0.5°',
               value >= .5 & value < 1.5 ~ '0.5° - 1.5°',
               value >= 1.5 & value < 2.5 ~ '1.5° - 2.5°',
               value >= 2.5 ~ '> 2.5°'
             )),
         level = factor(level, levels = mapping_levels),
         map_color = case_when(
           level %in% c('< 18°', '< 10°', '100%', '< -2.5°') ~ '#00008B',
           level %in% c('10° - 25°','66% - 100%', '-2.5° - -1.5°') ~ '#0000CD',
           level %in% c('-1.5° - -0.5°') ~ '#87CEFA',
           level %in% c('-0.5° - 0.5°') ~ '#808080',
           level %in% c('18° - 24°', '25° - 50°', '33% - 66%', '0.5° - 1.5°') ~ '#FFFF00',
           level %in% c('50° - 100°', '0% - 33%', '1.5° - 2.5°') ~ '#FF8C00',
           level %in% c('> 24°', '> 100°', '0%', '> 2.5°') ~ '#FF0000',
           is.na(level) ~ '#FFFFFF')
           ) 

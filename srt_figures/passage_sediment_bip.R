# Create habitat metrics for SRT meeting

# Barriers, Beaver, Finesediment



# Spatial data

# Access files on the network
# gdb_in <- '//nwcfile/FE/watershed/Chehalis/Data/10-Habitat data layers/SpatialModel_Archive/20200512/Inputs.gdb'
# gdb_out <- '//nwcfile/FE/watershed/Chehalis/Data/10-Habitat data layers/SpatialModel_Archive/20200512/Outputs.gdb'

# Files on Colin's local machine
gdb_in <- 'C:/01_Projects/Chehalis/Spatial_Model/spatial_model_gitlab/Inputs.gdb'
gdb_out <- 'C:/01_Projects/Chehalis/Spatial_Model/spatial_model_gitlab/Outputs.gdb'


shp_gsu <- sf::st_read(gdb_in, 'chehalis_gsu_20180919_NAD83')

shp_sub <- sf::st_read(gdb_in, 'NOAA_subbasins_w_fp')

gsu_list <- shp_gsu %>%
  sf::st_drop_geometry %>%
  select(GSU)

# Barriers ---- 

source('hab/R_files/2-read_in_data.R')

fl <- flowline %>% 
  select(noaaid, GSU, Reach,culv_list) %>%
  filter(culv_list != "") %>%
  mutate(culv_list = str_split(culv_list,',')) %>%
  unnest(culv_list) %>%
  filter(culv_list != 'None') %>%
  mutate(noaa_culv = as.numeric(culv_list)) %>%
  left_join(., culvs %>%
              select(-GSU)) %>%
  group_by(GSU, noaaid) %>%
  summarize(pass_tot = prod(FishPass),
            pass_tot_natural = prod(ifelse(FeatureTyp == 'Natural',FishPass,1))) %>%
  replace_na(list(pass_tot = 1, pass_tot_natural = 1))


gsu_pass <- fl %>% 
  group_by(GSU) %>%
  summarize(pass_tot = mean(pass_tot, na.rm = TRUE),
            pass_tot_nat = mean(pass_tot_natural, na.rm = TRUE)) %>%
  mutate(pass_diff = pass_tot - pass_tot_nat) %>%
  right_join(gsu_list) %>%
  mutate(pass_diff = ifelse(is.na(pass_diff), 0, pass_diff),
         pass_diff = ifelse(GSU == "Upper Skookumchuck", NA, pass_diff))



# Finesediment
flowline %>% filter(GSU == 'Cloquallum Cr')

gsu_sed <- flowline %>%
  group_by(GSU) %>%
  summarize(sed_current = mean(sed_current, na.rm = TRUE),
            sed_hist = mean(sed_hist, na.rm = TRUE)) %>%
  mutate(sed_diff = abs(sed_hist - sed_current))


# Or if do we want road density?


# Beaver

gsu_bip <- flowline %>%
  mutate(score_slope = 
           case_when(
             slope < .01 ~ 4,
             between(slope, .01, .02) ~ 3,
             between(slope, .02, .04) ~ 2,
             between(slope, .04, .06) ~ 1,
             between(slope, .06, .10) ~ 0.5,
             slope > .10 ~ 0
           ),
         score_width = case_when(
           BF_width < 7 ~ 4,
           between(BF_width, 7, 10) ~ 3,
           between(BF_width, 10, 18) ~ 2,
           between(BF_width, 18, 24) ~ 1,
           BF_width > 24 ~ 0),
         score = score_slope + score_width) %>%
  #select(GSU, noaaid, slope, BF_width, score_slope, score_width, score)
  group_by(GSU) %>%
  summarize(BIP = mean(score, na.rm = TRUE))


# Quick plots

shp_gsu_plt <- shp_gsu %>%
  left_join(gsu_pass) %>%
  left_join(gsu_sed) %>%
  left_join(gsu_bip)


# Test
# ggplot() +
#    geom_sf(data = shp_gsu_plt, aes(fill =  GSU), show.legend = FALSE)



# Barriers

ggplot() +
  theme_void() +
  geom_sf(data = shp_sub, color = 'black', lwd = 0.1) +
  geom_sf(data = shp_gsu_plt, aes(fill = pass_diff), lwd = 0.1, color = 'black') +
  #scale_fill_viridis_c(label = label_percent()) +
  scale_fill_gradient2(high = 'white', 
                       low = muted('orange'),
                       mid = 'orange',
                       midpoint = -.5,
                       label = label_percent(),
                       limits = c(-1, 0)) +
  labs(fill = 'Change in\npassage') +
  theme(legend.position = c(.8, .8))

ggsave('gsu_passage.tiff',
       height = 4,
       width = 6.5,
       dpi = 500,
       compression = "lzw")

# sediment

plot.params <- read.csv('lcm/data/scenarios.csv') %>% 
  select(scenario, scenario.label, color) %>%
  mutate_if(is.factor, as.character) %>%
  rowid_to_column()


ggplot() +
  theme_void() +
  geom_sf(data = shp_sub, color = 'black', lwd = 0.1) +
  geom_sf(data = shp_gsu_plt, aes(fill =  sed_diff/100), lwd = 0.1, color = 'black') +
  #scale_fill_viridis_c(label = label_percent()) +
  scale_fill_gradient2(low = 'white', 
                       high = muted('pink'),
                       mid = 'pink',
                       midpoint = .05,
                       label = label_percent()) +
  labs(fill = 'Change in\nfines') +
  theme(legend.position = c(.8, .8))

ggsave('gsu_fine_sed.tiff',
       height = 4,
       width = 6.5,
       dpi = 500,
       compression = "lzw")




# BIP
ggplot() +
  theme_void() +
  geom_sf(data = shp_sub, color = 'black', lwd = 0.1) +
  geom_sf(data = shp_gsu_plt, aes(fill = BIP), lwd = 0.1, color = 'black') +
  #scale_fill_viridis_c(label = label_percent()) +
  scale_fill_gradient2(low = 'white', 
                       high = muted('green4'),
                       mid = 'green4',
                       midpoint = 7, 
                       limits = c(0, 10)) +
  labs(fill = 'BIP score') +
  theme(legend.position = c(.8, .8))

ggsave('gsu_BIP.tiff',
       height = 4,
       width = 6.5,
       dpi = 500,
       compression = "lzw")

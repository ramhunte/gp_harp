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
  summarize(pass_tot = mean(pass_tot),
            pass_tot_nat = mean(pass_tot_natural)) %>%
  mutate(pass_diff = pass_tot_nat - pass_tot)



# Finesediment

gsu_sed <- flowline %>%
  group_by(GSU) %>%
  summarize(sed_current = mean(sed_current),
            sed_hist = mean(sed_hist)) %>%
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
  summarize(BIP = mean(score))


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
  geom_sf(data = shp_gsu_plt, aes(fill = pass_diff)) +
  scale_fill_viridis_c()


# sediment
ggplot() +
  theme_void() +
  geom_sf(data = shp_gsu_plt, aes(fill =  sed_diff)) +
  scale_fill_viridis_c()



# BIP
ggplot() +
  theme_void() +
  geom_sf(data = shp_gsu_plt, aes(fill = BIP)) +
  scale_fill_viridis_c()

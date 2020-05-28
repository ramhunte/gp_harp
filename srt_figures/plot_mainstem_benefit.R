
read_and_bind_files <- function(dir, pattern) {
  file_paths <- list.files('srt_figures', 
                          pattern = 'restore_per_sub',
                          full.names = TRUE)
  
  out_df <- file_paths %>%
    map_dfr(.id = 'species', read.csv) %>%
    select(-X) %>%
    mutate(species = case_when(
      species == 1 ~ 'Coho',
      species == 2 ~ 'Fall Chinook',
      species == 3 ~ 'Spring Chinook',
      species == 4 ~ 'Steelhead'),
    Subbasin_num = str_remove(subbasin, 'X') %>% 
      as.integer()
    )
  
  return(out_df)

}


restore_ms_df <- read_and_bind_files('srt_figures', 'restore_per_sub')


## Spatial data

gdb_in <- 'C:/01_Projects/Chehalis/Spatial_Model/spatial_model_gitlab/Inputs.gdb'


# Subbasin names and EDRs
Subbasin_names <- read.csv('../lcm/data/Subbasin_names.csv') %>%
  mutate(EcoRegion = ifelse(str_detect(Subbasin, 'Estuary'),
                            'Estuary', 
                            as.character(EcoRegion)))

# Plot colors and labels
plot.params <- read.csv('../lcm/data/scenarios.csv') %>% 
  select(scenario, scenario.label, color) %>%
  mutate_if(is.factor, as.character) %>%
  rowid_to_column()


# Read in subbasin layer
sub_shp <- st_read(dsn = gdb_in, layer = 'NOAA_subbasins_w_fp') %>%
  rename(Subbasin_num = noaa_sub_num) %>%
  left_join(Subbasin_names)




ms_shp <- sub_shp %>%
  left_join(restore_ms_df) %>%
  filter(Subbasin_num %in% 52:63, 
         str_detect(scenario, 'LR|FP|Hist', negate = TRUE)) %>%
  mutate(diff = ifelse(is.na(diff), 0, diff))

sub_crop <- sub_shp %>%
  st_crop(ms_shp %>% st_bbox())




plot_ms_restoration_benefit <- function(s) {
  shp_plt <- ms_shp %>% filter(species == s)
  
  p <- ggplot() +
    theme_void() +
    geom_sf(data = shp_plt, 
            aes(fill = diff), 
            color = 'black') +
    geom_sf(data = sub_crop, fill = NA, size = 0.2) +
    facet_wrap(~scenario) +
    scale_fill_viridis_c() +
    labs(fill = 'Basinwide\nspawner\nincrease')
  
  folder_name <- 'srt_figures/Figs_restore_ms'
  
  if (dir.exists(folder_name) == F) {dir.create(folder_name)}
  
  ggsave(file.path(folder_name,paste0(s,'.tiff')), 
         p,
         height = 4,
         width = 6.5,
         dpi = 500,
         compression = "lzw")
  
}


lapply(unique(restore_ms_df$species), plot_ms_restoration_benefit)

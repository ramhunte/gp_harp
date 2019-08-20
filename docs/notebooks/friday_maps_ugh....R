

gdb_in <- '//nwcfile/FE/watershed/Chehalis/Data/10-Habitat data layers/SpatialModel_Archive/20190604/Inputs.gdb'
gdb_out <- '//nwcfile/FE/watershed/Chehalis/Data/10-Habitat data layers/SpatialModel_Archive/20190604/Outputs.gdb'

sub <- st_read(dsn = gdb_in,
               layer = 'NOAA_subbasins_w_fp')

gsu <- st_read(dsn = gdb_in,
               layer = 'chehalis_gsu_20180919_NAD83')

buffer <- 1e4 
bbox <- st_bbox(sub) + c(-buffer, -buffer, buffer, buffer)

wa_state <- '//nwcfile/FE/watershed/Chehalis/Data/Map files/washington.shp'
wa <- st_read(wa_state) %>%
  filter(CARTO_SYMB == 1) %>%
  st_transform(crs = st_crs(sub)) %>%
  st_crop(bbox)
water <- st_read(wa_state) %>%
  filter(CARTO_SYMB == 4) %>%
  st_transform(crs = st_crs(sub)) %>%
  st_crop(bbox)

fl <- st_read(dsn = gdb_out,
              layer = 'flowline')

fl_schino <- fl %>%
  filter(sprspawn == 'Yes' | (noaa_sub_num %in% 52:63 & Habitat == 'LgRiver')) %>%
  select(noaaid)




# 2019 temperature ------------------

temp_breaks <- c(16, 18, 20, 22.6)

temp_plt <- fl_schino %>%
  left_join(flowline) %>%
  mutate(prespawn_temp_bin = cut(prespawn_temp, 
                                      c(-Inf,temp_breaks,Inf), 
                                      na.rm=T,
                                      dig.lab =10))



jpeg('map_jpegs/2019_temps.jpeg', width = 8, height = 6, units = 'in', res = 300)
plot(temp_plt['prespawn_temp_bin'], 
     main = 'Scenario 1 2040 temperature',
     reset = FALSE,
     key.pos = 1, 
     lwd = 2, 
     pal = rev(heat.colors(5)))
plot(st_geometry(sub), add = TRUE)
dev.off()

layout(matrix(1:2, ncol = 2), widths = c(1, lcm(2)))
plot(st_geometry(sub), col = 'grey80')
plot(temp_plt['prespawn_temp_bin'], 
     main = 'Scenario 1 2040 temperature',
     reset = FALSE,
     key.pos = 1, 
     lwd = 2, 
     pal = colorRampPalette(colorschemes$BluetoOrangeRed.14, space = "Lab")(5),
     add = TRUE)
.image_scale(c(0,temp_breaks,40), 
             col = colorRampPalette(colorschemes$BluetoOrangeRed.14, space = "Lab")(5), 
             key.length = lcm(8), 
             key.pos = 4, 
             at = c(0,temp_breaks,40),
             las = 1)

# Scenario 3 2080 temperature  ---------------------------

temp_breaks <- c(16, 18, 20, 22.6)

fl_2080 <- fl_schino %>%
  left_join(., asrp_mapping %>%
              filter(Scenario_num == 'scenario_3',
                     year == 2080)) %>%
  mutate(prespawn_temp_asrp_bin = cut(prespawn_temp_asrp, 
                                 c(-Inf,temp_breaks,Inf), 
                                 na.rm=T,
                                 dig.lab =10))


jpeg('map_jpegs/scenario_3_2080_temp.jpeg', width = 8, height = 6, units = 'in', res = 300)
plot(fl_2080['prespawn_temp_asrp_bin'],
     main = 'Scenario 3 2080 temperature',
     reset = FALSE,
     key.pos = 1, 
     lwd = 2, 
     pal = rev(heat.colors(5)))
plot(st_geometry(sub), add = TRUE)
dev.off()






# 'Scenario 3 2080 temperature change' -----------------
temp_breaks <- seq(-2, 3, 1)

fl_2080_diff <- fl_schino %>%
  left_join(., asrp_mapping %>%
              filter(Scenario_num == 'scenario_3',
                     year == 2080)) %>%
  mutate(ps_diff = temp_diff_2080 * prespawn_temp_slope - prespawn_temp_intercept,
         ps_diff_bin = cut(ps_diff, 
                                 c(-Inf,temp_breaks,Inf), 
                                 na.rm=T,
                                 dig.lab =10))



jpeg('map_jpegs/scenario_3_2080_temp_chg.jpeg', width = 8, height = 6, units = 'in', res = 300)
plot(fl_2080_diff['ps_diff_bin'],
     main = 'Scenario 3 2080 temperature change',
     reset = FALSE,
     key.pos = 1, 
     lwd = 2, 
     pal = topo.colors(13))
plot(st_geometry(sub), add = TRUE)
dev.off()

fl_2080_diff$ps_diff %>% hist(main = 'Temperature difference Scenario 3 2080 - current')


# 'Scenario 1 2080 temperature' ----------

fl_2080_1 <- fl_schino %>%
  left_join(., asrp_mapping %>% 
              filter(Scenario_num == 'scenario_1',
                     year == 2080)) 

jpeg('map_jpegs/scenario_1_2080_temp.jpeg', width = 8, height = 6, units = 'in', res = 300)
plot(fl_2080_1['prespawn_temp_asrp'],
     main = 'Scenario 1 2080 temperature',
     reset = FALSE,
     key.pos = 1, 
     lwd = 2, 
     pal = heat.colors)
plot(st_geometry(sub), add = TRUE)
dev.off()

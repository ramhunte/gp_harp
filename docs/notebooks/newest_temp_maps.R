# Purpose: create a set of output maps for use in the report

library(sf)
library(tidyverse)
library(colorspace)

gdb_in <- '//nwcfile/FE/watershed/Chehalis/Data/10-Habitat data layers/SpatialModel_Archive/20190604/Inputs.gdb'
gdb_out <- '//nwcfile/FE/watershed/Chehalis/Data/10-Habitat data layers/SpatialModel_Archive/20190604/Outputs.gdb'

sub <- st_read(dsn = gdb_in, layer = 'NOAA_subbasins_w_fp') 

gsu <- st_read(dsn = gdb_in, layer = 'chehalis_gsu_20180919_NAD83') %>%
  st_geometry()

buffer <- 1e4 
bbox <- st_bbox(sub) + c(-buffer, -buffer, buffer, buffer)

wa_state <- '//nwcfile/FE/watershed/Chehalis/Data/Map files/washington.shp'

wa <- st_read(wa_state) %>%
  filter(CARTO_SYMB == 1) %>%
  st_transform(crs = st_crs(sub)) %>%
  st_crop(bbox) %>%
  st_geometry()

water <- st_read(wa_state) %>%
  filter(CARTO_SYMB == 4) %>%
  st_transform(crs = st_crs(sub)) %>%
  st_crop(bbox) %>%
  st_geometry()


fl <- st_read(dsn = gdb_out,
              layer = 'flowline') %>%
  filter(cohospawn == 'Yes' | sprspawn == 'Yes'| fallspawn == 'Yes' | steelspawn == 'Yes') %>%
  select(noaaid) %>%
  inner_join(flowline) 

# 2080 temperature difference (Historical - Current) ----


temp_breaks <- c(-Inf, -2.5, -1.5, -.5, .5, 1.5, 2.5, Inf)

temp_diff_cols <- c('navy', 'blue', 'dodgerblue', 'white', 'yellow', 'orange', 'red')

temp_diff_plt <- fl %>%
  mutate(temp_diff_bin = cut(temp_diff_2080,
                            temp_breaks,
                             na.rm = T,
                             dig.lab = 10))

levels(temp_diff_plt$temp_diff_bin) <- c(levels(temp_diff_plt$temp_diff_bin) %>% 
                                           gsub("]|\\(", "",.) %>% 
                                           sub(","," - ",.) )

jpeg('Tm_Chg_2080.jpeg', width = 10, height = 10, unit = 'in', res = 300)
plot(water, col = 'steelblue3', main = '2080 Temperature Difference from Current', reset = FALSE)
plot(wa, col = 'white', add = TRUE)
plot(sub, col = 'lightgrey', add = TRUE)
plot(temp_diff_plt['temp_diff_bin'], 
     reset = FALSE,
     key.pos = 1,
     lwd = 2,
     pal = temp_diff_cols,
     add = TRUE)
legend(x = 551386, y = 5273509,
       xjust = 1,
       yjust = 1,
       legend = rev(unique(temp_diff_plt$temp_diff_bin)),
       bg = 'lightgrey',
       lty = c(1, 1, 1, 1, 1, 1, 1),
       col = temp_diff_cols,
       lwd = 3,
       # y.intersp = .75,
       cex = .85,
       title = 'Temp Chg (°C)')
dev.off()

# Percent passage ----
spawners_sub <- read.csv('outputs/coho/lcm/coho_abundance_by_subbasin.csv') %>%
  select(natal.basin, spawners, scenario) %>%
  rename(Subbasin = natal.basin) %>%
  spread(scenario, spawners) %>%
  mutate(diff = Barriers - Current,
         perc_diff = (diff/Current) * 100) %>%
  select(Subbasin, diff, perc_diff) %>%
  full_join(., subbasins) %>%
  rename(noaa_sub = Subbasin)

sub_breaks <- c(-Inf, 10, 25, Inf)

sub_cols <- c('lightgrey', 'orange', 'orange3')

sub_plt <- sub %>%
  full_join(., spawners_sub) %>%
  mutate(perc_diff = ifelse(is.na(perc_diff),
                            0,
                            perc_diff),
         sub_bin = cut(perc_diff,
                       sub_breaks,
                       na.rm = T,
                       dig.lab = 10))

levels(sub_plt$sub_bin) <- c(levels(sub_plt$sub_bin) %>%
                                 gsub("]|\\(", "",.) %>%
                                 sub(","," - ",.) )



jpeg('Benefit of barrier removal.jpeg', width = 10, height = 10, unit = 'in', res = 300)

plot(water, col = 'steelblue3', main = 'Percent Benefit of Barrier Removal', reset = FALSE)
plot(wa, col = 'white', add = TRUE)
plot(sub_plt['sub_bin'],
     pal = sub_cols,
     add = TRUE)
legend(x = 551386, y = 5273509,
       xjust = 1,
       yjust = 1,
       legend = unique(sub_plt$sub_bin),
       bg = 'white',
       # fill = sub_cols,
       lty = c(1, 1, 1),
       col = sub_cols,
       lwd = 3,
       # y.intersp = .75,
       # cex = .85,
       title = 'Percent Benefit')
dev.off()

# Percent passage ----

sub_breaks_2 <- c(-Inf, 10, Inf)

sub_cols_2 <- c('lightgrey', 'darkgrey')

sub_plt_2 <- sub %>%
  full_join(., spawners_sub) %>%
  mutate(perc_diff = ifelse(is.na(perc_diff),
                            0,
                            perc_diff),
         sub_bin_2 = cut(perc_diff,
                       sub_breaks_2,
                       na.rm = T,
                       dig.lab = 10))

levels(sub_plt_2$sub_bin_2) <- c(levels(sub_plt_2$sub_bin_2) %>%
                               gsub("]|\\(", "",.) %>%
                               sub(","," - ",.) )

pass_breaks <- c(0, 20, 40, 60, 80, 100)

pass_cols <- c('navy', 'dodgerblue', 'yellow', 'orange', 'red')

pass_plt <- fl %>%
  mutate(pass_tot = as.numeric(pass_tot * 100),
         pass_bin = cut(pass_tot,
                        pass_breaks,
                        na.rm = T,
                        dig.lab = 10))

levels(pass_plt$pass_bin) <- c(levels(pass_plt$pass_bin) %>%
                                 gsub("]|\\(", "",.) %>%
                                 sub(","," - ",.) )


jpeg('Passage percentage.jpeg', width = 10, height = 10, unit = 'in', res = 300)
plot(water, col = 'steelblue3', main = 'Percent Passage', reset = FALSE)
plot(wa, col = 'white', add = TRUE)
plot(sub_plt_2['sub_bin_2'], pal = sub_cols_2, add = TRUE)
plot(pass_plt['pass_bin'],
     reset = FALSE,
     key.pos = 1,
     lwd = 2,
     pal = rev(pass_cols),
     add = TRUE
)
legend(x = 551386, y = 5273509,
       xjust = 1,
       yjust = 1,
       legend = levels(pass_plt$pass_bin),
       bg = 'white',
       lty = c(1, 1, 1, 1, 1),
       col = rev(pass_cols),
       lwd = 3,
       # y.intersp = .75,
       cex = .85,
       title = 'Percent passage')
dev.off()



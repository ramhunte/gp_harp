# Purpose: create a set of output maps for use in the report

library(sf)
library(tidyverse)
library(colorspace)

gdb_in <- '//nwcfile/FE/watershed/Chehalis/Data/10-Habitat data layers/SpatialModel_Archive/20190604/Inputs.gdb'
gdb_out <- '//nwcfile/FE/watershed/Chehalis/Data/10-Habitat data layers/SpatialModel_Archive/20190604/Outputs.gdb'

sub <- st_read(dsn = gdb_in, layer = 'NOAA_subbasins_w_fp') %>%
  st_geometry()

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
nclr <- 7
class <- classIntervals(fl$temp_diff,  
                        nclr,
                        style = 'fixed',
                        fixedBreaks = temp_breaks)


temp_diff_cols <- c('navy', 'blue', 'dodgerblue', 'white', 'yellow', 'orange', 'red')

colcode <- findColours(class, temp_diff_cols)

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
plot(temp_diff_plt['temp_diff_2080'], 
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




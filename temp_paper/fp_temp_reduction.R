floodplain_5m <- read.delim('temp_paper/floodplain_5m.txt', header = TRUE, sep = ",") %>%
  filter(HabUnit == 'Marsh') %>%
  group_by(noaaid, Period) %>%
  summarize(Area = sum(Area_ha, na.rm = T)) %>%
  spread(Period, Area) %>%
  filter(Hist > 0,
         # Curr != Hist)
         is.na(Curr) | (Hist-Curr)/Hist > .50)
ss_fp_reconnect <- unique(floodplain_5m$noaaid)
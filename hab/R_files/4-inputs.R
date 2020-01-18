mainstem.subs = c(52:63) # This is a list of all mainstem subbasins

#change in future temperatures due to climate change.  See temp conversion doc for more details
if (fishtype %in% c('spring_chinook', 'fall_chinook')) {
  cc_mid_rear = .9585
  cc_late_rear = 1.917
} else {
  cc_mid_rear = 1.024
  cc_late_rear = 2.049
}
cc_mid_prespawn = 1.024
cc_late_prespawn = 2.049

bw_scalar = .16 # see Tim's description in Trello card, V4

wood_spawn_mult = 1.3 # spawning gravel multiplier for hist wood scenarios

schino_subs = c(1, 3, 5, 12, 18, 52:63)

hist_beaver_mult = .85 # This comes from the fact that in historical beaver scenarios, beaver ponds take up 15% of all trib reaches
hist_pond_area_per_m = 3 # m^2 per m
curr_beaver_mult = .98625 # This comes from the fact that in current beaver scenarios, beaver ponds take up 1.375% of all trib reaches
curr_pond_area_per_m = .3

# Create list of Diagnostic scenarios
diag_scenarios <- scenarios %>%
  filter(substr(scenario, 1, 4) != 'ASRP') %>%
  pull(scenario) %>%
  as.character() %>%
  gsub('\\.', '_', .) # replace . with _

mainstem_reaches <- unique(grep("^Chehalis-", flowline$Reach, value = T))

winter_pool_scalar_warm = .3

LgRiver_habs = c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater")
SmStream_habs = c("Pool", "Riffle", "Beaver.Pond")
Floodplain_habs = c("FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm", "Side_Channel")

psp_lwls = 12.5 # pool spacing for low wood, low slope reaches
psp_hwls = 6.25 # pool spacing for high wood, low slope reaches
psp_lwhs = 27.5 # pool spacing for low wood, high slope reahces
psp_hwhs = 5 # pool spacing for high wood, high slope reaches


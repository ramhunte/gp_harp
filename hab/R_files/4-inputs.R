# winter_pool_scalar_cold = .14 # Used for converting some pool habitat to riffle in winter.  Neet to confirm whether this is used for all species
# winter_pool_scalar_warm = .43 # we use the warm winter pool scalar

mainstem.subs = c(52:63) # This is a list of all mainstem subbasins

mainstem.reaches.num = c(1:57)  # Used for isolating all mainstem Chehalis reaches.  This was used for the mainstem temperature mask but is not currently being used.  Will keep it 
mainstem.reaches = lapply(mainstem.reaches.num, function(x){ # around in case we decide to add the mainstem temperature mask back in 
  paste0("Chehalis-", x)}) %>%
  do.call('rbind',.)

# cc_mid_rear = 1.8
# cc_late_rear = 3
# cc_mid_prespawn = 1.8
# cc_late_prespawn = 3

#change in future temperatures due to climate change.  See temp conversion doc for more details
if (fishtype %in% c('spring_chinook', 'fall_chinook')) {
cc_mid_rear = .9585
cc_late_rear = 1.917
} else {
cc_mid_rear = 1.024
cc_late_rear = 2.049
}
cc_mid_prespawn = .9265
cc_late_prespawn = 1.853

bw_scalar = .16 # see Tim's description in Trello card, V4

wood_spawn_mult = 1.3 # spawning gravel multiplier for hist wood scenarios

prespawn_temp_slope = .90223570 # used to convert 7DADM to MDM
prespawn_temp_intercept = .06201682 # used to convert 7DADM to MDM

schino_subs = c(1, 3, 5, 12, 18, 52:63)
# schino_mult = .19 # fall chinook and spring chinook multipliers are used when both spring and fall chinook exist within the same reach
# fchino_mult = .81

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


# List of all scenarios (note: LCM uses a different habitat scenario format)
all_scenarios <- scenarios %>%
  pull(scenario) %>%
  as.character() %>%
  gsub('\\.', '_', .) # replace . with _

asrp_scenario_names <- scenarios %>%
  filter(substr(scenario, 1, 4) == 'ASRP') %>%
  pull(scenario) %>%
  as.character() %>% 
  gsub('\\.', '_',.)

mainstem_reaches <- unique(grep("^Chehalis-", flowline$Reach, value = T))

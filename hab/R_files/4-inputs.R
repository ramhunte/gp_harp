# winter_pool_scalar_cold = .14 # Used for converting some pool habitat to riffle in winter.  Neet to confirm whether this is used for all species
# winter_pool_scalar_warm = .43 # we use the warm winter pool scalar

mainstem.subs = c(52:63) # This is a list of all mainstem subbasins

mainstem.reaches.num = c(1:57)  # Used for isolating all mainstem Chehalis reaches.  This was used for the mainstem temperature mask but is not currently being used.  Will keep it 
mainstem.reaches = lapply(mainstem.reaches.num, function(x){ # around in case we decide to add the mainstem temperature mask back in 
  paste0("Chehalis-", x)}) %>%
  do.call('rbind',.)

cc_2040 = 1.8 # projected magnitude of increase in temperature by 2040
cc_2080 = 3 #projected magnitude of increase in temperature by 2080


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

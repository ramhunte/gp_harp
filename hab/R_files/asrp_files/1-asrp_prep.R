# Prep work to begin asrp scenarios: ----
# 1. Prepare asrp scenarios data frame
# 2. Replicate habitat data to assign unique combinations of year (2019, 2040, 2080) and scenario (Current, 1-3).
# 3. Generate list of primary creeks

# Prep asrp_scenarios data frame.  Convert columns to character form for compatibility later in process ----

asrp_scenarios_char <- asrp_scenarios_raw %>%
  select(-EDR, -Analog_name, -Notes) %>%
  mutate(LW = as.character(LW),
         Barriers = as.character(Barriers),
         Floodplain = as.character(Floodplain),
         Beaver = as.character(Beaver),
         Riparian = as.character(Riparian),
         primary_cr_only = as.character(primary_cr_only),
         managed_forest = as.character(managed_forest))

if (run_single_action == 'yes') {
  single_action_list <- c('wood_only', 'beaver_only', 'fp_only', 'barrier_only', 'riparian_only', 'no_climate_chg')
} else {
  single_action_list <- c('wood_only', 'beaver_only', 'fp_only')
}

  asrp_scenarios <- lapply(single_action_list, function(q) {
    asrp_scenarios_char %>%
      mutate(Scenario_num_2 = q) %>%
      unite(Scenario_num, Scenario_num, Scenario_num_2, sep = '_')
  }) %>%
    do.call('rbind',.) %>%
    bind_rows(asrp_scenarios_char)
  
# Create single dataframe with all habitat data included

all_habs <- bind_rows(asrp_bw_raw, asrp_fp_raw, asrp_lr_raw, asrp_ss_raw %>%
                        mutate(Habitat = 'SmStream')) %>%
  select(-Wtrbdy_wau, -Area_km2, -HabUnit, -NEAR_FID, -ET_ID, -noaaid_lr, -OID, -Join_Count, -TARGET_FID, -Basin_wau, Shape_Length_1, -noaa_sub,
         -noaa_sub_num, -source_hab, -temp_diff, -curr.tempmult, -hist.tempmult)

# Attach year column to dataset

scenario.years <- c(2040, 2080, 2019) # Note:  We use 2019 as the current year for the Current_asrp scenario

all_habs_year <- lapply(scenario.years, function(g) {
  all_habs %>%
    mutate(year = g)
  
}) %>%
  do.call('rbind',.)

# Attach scenario numbers to habitat data and filter to only those scenarios which we wish to run

scenario.nums <- c(unique(asrp_scenarios$Scenario_num), "Current_asrp", 'dev_and_climate', 'floodplain_hist') 
single_action_scenarios <- c(unique(asrp_scenarios$Scenario_num[!asrp_scenarios$Scenario_num %in% c('scenario_1', 'scenario_2', 'scenario_3')]), 'dev_and_climate')
growth_scenarios <- c('scenario_1_riparian_only', 'scenario_2_riparian_only','scenario_3_riparian_only', 'scenario_1_no_climate_chg',
                      'scenario_2_no_climate_chg', 'scenario_3_no_climate_chg')

all_habs_scenario <- lapply(scenario.nums, function(h) {
  all_habs_year %>%
    mutate(Scenario_num = h)
  }) %>%
  do.call('rbind',.) %>%
  filter(!(year == 2019 & Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", growth_scenarios)),
         !(Scenario_num %in% c(single_action_scenarios[!single_action_scenarios %in% growth_scenarios], 'floodplain_hist') & 
             year %in% c(2040, 2080)),
         !(is.na(Shape_Length) & Area_ha == 0))


# Create list of primary creeks to be used when asrp scenarios call for restoration of primary creek only ----

primary_cr_list <- c("Decker-", "Bingham-", "Cedar-", "Sherman-", "Elk Cr-", "Crim Cr-", "Stillman-", "Big (Hump)-", "Stevens-", "Johns-", "Cloquallum-", "Porter-", "Scatter Cr-",
                     "Beaver Cr-", "Lincoln Cr-", "Elk Cr-", "Elk Cr-", "Dry Run-", "Black (Wynoochee)-", "(Wynoochee Resevoir)", "Wynoochee-", "Waddell Cr-", "Garrard Cr-",
                     "Bunker-", "Skookumchuck-", "Hanaford-", "Sterns Cr-", "Thrash Cr-", "Lake Cr-")

primary_cr <- c(lapply(primary_cr_list, function(z) {
  c(grep(z, flowline$Reach, value = TRUE))
})) %>%
  unlist()

rm(all_habs)
rm(all_habs_year)
rm(all_scenarios_char)

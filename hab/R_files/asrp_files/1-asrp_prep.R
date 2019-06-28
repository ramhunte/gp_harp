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
         primary_cr_only = as.character(primary_cr_only))

  asrp_scenarios <- asrp_scenarios_char %>%
    bind_rows(., asrp_scenarios_char %>%
              mutate(LW = as.character(LW),
                     Barriers = 'n',
                     Floodplain = 'n',
                     Beaver = 'n',
                     Riparian = 'n',
                     primary_cr_only = as.character(primary_cr_only),
                     Scenario_num_2 = "wood_only") %>%
                unite(Scenario_num, Scenario_num, Scenario_num_2, sep = "_")) %>%
  bind_rows(., asrp_scenarios_char %>%
              mutate(LW = 'n',
                     Barriers = 'n',
                     Floodplain = as.character(Floodplain),
                     Beaver = 'n',
                     Riparian = 'n',
                     primary_cr_only = as.character(primary_cr_only),
                     Scenario_num_2 = "fp_only") %>%
              unite(Scenario_num, Scenario_num, Scenario_num_2, sep = "_")) %>%
  bind_rows(., asrp_scenarios_char %>%
              mutate(LW = 'n',
                     Barriers = 'n',
                     Floodplain = 'n',
                     Beaver = as.character(Beaver),
                     Riparian = 'n',
                     primary_cr_only = as.character(primary_cr_only),
                     Scenario_num_2 = "beaver_only") %>%
              unite(Scenario_num, Scenario_num, Scenario_num_2, sep = "_"))

# Attach year and scenario num columns to habitat data sets ---- 

# Assign habitat for SmStream data.  LgRiver and Floodplain already have Habitat field attached.  

asrp_ss_raw %<>% 
  mutate(Habitat = "SmStream")

all_habs <- bind_rows(asrp_bw_raw, asrp_fp_raw, asrp_lr_raw, asrp_ss_raw)

# Attach year column to dataset

scenario.years <- c(2040, 2080, 2019) # Note:  We use 2019 as the current year for the Current_asrp scenario

all_habs_year <- lapply(scenario.years, function(g) {
  all_habs %>%
    mutate(year = g)
  
}) %>%
  do.call('rbind',.)

# Attach scenario numbers to habitat data and filter to only those scenarios which we wish to run

scenario.nums <- c(unique(asrp_scenarios$Scenario_num), "Current_asrp", 'dev_and_climate') # Note:  Current is only run to compare the asrp scenario outputs with the diagnostics
                                                                                # scenario code to ensure that the asrp scenario code is working properly
all_habs_scenario <- lapply(scenario.nums, function(h) {
  all_habs_year %>%
    mutate(Scenario_num = h)
  }) %>%
  do.call('rbind',.) %>%
  filter(!(year == 2019 & Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", 'dev_and_climate')),
         !(Scenario_num %in% c("scenario_1_wood_only", "scenario_2_wood_only", "scenario_3_wood_only", "scenario_1_fp_only", "scenario_2_fp_only", 
                               "scenario_3_fp_only", "scenario_1_beaver_only",  "scenario_2_beaver_only", "scenario_3_beaver_only") & 
             year %in% c(2040, 2080)))


# Create list of primary creeks to be used when asrp scenarios call for restoration of primary creek only ----

primary_cr_list <- c("Decker-", "Bingham-", "Cedar-", "Sherman-", "Elk Cr-", "Crim Cr-", "Stillman-", "Big (Hump)-", "Stevens-", "Johns-", "Cloquallum-", "Porter-", "Scatter Cr-",
                     "Beaver Cr-", "Lincoln Cr-", "Elk Cr-", "Elk Cr-", "Dry Run-", "Black (Wynoochee)-", "(Wynoochee Resevoir)", "Wynoochee-", "Waddell Cr-", "Garrard Cr-",
                     "Bunker-", "Skookumchuck-", "Hanaford-", "Sterns Cr-", "Thrash Cr-", "Lake Cr-")

primary_cr <- c(lapply(primary_cr_list, function(z) {
  c(grep(z, flowline$Reach, value = TRUE))
}))

rm(all_habs)
rm(all_habs_year)

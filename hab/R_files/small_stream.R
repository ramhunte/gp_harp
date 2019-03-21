#### Pool/riffle ratios based on landcover and slope defined: ----
lc = c(rep(c("Agriculture", "BareShrubGrass", "Developed", "Forest", "Wetland", "Reference"), times = 3))
slope.class = c(rep("low", times = 6), rep("med", times = 6), rep("high", times = 6))
pool.perc = c(c(.92, .83, .74, .75, .89, .81), c(.60, .50, .51, .48, .53, .66), c(.31, .35, .54, .34, NA, .35))
ss.dist = data.frame(lc, slope.class, pool.perc)

SmStream_raw <- list.files(path = Inputs, pattern = "flowline", full.names = T) %>% 
  read.csv(.) %>% 
  filter(Habitat == 'SmStream')

ss <- SmStream_raw %>%
  select(noaaid) %>%
  left_join(., flowline %>%
              select(noaaid, curr_temp, hist_temp, temp_diff, curr.tempmult, hist.tempmult, Subbasin_num, pass_tot, Reach, Shape_Length, slope, lc, 
                     spawn_dist, species, both_chk, Area_ha, pass_tot_natural, tm_2040, tm_2080, tm_2040_cc_only, tm_2080_cc_only, can_ang)) %>%
  mutate(slope.class = ifelse(slope < .02, 
                              "low",
                              ifelse(slope >= .02 & slope < .04, 
                                     "med", 
                                     "high")),
         lc = ifelse(lc == "", 
                     "Reference", 
                     as.character(lc))) %>%
  filter(spawn_dist == "Yes")

if (fishtype == "spring_chinook") {
  ss <- ss %>%
    filter(Subbasin_num %in% schino_subs)
}

assign('asrp_ss_raw', ss , envir = .GlobalEnv) 

ss_curr_scenarios <- c("Current", "Fine_sediment", "Floodplain", "LR_bank", "LR_length", "Barriers")

ss_1 <- lapply(ss_curr_scenarios, function(x){
  ss %>%
    left_join(., ss.dist) %>%
    mutate(hab.scenario = x,
           Pool = Area_ha * pool.perc * curr.tempmult * curr_beaver_mult, # See trello board for discussion of the .985 multiplier.  This is for adding in current beaver ponds
           Riffle = Area_ha * (1 - pool.perc) * curr.tempmult * curr_beaver_mult,
           Beaver.Pond = (Shape_Length * curr_pond_area_per_m) / 10000 * curr.tempmult,
           winter.pool = Area_ha * pool.perc * winter_pool_scalar_warm * curr_beaver_mult,
           winter.riffle = Area_ha * (1 - pool.perc) * curr_beaver_mult + ((1 - winter_pool_scalar_warm) * Area_ha * pool.perc * curr_beaver_mult),
           winter.beaver.pond = (Shape_Length * curr_pond_area_per_m) / 10000) %>%
    gather(Habitat, Area, Pool:winter.beaver.pond) %>%
    mutate(life.stage = ifelse(Habitat %in% c("Pool", "Riffle", "Beaver.Pond"), 
                               "summer", 
                               "winter"),
           Habitat = ifelse(Habitat %in% c("Pool", "Riffle", "Beaver.Pond"), 
                            Habitat, 
                            ifelse(Habitat == "winter.pool", 
                                   "Pool",
                                   ifelse(Habitat == "winter.riffle",
                                          "Riffle",
                                          "Beaver.Pond"))))
}) %>%
  do.call('rbind',.)
ss_2 <- ss_1 %>%
  bind_rows(., ss %>%
              left_join(., ss.dist) %>%
              mutate(hab.scenario = "Shade",
                     Pool = Area_ha * pool.perc * hist.tempmult * curr_beaver_mult,
                     Riffle = Area_ha * (1 - pool.perc) * hist.tempmult * curr_beaver_mult,
                     Beaver.Pond = (Shape_Length * curr_pond_area_per_m) / 10000 * hist.tempmult,
                     winter.pool = Area_ha * pool.perc * winter_pool_scalar_warm * curr_beaver_mult,
                     winter.riffle = Area_ha * (1 - pool.perc) * curr_beaver_mult + ((1 - winter_pool_scalar_warm) * Area_ha * pool.perc * 
                                                                                       curr_beaver_mult),
                     winter.beaver.pond = (Shape_Length * curr_pond_area_per_m) / 10000) %>%
              gather(Habitat, Area, Pool:winter.beaver.pond) %>%
              mutate(life.stage = ifelse(Habitat %in% c("Pool", "Riffle", "Beaver.Pond"), 
                                         "summer", 
                                         "winter"),
                     Habitat = ifelse(Habitat %in% c("Pool", "Riffle", "Beaver.Pond"), 
                                      Habitat, 
                                      ifelse(Habitat == "winter.pool", 
                                             "Pool", 
                                             ifelse(Habitat == "winter.riffle",
                                                    "Riffle",
                                                    "Beaver.Pond"))))) %>%
  bind_rows(., ss %>%
              mutate(hab.scenario = "Wood",
                     lc = "Reference") %>%
              left_join(., ss.dist) %>%
              left_join(., wood_data) %>%
              mutate(Pool = Area_ha * pool.perc * curr.tempmult * woodmult_s * curr_beaver_mult,
                     Riffle = Area_ha * (1 - pool.perc) * curr.tempmult * woodmult_s * curr_beaver_mult,
                     Beaver.Pond = (Shape_Length * curr_pond_area_per_m) / 10000 * curr.tempmult * woodmult_s,
                     winter.pool = Area_ha * pool.perc * winter_pool_scalar_warm * woodmult_w * curr_beaver_mult,
                     winter.riffle = (Area_ha * (1 - pool.perc) * curr_beaver_mult + ((1 - winter_pool_scalar_warm) * Area_ha * pool.perc * 
                                                                                        curr_beaver_mult)) * woodmult_w,
                     winter.beaver.pond = (Shape_Length * curr_pond_area_per_m) / 10000 * woodmult_w) %>%
              gather(Habitat, Area, Pool:winter.beaver.pond) %>%
              mutate(life.stage = ifelse(Habitat %in% c("Pool", "Riffle", "Beaver.Pond"), 
                                         "summer", 
                                         "winter"),
                     Habitat = ifelse(Habitat %in% c("Pool", "Riffle", "Beaver.Pond"),
                                      Habitat, 
                                      ifelse(Habitat == "winter.pool",
                                             "Pool", 
                                             ifelse(Habitat == "winter.riffle",
                                                    "Riffle",
                                                    "Beaver.Pond"))))) %>%
  bind_rows(., ss %>%
              mutate(hab.scenario = "FP_wood_comb",
                     lc = "Reference") %>%
              left_join(., ss.dist) %>%
              left_join(., wood_data) %>%
              mutate(Pool = Area_ha * pool.perc * curr.tempmult * woodmult_s * curr_beaver_mult,
                     Riffle = Area_ha * (1 - pool.perc) * curr.tempmult * woodmult_s * curr_beaver_mult,
                     Beaver.Pond = (Shape_Length * curr_pond_area_per_m) / 10000 * curr.tempmult * woodmult_s,
                     winter.pool = Area_ha * pool.perc * winter_pool_scalar_warm * woodmult_w * curr_beaver_mult,
                     winter.riffle = (Area_ha * (1 - pool.perc) * curr_beaver_mult + ((1 - winter_pool_scalar_warm) * Area_ha * pool.perc * 
                                                                                        curr_beaver_mult)) * woodmult_w,
                     winter.beaver.pond = (Shape_Length * curr_pond_area_per_m) / 10000 * woodmult_w) %>%
              gather(Habitat, Area, Pool:winter.beaver.pond) %>%
              mutate(life.stage = ifelse(Habitat %in% c("Pool", "Riffle", "Beaver.Pond"), 
                                         "summer", 
                                         "winter"),
                     Habitat = ifelse(Habitat %in% c("Pool", "Riffle", "Beaver.Pond"),
                                      Habitat, 
                                      ifelse(Habitat == "winter.pool", 
                                             "Pool", 
                                             ifelse(Habitat == "winter.riffle",
                                                    "Riffle",
                                                    "Beaver.Pond"))))) %>%
  bind_rows(., ss %>%
              left_join(., ss.dist) %>%
              mutate(hab.scenario = "Beaver",
                     Pool = Area_ha * pool.perc * hist_beaver_mult * curr.tempmult,
                     Riffle = Area_ha * (1 - pool.perc) * hist_beaver_mult * curr.tempmult,
                     Beaver.Pond = (Shape_Length * hist_pond_area_per_m)/10000 * curr.tempmult,
                     winter.pool = Area_ha * pool.perc * winter_pool_scalar_warm * hist_beaver_mult,
                     winter.riffle = Area_ha * (1 - pool.perc) * hist_beaver_mult + ((1 - winter_pool_scalar_warm) * Area_ha * pool.perc * 
                                                                                       hist_beaver_mult),
                     winter.beaver.pond = (Shape_Length * hist_pond_area_per_m) / 10000) %>%  ##### does tempmult apply for beaver ponds?
              gather(Habitat, Area, Pool:winter.beaver.pond) %>%
              mutate(life.stage = ifelse(Habitat %in% c("Pool", "Riffle", "Beaver.Pond"), 
                                         "summer", 
                                         "winter"),
                     Habitat = ifelse(Habitat %in% c("Pool", "Riffle", "Beaver.Pond"), 
                                      Habitat, 
                                      ifelse(Habitat == "winter.pool", 
                                             "Pool", 
                                             ifelse(Habitat == "winter.riffle", 
                                                    "Riffle", 
                                                    "Beaver.Pond"))))) %>%
  bind_rows(., ss %>%
              mutate(hab.scenario = "Historical",
                     lc = "Reference") %>%
              left_join(., ss.dist) %>%
              left_join(., wood_data) %>%
              mutate(Pool = Area_ha * pool.perc * hist_beaver_mult * hist.tempmult * woodmult_s,
                     Riffle = Area_ha * (1 - pool.perc) * hist_beaver_mult * hist.tempmult * woodmult_s,
                     Beaver.Pond = (Shape_Length * hist_pond_area_per_m)/10000 * hist.tempmult * woodmult_s,
                     winter.pool = Area_ha * pool.perc * winter_pool_scalar_warm * woodmult_w * hist_beaver_mult,
                     winter.riffle = (Area_ha * (1 - pool.perc) * hist_beaver_mult + ((1 - winter_pool_scalar_warm) * Area_ha * pool.perc *
                                                                                        hist_beaver_mult)) * woodmult_w,
                     winter.beaver.pond = (Shape_Length * hist_pond_area_per_m)/10000 * woodmult_w) %>% ##### does tempmult apply for beaver ponds?
              gather(Habitat, Area, Pool:winter.beaver.pond) %>%
              mutate(life.stage = ifelse(Habitat %in% c("Pool", "Riffle", "Beaver.Pond"), 
                                         "summer", 
                                         "winter"),
                     Habitat = ifelse(Habitat %in% c("Pool", "Riffle", "Beaver.Pond"), 
                                      Habitat, 
                                      ifelse(Habitat == "winter.pool", 
                                             "Pool", 
                                             ifelse(Habitat == "winter.riffle", 
                                                    "Riffle", 
                                                    "Beaver.Pond"))))) %>%
  mutate(Area = ifelse(hab.scenario %in% c("Barriers", "Historical"),
                       ifelse(pass_tot_natural == 0, 
                              0, 
                              Area), 
                       ifelse(pass_tot == 0, 
                              0, 
                              Area))) %>%
  select(noaaid, Subbasin_num, hab.scenario, Habitat, Area, life.stage, curr.tempmult, hist.tempmult, pass_tot, both_chk, pass_tot_natural)


if (fishtype == "spring_chinook") {
  ss_2 <- ss_2 %>%
    mutate(chinook_scalar = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs, 
                                   schino_mult, 
                                   1),
           Area = Area * chinook_scalar)
}

if (fishtype == "fall_chinook") {
  ss_2 <- ss_2 %>%
    mutate(chinook_scalar = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs, 
                                   fchino_mult, 
                                   1),
           Area = Area * chinook_scalar)
}

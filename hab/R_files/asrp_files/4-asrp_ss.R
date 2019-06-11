# Build reference (historical) pool/riffle ratios data table to recalculate pool/riffle areas in added large wood reaches ----

lc.ref = c(rep("Reference", times = 3))
slope.class = c("low", "med", "high")
pool.perc.ref = c(.81, .66, .35)
ss.dist.ref = data.frame(lc.ref, slope.class, pool.perc.ref)
colnames(asrp_ss_raw)

# Calculate area of each reach in each scenario ----
asrp_ss <- all_habs_scenario %>%
  filter(Habitat == "SmStream") %>%
  select(noaaid, Subbasin_num, Reach, Shape_Length, slope, lc, spawn_dist, species, both_chk,
         Reach_low, slope.class, Habitat, wet_width, can_ang, Scenario_num, year, chino_mult) %>%
  left_join(., edt_width) %>%
  left_join(., ss.dist) %>%
  left_join(., ss.dist.ref) %>%
  left_join(., asrp_reach_data) %>%
  left_join(., asrp_culvs) %>%
  mutate(
    tempmult.asrp = ifelse(species %in% c("spring_chinook", "fall_chinook"), # Added because of spring chinook w/temp survival 
                           1,
                           tempmult.asrp),
    width_s = ifelse(is.na(width_s),
                     wet_width,
                     width_s),
    width_w = ifelse(is.na(width_w),
                     wet_width,
                     width_w),
    area_s = (Shape_Length * width_s) / 10000,
    area_w = (Shape_Length * width_w) / 10000,
    pool.perc.asrp = ifelse(LW == "y",
                            pool.perc + ((pool.perc.ref - pool.perc) * rest_perc * wood_intensity_scalar),
                            pool.perc),
    beaver_mult_asrp = ifelse(Beaver == "y",
                              curr_beaver_mult - ((1 - hist_beaver_mult) * rest_perc * beaver_intensity_scalar),
                              curr_beaver_mult),
    
    # Begin habitat area calculations ----
    
    Pool = area_s * pool.perc.asrp * tempmult.asrp * woodmult_s_asrp * beaver_mult_asrp,
    Riffle = area_s * (1 - pool.perc) * tempmult.asrp * woodmult_s_asrp * beaver_mult_asrp, 
    Beaver.Pond = ifelse(Beaver == "y",
                         ((Shape_Length * curr_pond_area_per_m / 10000) + (Shape_Length * (hist_pond_area_per_m - curr_pond_area_per_m) / 
                                                                             10000 * rest_perc * beaver_intensity_scalar)) * # 3 m^2 / m in historical - .3 m^2 in current = 2.7 m^2 diff between historical and current
                           tempmult.asrp * woodmult_s_asrp,
                         (Shape_Length * curr_pond_area_per_m) / 10000 * tempmult.asrp * woodmult_s_asrp),
    winter.pool = area_w * pool.perc.asrp * winter_pool_scalar_warm * woodmult_w_asrp * beaver_mult_asrp,
    winter.riffle = (area_w * (1 - pool.perc.asrp) * beaver_mult_asrp + ((1 - winter_pool_scalar_warm) * area_w * pool.perc) * beaver_mult_asrp) * 
      woodmult_w_asrp,
    winter.beaver.pond = ifelse(Beaver == "y",
                                ((Shape_Length * curr_pond_area_per_m / 10000) + 
                                   (Shape_Length * (hist_pond_area_per_m - curr_pond_area_per_m) / 10000 * rest_perc * 
                                      beaver_intensity_scalar)) * woodmult_w_asrp,
                                (Shape_Length * curr_pond_area_per_m) / 10000 * woodmult_s_asrp)) %>%
  gather(Habitat, Area, Pool:winter.beaver.pond) %>%
  mutate(
    life.stage = case_when(
      Habitat %in% c("Pool", "Riffle", "Beaver.Pond") ~ "summer",
      Habitat %in% c("winter.pool", "winter.riffle", "winter.beaver.pond") ~ "winter"),
    Habitat = case_when(
      Habitat %in% c("Pool", "Riffle", "Beaver.Pond") ~ Habitat,
      Habitat == "winter.pool" ~ "Pool",
      Habitat == "winter.riffle" ~ "Riffle",
      Habitat == "winter.beaver.pond" ~ "Beaver.Pond"),
    Area = ifelse(pass_tot_asrp == 0,
                  0,
                  Area)) %>%
  select(noaaid, Subbasin_num, pass_tot_asrp, GSU, forest, woodmult_s_asrp, woodmult_w_asrp, tempmult.asrp, Habitat, Area, life.stage, lc, 
         slope.class, rest_perc, rest_perc, both_chk, Scenario_num, year, LW, Floodplain, Beaver, Riparian, Barriers, wood_intensity_scalar, wood_intensity_scalar, chino_mult)

if (fishtype %in% c("spring_chinook", "fall_chinook")) {
  asrp_ss %<>%
    rename(Area_nochino = Area) %>%
    mutate(Area = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
                         Area_nochino * chino_mult,
                         Area_nochino))
}

asrp_ss_mvmt <- asrp_ss %>%
  filter(!Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", "Current_asrp", 'dev_and_climate'))

asrp_ss %<>%
  filter(Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", "Current_asrp", 'dev_and_climate'))
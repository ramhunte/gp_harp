# a.)  Small stream ----

# Build reference (historical) pool/riffle ratios data table to recalculate pool/riffle areas in added large wood reaches

lc.ref = c(rep("Reference", times = 3))
slope.class = c("low", "med", "high")
pool.perc.ref = c(.81, .66, .35)
ss.dist.ref = data.frame(lc.ref, slope.class, pool.perc.ref)

asrp_ss <- asrp_ss_raw %>%
  select(-area_s, -area_w) %>%
  left_join(., edt_width %>%
              filter( year == x),
            by = "Reach_low") %>%
  left_join(., ss.dist) %>%
  left_join(., ss.dist.ref) %>%
  left_join(., wood_data) %>%
  left_join(., asrp_culvs) %>%
  left_join(., fl_to_gsu) %>%
  mutate(
    
    # Begin habitat parameter calculations ----
    
    width_s = ifelse(is.na(width_s),
                     wet_width,
                     width_s),
    width_w = ifelse(is.na(width_w),
                     wet_width,
                     width_w),
    area_s = (Shape_Length * width_s) / 10000,
    area_w = (Shape_Length * width_w) / 10000,
    GSU = ifelse(primary_cr_only == "y",
                 ifelse(Reach %in% primary_cr,
                        GSU,
                        paste0(GSU, "_np")),
                 GSU),
    pool.perc.asrp = ifelse(GSU %in% wood_gsu,
                            ifelse(forest == "y",
                                   pool.perc + ((pool.perc.ref - pool.perc) * rest_perc_f * wood_intensity_scalar_f),
                                   pool.perc + ((pool.perc.ref - pool.perc) * rest_perc_nf * wood_intensity_scalar_nf)),
                            pool.perc),
    woodmult_s_asrp = ifelse(GSU %in% wood_gsu,
                             ifelse(forest == "y",
                                    1 + ((woodmult_s - 1) * rest_perc_f * wood_intensity_scalar_f),
                                    1 + ((woodmult_s - 1) * rest_perc_nf * wood_intensity_scalar_nf)),
                             1),
    woodmult_w_asrp = ifelse(GSU %in% wood_gsu,
                             ifelse(forest == "y",
                                    1 + ((woodmult_w - 1) * rest_perc_f * wood_intensity_scalar_f),
                                    1 + ((woodmult_w - 1) * rest_perc_nf * wood_intensity_scalar_nf)),
                             1),
    asrp_temp = ifelse(GSU %in% shade_gsu,
                       asrp_temp_w_growth,
                       ifelse(can_ang > 170,
                              asrp_temp_cc_only,
                              asrp_temp_w_growth)),
    tempmult.asrp = ifelse(species %in% c("coho", "steelhead"),
                           temp_func(asrp_temp),
                           1),    # set tempmult to 1 for chinook so that temperature does not have an effect on rearing capacity and survival
    beaver_mult_asrp = ifelse(GSU %in% beaver_gsu,
                              ifelse(forest == "y",
                                     curr_beaver_mult - ((1 - hist_beaver_mult) * rest_perc_f * beaver_intensity_scalar_f),
                                     curr_beaver_mult - ((1 - hist_beaver_mult) * rest_perc_nf * beaver_intensity_scalar_nf)),
                              curr_beaver_mult),
    
    # Begin habitat area calculations ----
    
    Pool = area_s * pool.perc.asrp * tempmult.asrp * woodmult_s_asrp * beaver_mult_asrp,
    Riffle = area_s * (1 - pool.perc) * tempmult.asrp * woodmult_s_asrp * beaver_mult_asrp, 
    Beaver.Pond = ifelse(GSU %in% beaver_gsu,
                         ifelse(forest == "y",
                                ((Shape_Length * curr_pond_area_per_m / 10000) + (Shape_Length * (hist_pond_area_per_m - curr_pond_area_per_m) / 
                                                                                    10000 * rest_perc_f * beaver_intensity_scalar_f)) * # 3 m^2 / m in historical - .3 m^2 in current = 2.7 m^2 diff between historical and current
                                  tempmult.asrp * woodmult_s_asrp,
                                ((Shape_Length * curr_pond_area_per_m / 10000) + (Shape_Length * (hist_pond_area_per_m - curr_pond_area_per_m) / 
                                                                                    10000 * rest_perc_nf * beaver_intensity_scalar_nf)) * 
                                  tempmult.asrp * woodmult_s_asrp),
                         (Shape_Length * curr_pond_area_per_m) / 10000 * tempmult.asrp * woodmult_s_asrp),
    winter.pool = area_w * pool.perc.asrp * winter_pool_scalar_warm * woodmult_w_asrp * beaver_mult_asrp,
    winter.riffle = (area_w * (1 - pool.perc.asrp) * beaver_mult_asrp + ((1 - winter_pool_scalar_warm) * area_w * pool.perc) * beaver_mult_asrp) * woodmult_w_asrp,
    winter.beaver.pond = ifelse(GSU %in% beaver_gsu,
                                ifelse(forest == "y",
                                       ((Shape_Length * curr_pond_area_per_m / 10000) + 
                                          (Shape_Length * (hist_pond_area_per_m - curr_pond_area_per_m) / 10000 * rest_perc_f * 
                                             beaver_intensity_scalar_f)) * woodmult_w_asrp,
                                       ((Shape_Length * curr_pond_area_per_m / 10000) + 
                                          (Shape_Length * (hist_pond_area_per_m - curr_pond_area_per_m) / 10000 * rest_perc_nf * 
                                             beaver_intensity_scalar_nf)) * 
                                         woodmult_w_asrp),
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
         slope.class, rest_perc_nf, rest_perc_f, both_chk)

if (fishtype == "spring_chinook") {
  asrp_ss %<>%
    rename(Area_nochino = Area) %>%
    mutate(Area = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
                         Area_nochino * schino_mult,
                             Area_nochino))
} else if (fishtype == "fall_chinook") {
  asrp_ss %<>%
    rename(Area_nochino = Area) %>%
    mutate(Area = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
                         Area_nochino * fchino_mult,
                         Area_nochino))
}
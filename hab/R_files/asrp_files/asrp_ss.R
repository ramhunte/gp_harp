# a.)  Small stream ----

# Build reference (historical) pool/riffle ratios data table to recalculate pool/riffle areas in added large wood reaches

lc.ref = c(rep("Reference", times = 3))
slope.class = c("low", "med", "high")
pool.perc.ref = c(.81, .66, .35)
ss.dist.ref = data.frame(lc.ref, slope.class, pool.perc.ref)

asrp_ss <- asrp_ss_raw %>%
  left_join(., ss.dist) %>%
  left_join(., ss.dist.ref) %>%
  left_join(., wood_data) %>%
  left_join(., asrp_culvs) %>%
  left_join(., fl_to_gsu) %>%
  mutate(GSU = ifelse(primary_cr_only == "y",
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
         Pool = ifelse(GSU %in% beaver_gsu,
                       ifelse(forest == "y",
                              Area_ha * pool.perc.asrp * tempmult.asrp * woodmult_s_asrp * (1 - (.15 * rest_perc_f * beaver_intensity_scalar_f)),
                              Area_ha * pool.perc.asrp * tempmult.asrp * woodmult_s_asrp * (1 - (.15 * rest_perc_nf * beaver_intensity_scalar_nf))),
                       Area_ha * pool.perc.asrp * tempmult.asrp * woodmult_s_asrp),
         Riffle = ifelse(GSU %in% beaver_gsu,
                         ifelse(forest == "y",
                                Area_ha * (1 - pool.perc) * tempmult.asrp * woodmult_s_asrp * (1 - (.15 * rest_perc_f * beaver_intensity_scalar_f)),
                                Area_ha * (1 - pool.perc) * tempmult.asrp * woodmult_s_asrp * (1 - (.15 * rest_perc_nf * 
                                                                                                      beaver_intensity_scalar_nf))),
                         Area_ha * (1 - pool.perc) * tempmult.asrp * woodmult_s_asrp),
         Beaver.Pond = ifelse(GSU %in% beaver_gsu,
                              ifelse(forest == "y",
                                     (Shape_Length * 3) / 10000 * tempmult.asrp * woodmult_s_asrp * rest_perc_f * beaver_intensity_scalar_f,
                                     (Shape_Length * 3)/10000 * tempmult.asrp * woodmult_s_asrp * rest_perc_nf * beaver_intensity_scalar_nf),
                              0),
         winter.pool = ifelse(GSU %in% beaver_gsu,
                              ifelse(forest == "y",
                                     Area_ha * pool.perc.asrp * winter_pool_scalar_warm * woodmult_w_asrp * (1 - (.15 * rest_perc_f * 
                                                                                                                    beaver_intensity_scalar_f)),
                                     Area_ha * pool.perc.asrp * winter_pool_scalar_warm * woodmult_w_asrp * (1 - (.15 * rest_perc_nf * 
                                                                                                                    beaver_intensity_scalar_nf))),
                              Area_ha * pool.perc.asrp * winter_pool_scalar_warm * woodmult_w_asrp),
         winter.riffle = ifelse(GSU %in% beaver_gsu,
                                ifelse(forest == "y",
                                       (Area_ha * (1 - pool.perc.asrp) * (1 - (.15 * rest_perc_f * beaver_intensity_scalar_f)) + 
                                          ((1 - winter_pool_scalar_warm) * Area_ha * pool.perc * ((1 - (.15 * rest_perc_f * 
                                                                                                          beaver_intensity_scalar_f))))),
                                       (Area_ha * (1 - pool.perc.asrp) * (1 - (.15 * rest_perc_nf * beaver_intensity_scalar_nf)) + 
                                          ((1 - winter_pool_scalar_warm) * Area_ha * pool.perc * ((1 - (.15 * rest_perc_nf * 
                                                                                                          beaver_intensity_scalar_nf)))))),
                                (Area_ha * (1 - pool.perc.asrp) + ((1 - winter_pool_scalar_warm) * Area_ha * pool.perc)) * woodmult_w_asrp),
         winter.beaver.pond = ifelse(GSU %in% beaver_gsu,
                                     ifelse(forest == "y",
                                            (Shape_Length * 3) / 10000 * woodmult_w_asrp * rest_perc_f * beaver_intensity_scalar_f,
                                            (Shape_Length * 3) / 10000 * woodmult_w_asrp * rest_perc_nf * beaver_intensity_scalar_nf),
                                     0)) %>%
  gather(Habitat, Area, Pool:winter.beaver.pond) %>%
  mutate(life.stage = ifelse(Habitat %in% c("Pool", "Riffle", "Beaver.Pond"), 
                             "summer", 
                             "winter"),
         Habitat = ifelse(Habitat == "winter.pool", "Pool",
                          ifelse(Habitat == "winter.riffle", 
                                 "Riffle",
                                 ifelse(Habitat == "winter.beaver.pond", 
                                        "Beaver.Pond", 
                                        Habitat))),
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
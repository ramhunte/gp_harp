steelhead_vars = "yes"

#Spawning#
fecundity = 3500
redd_area = 5.4
defended_redd_area = NA
adult_per_redd = 1.3

PR_redd_density = 2.5
F_redd_density = 7.3
NF_redd_density = 2.3

# winter_pool_scalar_cold = .14
winter_pool_scalar_warm = .43
# winter_riffle_scalar_cold = 1 + (1-winter_pool_scalar_cold)
# winter_riffle_scalar_warm = 1 + (1-winter_pool_scalar_warm)

Habitat = c(rep(c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater", "Pool", "Riffle", "Beaver.Pond", "FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", 
                  "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm"), times = 4))
life.stage = c(rep("summer", times = length(Habitat)/4), rep("winter", times = length(Habitat)/4), rep("summer.2", times = length(Habitat)/4), rep("winter.2", times = length(Habitat)/4))

year2_scalar = 0.2 # density scalar for juveniles in year 1 vs year 2
Density = c(3000, 1000, 2071, 2470, 253, 2000, 6300, 4600, 0, 0, 0, 0, 1000, 1000, 6300, 4600, 0, 0, 
            1000, 1000, 1998, 702, 0, 0, 1400, 1000, 300, 0, 0, 0, 0, 0, 1400, 1000, 0, 0,
            c(3000, 1000, 2071, 2470, 253, 2000, 6300, 4600, 0, 0, 0, 0, 1000, 1000, 6300, 4600, 0, 0) * year2_scalar, 
            c(1000, 1000, 1998, 702, 0, 0, 1400, 1000, 300, 0, 0, 0, 0, 0, 1400, 1000, 0, 0) * year2_scalar)
density <- data.frame(Habitat, life.stage, Density)


edt_species = "Steelh"
spawn_species = "edt_fchnk"
lower_species = "fchino"

fishmult = 1

Habitat = c(rep(c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater", "Pool", "Riffle", "Beaver.Pond", "FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", 
                  "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm"), times = 4))
life.stage = c(rep("summer", times = length(Habitat)/4), rep("winter", times = length(Habitat)/4), rep("summer.2", times = length(Habitat)/4), rep("winter.2", times = length(Habitat)/4))
surv.base = c(rep(.4, times = length(Habitat)/8), rep(.59, times = length(Habitat)/ 8),
              rep(.32, times = length(Habitat)/8), rep(.48, times = length(Habitat)/ 8),
              rep(.4, times = length(Habitat)/8), rep(.59, times = length(Habitat)/ 8),
              rep(.32, times = length(Habitat)/8), rep(.48, times = length(Habitat)/ 8))
wood.surv.base = c(rep(.4, times = length(Habitat)/8), rep(.59, times = length(Habitat)/ 8),
                   rep(.32, times = length(Habitat)/8), rep(.48, times = length(Habitat)/ 8),
                   rep(.4, times = length(Habitat)/8), rep(.59, times = length(Habitat)/ 8),
                   rep(.32, times = length(Habitat)/8), rep(.48, times = length(Habitat)/ 8))
survival <- data.frame(Habitat, life.stage, surv.base, wood.surv.base)


LgRiver_habs = c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater")
SmStream_habs = c("Pool", "Riffle", "Trib_Pond")
Floodplain_habs = c("FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", "Side_Channel_pool", "Side_Channel_riffle", "Slough_lg", "Slough_sm", "SC_pool", "SC_riffle")

fry_colonization_surv = .78

Scalar = 1

# Wood multipliers for large river ----
# Summer
lr_wd_s_bank <- 1.052
lr_wd_s_bar <- 1.011

# Winter
lr_wd_w_bank <- 1.069
lr_wd_w_bar <- 1.108

# Prespawn survival ----
prespawn_surv_raw <- .9
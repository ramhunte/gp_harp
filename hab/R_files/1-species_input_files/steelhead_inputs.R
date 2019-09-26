steelhead_vars = "yes"

#Spawning#
fecundity = 4000
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
Density = c(3200, 2600, 4200, 2800, 200, 2000, 6300, 4600, 0, 0, 0, 0, 0, 0, 6300, 4600, 0, 0, 
            2100, 1800, 1300, 2000, 0, 800, 1400, 1000, 300, 0, 0, 0, 300, 300, 1400, 1000, 0, 0,
            c(3200, 2600, 4200, 2800, 200, 2000, 8500, 1500, 3500, 0, 0, 0, 3500, 3500, 8500, 1500, 0, 0) * year2_scalar, 
            c(2100, 1800, 1300, 2000, 0, 800, 3500, 1500, 500, 0, 0, 0, 500, 500, 3500, 1500, 0, 0) * year2_scalar)
density <- data.frame(Habitat, life.stage, Density)

fishmult = 1

Habitat = c(rep(c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater", "Pool", "Riffle", "Beaver.Pond", "FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", 
                  "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm"), times = 4))
life.stage = c(rep("summer", times = length(Habitat)/4), rep("winter", times = length(Habitat)/4), rep("summer.2", times = length(Habitat)/4), rep("winter.2", times = length(Habitat)/4))
surv.base = c(rep(.27, times = length(Habitat)/9), rep(.59, times = length(Habitat)/ 7),
              rep(.32, times = length(Habitat)/9), rep(.48, times = length(Habitat)/ 7),
              rep(.56, times = length(Habitat)/9), rep(.59, times = length(Habitat)/ 7),
              rep(.45, times = length(Habitat)/9), rep(.48, times = length(Habitat)/ 7))
wood.surv.base = c(rep(.27, times = length(Habitat)/9), rep(.59, times = length(Habitat)/ 7),
                   rep(.32, times = length(Habitat)/9), rep(.48, times = length(Habitat)/ 7),
                   rep(.56, times = length(Habitat)/9), rep(.59, times = length(Habitat)/ 7),
                   rep(.45, times = length(Habitat)/9), rep(.48, times = length(Habitat)/ 7))
survival <- data.frame(Habitat, life.stage, surv.base, wood.surv.base)


LgRiver_habs = c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater")
SmStream_habs = c("Pool", "Riffle", "Beaver.Pond")
Floodplain_habs = c("FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", "Slough_lg", "Slough_sm", "SC_pool", "SC_riffle", "Side_Channel")

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
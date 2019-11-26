#Spawning#
fecundity = 3200
redd_area = 2.3
defended_redd_area = 42.3
adult_per_redd = 1.9

PR_redd_density = 4.2
F_redd_density = 8.8
NF_redd_density = 0

prespawn_surv_raw <- .76

#Rearing
Habitat = c(rep(c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater", "Pool", "Riffle", "Beaver.Pond","Pool_low_ag", 
                  "FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm"), times = 4))
life.stage = c(rep("fry.colonization", times = length(Habitat)/4), rep("winter", times = length(Habitat)/4), rep('summer.2', times = length(Habitat) / 4), 
               rep('winter.2', times = length(Habitat) / 4))
Density = c(rep(13300, times = 2), rep(17000, times = 2), 6700, 30000, rep(30000, times = 2), 0, 30000,rep(0, times = 66))
density <- data.frame(Habitat, life.stage, Density)

winter_pool_scalar_warm = 1
winter_pool_scalar_cold = 1

#Survival#

Habitat = c(rep(c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater", "Pool", "Riffle", "Beaver.Pond", "FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", 
                  "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm"), times = 2))
life.stage = c(rep("summer", times = length(Habitat)/2), rep("winter", times = length(Habitat)/2), rep('fry.colonization', times = length(Habitat) / 2),
               rep('winter.2', times = length(Habitat) / 2))
surv.base = (c(rep(.44, times = 18), rep(NA, times = 18), rep(.48, times = 18), rep(NA, times = 18)))
wood.surv.base = (c(rep(.44, times = 18), rep(NA, times = 18), rep(.48, times = 18), rep(NA, times = 18)))
survival <- data.frame(Habitat, life.stage, surv.base, wood.surv.base)

LgRiver_habs = c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater")
SmStream_habs = c("Pool", "Riffle", "Beaver.Pond")
Floodplain_habs = c("FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm", "Side_Channel")

# Wood multipliers for large river ----
# Summer
lr_wd_s_bank <- 1
lr_wd_s_bar <- 1

# Winter
lr_wd_w_bank <- -9999
lr_wd_w_bar <- -9999
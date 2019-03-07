schino_vars = "yes"

#Spawning#
fecundity = 5400
redd_area = 14.1
defended_redd_area = 42.3
adult_per_redd = 1.9

PR_redd_density = 4.2
F_redd_density = 8.8
NF_redd_density = 0

Habitat = c(rep(c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater", "Pool", "Riffle", "Beaver.Pond","Pool_low_ag", "FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", 
            "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm"), times = 2))
life.stage = c(rep("summer", times = length(Habitat)/2), rep("winter", times = length(Habitat)/2))
Density = c(6766, 2738, 511, 1463, 449, 9671, 922, 57, 922, 922, 376, 0, 0, 922, 922, 922, 57, 1183, 1183, rep(NA, times = 19))
density <- data.frame(Habitat, life.stage, Density)

winter_pool_scalar_warm = 1
winter_pool_scalar_cold = 1

#Other#
edt_species = "SChino"
spawn_species = "edt_schnk"
lower_species = "schino"
species_save = "spring.chinook"

#Survival#

Habitat = c(rep(c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater", "Pool", "Riffle", "Beaver.Pond", "FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", 
            "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm"), times = 2))
life.stage = c(rep("summer", times = length(Habitat)/2), rep("winter", times = length(Habitat)/2))
surv.base = c(rep(.31, times = 8), rep(.46, times = 10), rep(NA, times = 18))
wood.surv.base = c(rep(.31, times = 6), rep(.52, times = 2), rep(.46, times = 10), rep(NA, times = 18))
survival <- data.frame(Habitat, life.stage, surv.base, wood.surv.base)

LgRiver_habs = c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater")
SmStream_habs = c("Pool", "Riffle", "Trib_Pond")
Floodplain_habs = c("FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", "Side_Channel_pool", "Side_Channel_riffle", "Slough_lg", "Slough_sm")

fry_colonization_surv = -9999
 
# Wood multipliers for large river ----
# Summer
lr_wd_s_bank <- 1.16
lr_wd_s_bar <- 1.03

# Winter
lr_wd_w_bank <- -9999
lr_wd_w_bar <- -9999

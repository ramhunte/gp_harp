#Spawning#
fecundity = 5400
redd_area = 14.1
adult_per_redd = 1.9

PR_redd_density = 9.7
F_redd_density = 6.4
NF_redd_density = 2.9


Habitat = c(rep(c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater", "Pool", "Riffle", "Beaver.Pond","Pool_low_ag", 
                  "FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm"), times = 4))
life.stage = c(rep("summer", times = length(Habitat)/4), rep("winter", times = length(Habitat)/4), rep('summer.2', times = length(Habitat) / 4), 
               rep('winter.2', times = length(Habitat) / 4))
Density = c(rep(c(6766, 2738, 511, 1463, 449, 9671, 922, 57, 922, 922, 376, 0, 0, 922, 922, 922, 57, 1183, 1183, rep(NA, times = 19)), times = 2))
density <- data.frame(Habitat, life.stage, Density)

Habitat = c(rep(c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater", "Pool", "Riffle", "Beaver.Pond", "FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", 
                  "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm"), times = 2))
life.stage = c(rep("summer", times = length(Habitat)/2), rep("winter", times = length(Habitat)/2), rep('summer.2', times = length(Habitat) / 2),
               rep('winter.2', times = length(Habitat) / 2))
surv.base = c(rep(c(rep(.26, times = 8), rep(.46, times = 6), .26, .26, .46, .46, rep(NA, times = 18)), times = 2))
wood.surv.base = c(rep(c(rep(.26, times = 6), rep(.52, times = 2), rep(.46, times = 6), .52, .52, .46, .46, rep(NA, times = 18)), times = 2))
survival <- data.frame(Habitat, life.stage, surv.base, wood.surv.base)

# Wood multipliers for large river ----
# Summer
lr_wd_s_bank <- 1.16
lr_wd_s_bar <- 1.03

# Winter
lr_wd_w_bank <- -9999
lr_wd_w_bar <- -9999

# Prespawn survival ----
prespawn_surv_raw <- 1

chino_mult <- .81

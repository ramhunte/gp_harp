#Spawning#
fecundity = 2500
redd_area = 6
adult_per_redd = 1.6

PR_redd_density = 85
F_redd_density = 274
NF_redd_density = 12

Habitat = c(rep(c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater", "Pool", "Riffle", "Beaver.Pond", "FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", 
            "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm"), times = 2))
life.stage = c(rep("summer", times = length(Habitat)/2), rep("winter", times = length(Habitat)/2))
Density = c(5400, 1996, 0, 1000, 1000, 7900, 17000, 3000, 12000, 0, 0, 0, 9000, 18000, 17000, 3000, 9000, 18000,540, 500, 0, 0, 0, 600, 4000, 100, 12000, 0,25,3200, 9000, 18000, 
            4000, 100, 9000, 18000)              
density <- data.frame(Habitat,life.stage, Density)

Habitat = c(rep(c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater", "Pool", "Riffle", "Beaver.Pond", "FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", 
            "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm"), times = 2))
life.stage = c(rep("summer", times = length(Habitat)/2), rep("winter", times = length(Habitat)/2))
surv.base = c(rep(.84, times = length(Habitat)/2), rep(.24, times = 6), .24, .24, .68, .4, .4, .68, .68, .68, .24, .24, .4, .4)
wood.surv.base = c(rep(.84, times = 6), .9, .9, rep(.84, times = 6), .9, .9, .84, .84, rep(.24, times = 6), .4, .4, .68, .4, .4, .68, .68, .68, .4, .4, .4, .4)
survival <- data.frame(Habitat, life.stage, surv.base, wood.surv.base)

# Wood multipliers for large river ----
# Summer
lr_wd_s_bank <- 1.2
lr_wd_s_bar <- 1.02

# Winter
lr_wd_w_bank <- 1.21
lr_wd_w_bar <- 1.2

# Prespawn survival ----
prespawn_surv_raw <- .95

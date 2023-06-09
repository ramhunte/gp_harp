#Spawning#
fecundity = 5400
redd_area = 5.4
adult_per_redd = 1.3

#Rearing

Habitat = c(rep(c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater", "Pool", "Riffle", "Beaver.Pond", "FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm", "Bank_center", "HM_Bank_center", "Bar_boulder_center", "Bar_gravel_center", 
                  "Bar_sand_center"), times = 4))
life.stage = c(rep("summer", times = length(Habitat)/4), rep("winter", times = length(Habitat)/4), rep("summer.2", times = length(Habitat)/4), rep("winter.2", times = length(Habitat)/4))

year2_scalar = 0.2 # density scalar for juveniles in year 1 vs year 2
Density = c(12700, 6400, 15900, 15900, 0, 12700, 7000, 5300, 0, 0, 0, 0, 0, 0, 7000, 5300, 0, 0, 640, 640, 640, 640, 640, 
            3100, 3100, 3100, 3100, 0, 0, 1600, 1100, 300, 0, 0, 0, 300, 300, 1600, 1100, 0, 0, 160, 160, 160, 160, 160,
            3900, 2000, 4900, 4900, 0, 3900, 1800, 700, 700, 0, 0, 0, 700, 700, 1800, 700, 0, 0, 1100, 1100, 1100, 1100, 1100, 
            960, 960, 960, 960, 0, 0, 900, 400, 100, 0, 0, 0, 100, 100, 900, 400, 0, 0, 270, 270, 270, 270, 270)
density <- data.frame(Habitat, life.stage, Density)

Habitat = c(rep(c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater", "Pool", "Riffle", "Beaver.Pond", "FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", 
                  "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm", "Bank_center", "HM_Bank_center", "Bar_boulder_center", "Bar_gravel_center", 
                  "Bar_sand_center"), times = 4))
life.stage = c(rep("summer", times = length(Habitat)/4), rep("winter", times = length(Habitat)/4), rep("summer.2", times = length(Habitat)/4), rep("winter.2", times = length(Habitat)/4))
surv.base = c(rep(.6, times = 8), rep(.74, times = 6), rep(.6, times = 2), rep(.74, times = 2), rep(.6, times = 5),
              rep(.35, times = 8), rep(.52, times = 6), rep(.35, times = 2), rep(.52, times = 2), rep(.35, times = 5),
              rep(.85, times = 8), rep(.74, times = 6), rep(.85, times = 2), rep(.74, times = 2), rep(.85, times = 5),
              rep(.49, times = 8), rep(.52, times = 6), rep(.49, times = 2), rep(.52, times = 2), rep(.49, times = 5))
wood.surv.base = c(rep(.6, times = 6), rep(.62, times = 2),  rep(.74, times = 6), rep(.62, times = 2), rep(.74, times = 2), rep(.6, times = 5),
                   rep(.35, times = 6), rep(.58, times = 2), rep(.52, times = 6), rep(.58, times = 2), rep(.52, times = 2), rep(.35, times = 5),
                   rep(.85, times = 6), rep(.88, times = 2), rep(.74, times = 6), rep(.88, times = 2), rep(.74, times = 2), rep(.85, times = 5),
                   rep(.49, times = 6), rep(.53, times = 2), rep(.52, times = 6), rep(.53, times = 2), rep(.52, times = 2), rep(.49, times = 5))
survival <- data.frame(Habitat, life.stage, surv.base, wood.surv.base)

# Wood multipliers for large river ----
# Summer
lr_wd_s_bank <- 1.052
lr_wd_s_bar <- 1.011

# Winter
lr_wd_w_bank <- 1.069
lr_wd_w_bar <- 1.108

# Prespawn survival ----
prespawn_surv_raw <- .95
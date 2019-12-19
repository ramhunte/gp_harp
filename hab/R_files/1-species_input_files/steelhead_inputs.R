#Spawning#
fecundity = 5400
redd_area = 5.4
adult_per_redd = 1.3

#Rearing

Habitat = c(rep(c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater", "Pool", "Riffle", "Beaver.Pond", "FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm", "Bank_center", "HM_Bank_center", "Bar_boulder_center", "Bar_gravel_center", 
                  "Bar_sand_center"), times = 4))
life.stage = c(rep("summer", times = length(Habitat)/4), rep("winter", times = length(Habitat)/4), rep("summer.2", times = length(Habitat)/4), rep("winter.2", times = length(Habitat)/4))

year2_scalar = 0.2 # density scalar for juveniles in year 1 vs year 2
Density = c(3200, 2600, 4200, 2800, 200, 2000, 6300, 4800, 0, 0, 0, 0, 0, 0, 6300, 4800, 0, 0, 3200*.05, 3200*.05, 3200*.05, 3200*.05, 3200*.05, 
            2100, 1800, 1300, 2000, 0, 800, 1400, 900, 300, 0, 0, 0, 300, 300, 1400, 900, 0, 0, 2100*.05, 2100*.05, 2100*.05, 2100*.05, 2100*.05,
            c(3200, 2600, 4200, 2800, 200, 2000, 8500, 3000, 3500, 0, 0, 0, 3500, 3500, 8500, 3000, 0, 0, 3200*.28, 3200*.28, 3200*.28, 3200*.28, 3200*.28) * year2_scalar, 
            c(2100, 1800, 1300, 2000, 0, 800, 3500, 1500, 500, 0, 0, 0, 500, 500, 3500, 1500, 0, 0, 2100*.28, 2100*.28, 2100*.28, 2100*.28, 2100*.28) * year2_scalar)
density <- data.frame(Habitat, life.stage, Density)

Habitat = c(rep(c("Bank", "HM_Bank", "Bar_boulder", "Bar_gravel", "Bar_sand", "Backwater", "Pool", "Riffle", "Beaver.Pond", "FP_Channel", "Lake", "Marsh", "FP_Pond_lg", "FP_Pond_sm", 
                  "SC_pool", "SC_riffle", "Slough_lg", "Slough_sm", "Bank_center", "HM_Bank_center", "Bar_boulder_center", "Bar_gravel_center", 
                  "Bar_sand_center"), times = 4))
life.stage = c(rep("summer", times = length(Habitat)/4), rep("winter", times = length(Habitat)/4), rep("summer.2", times = length(Habitat)/4), rep("winter.2", times = length(Habitat)/4))
surv.base = c(rep(.47, times = 8), rep(.59, times = 6), rep(.47, times = 2), rep(.59, times = 2), rep(.47, times = 5),
              rep(.32, times = 8), rep(.48, times = 6), rep(.32, times = 2), rep(.48, times = 2), rep(.32, times = 5),
              rep(.7, times = 23),
              rep(.45, times = 8), rep(.48, times = 6), rep(.45, times = 2), rep(.48, times = 2), rep(.45, times = 5))
wood.surv.base = c(rep(.47, times = 6), rep(.49, times = 2),  rep(.59, times = 6), rep(.49, times = 2), rep(.59, times = 2), rep(.47, times = 5),
                   rep(.32, times = 6), rep(.53, times = 2), rep(.48, times = 6), rep(.53, times = 2), rep(.48, times = 2), rep(.32, times = 5),
                   rep(.7, times = 6), rep(.73, times = 2), rep(.7, times = 6), rep(.73, times = 2), rep(.7, times = 7),
                   rep(.45, times = 6), rep(.49, times = 2), rep(.48, times = 6), rep(.49, times = 2), rep(.48, times = 2), rep(.45, times = 5))
survival <- data.frame(Habitat, life.stage, surv.base, wood.surv.base)

# Wood multipliers for large river ----
# Summer
lr_wd_s_bank <- 1.052
lr_wd_s_bar <- 1.011

# Winter
lr_wd_w_bank <- 1.069
lr_wd_w_bar <- 1.108

# Prespawn survival ----
prespawn_surv_raw <- .9
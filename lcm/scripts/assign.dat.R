# Purpose: this script loads one habitat scenario, and creates seperate vectors for each parameter held within the 
# scenario. For example, the row "eggs" in the csv is assigned to a vector (egg.cap) which is then used in teh subbasin function. 


# Read in habitat scenario file ----

dat <- read.csv(file.path('outputs',pop,'hab.scenarios','Current.csv'), header = TRUE, row.names = 2)




# remove first column of numbers
dat <- dat[,-1]
# remove NA and NaN
dat[is.na(dat)] <- 0

colnames(dat) <- c(reach.names)

dat <- as.matrix(dat)



# Egg capacity
egg.cap <- dat["eggs", ]



# Egg to fry survival
egg.fry.surv <- dat["eggtofry_surv", ]


#Proportions of total run in each DU
prop.init <- dat["adults", ]/sum(dat["adults", ])


# Upstream survival, calculated in the hab model using barriers
S.up <- dat['prespawn_surv', ]


if (pop == "coho") {
  
  # Fry to parr survival and capacity
  parr.surv <- dat["surv_s", ]
  parr.cap  <- dat["capacity_s", ]
  
  # Fall redistribution
  # Historical wood gets 7% redistribution, historical ponds and all historical get 3%. All other scenarios get 11%.

  redist <- dat["movement", ]/100 # If starts with ASRP
    
  # Parr-to-smolt survival and capacity
  parr.smolt.surv <- dat["surv_w",]
  parr.smolt.cap <- dat["capacity_w",]
  
} #end if coho


if (pop == "fall_chinook" | pop == "spring_chinook") {
  
  # Fry to sub yearling migrant survival and capacity
  weekly.surv <- dat['surv_s', ]^(1/12) # 1 week of freshwater mortality
  cap <- dat['capacity_s', ]
  
  # Weekly productivity scaled with June temperatures
  # This is weighted so that 45% of fish will get the impact of temperature for 4 of 11 weeks of the subyr rearing period
  w <- (0.45 * 4/11)
  
  weekly.surv.temp <- (w * dat['surv_s_2', ]^(1/12)) + ((1 - w) * weekly.surv) 
  
} # end if chinook

if (pop == "steelhead") {
  
  #first summer parr
  parr.cap <-  dat["capacity_s", ]
  parr.surv <- dat["surv_s", ]
  
  #1st overwinter Age1
  first.winter.cap <-  dat["capacity_w", ]
  first.winter.surv <- dat["surv_w", ]
  
  #2nd summer: Age1.plus
  second.summer.cap <-  dat["capacity_s_2", ]
  second.summer.surv <- dat["surv_s_2", ]
  
  #2nd overwinter: Age2
  second.winter.cap <-  dat["capacity_w_2", ]
  second.winter.surv <- dat["surv_w_2", ]
}


if (pop == 'chum') {
  # fry colonization
  fry.colonization.cap <- dat['fry_colonization_cap', ]
  fry.colonization.surv <- dat['fry_colonization_surv', ]
  
  # fry to smolt 
  fry.migrant.surv <- dat['surv_s', ]
  
}


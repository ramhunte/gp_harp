# Purpose: this script loads one habitat scenario, and creates seperate vectors for each parameter held within the 
# scenario. For example, the row "eggs" in the csv is assigned to a vector (egg.cap) which is then used in teh subbasin function. 


# Read in habitat scenario file ----

dat <- read.csv(file.path(hab.path, habitat.file[n]), header = TRUE, row.names = 2)




# remove first column of numbers
dat <- dat[,-1]
# remove NA and NaN
dat[is.na(dat)] <- 0

colnames(dat) <- c(reach.names)

dat <- as.matrix(dat)



# Egg capacity
egg.cap <- dat["eggs",]*egg.cap.adj



# Egg to fry survival
egg.fry.surv <- dat["eggtofry_surv",]*egg.fry.surv.adj


#Proportions of total run in each DU
prop.init <- dat["adults",]/sum(dat["adults",])


# Upstream survival, calculated in the hab model using barriers
S.up <- dat['prespawn_surv',]


if (pop == "coho") {
  
  # Fry to parr survival and capacity
  parr.surv <- dat["surv_s",]*sub.yr.surv.adj
  parr.cap  <- dat["capacity_s",]*sub.yr.cap.adj
  
  # Fall redistribution
  # Historical wood gets 7% redistribution, historical ponds and all historical get 3%. All other scenarios get 11%.
  redist_7 <- c('Wood')
  redist_3 <- c('Beaver','Historical')
  redist <- if (scenario.file[n] %in% redist_7) {redist.histwood
            } else if (scenario.file[n] %in% redist_3) {redist.histpond
            } else if (substr(scenario.file[n],1,4) == 'ASRP') {dat["movement", ]/100 # If starts with ASRP
            }else redist.current
    
  # Parr-to-smolt survival and capacity
  parr.smolt.surv <- dat["surv_w",]*ps.surv.adj
  parr.smolt.cap <-dat["capacity_w",]*ps.cap.adj
  
  #Harvest for the historical no beaver scenario set to 0.67
  Hr <- ifelse(substr(habitat.file[n],1,15) == "Historical_no_b",Hr.hist,Hr.cur)
  
  
} #end if coho

if(pop=="steelhead"){
 
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


if (pop == "fall.chinook" | pop == "spring.chinook"){
  
  # Fry to sub yearling migrant survival and capacity
  fry.surv <- (dat['surv_s',] * fry.surv.adj )^(1/12) # 1 week of freshwater mortality
  sub.yr.surv <- (fry.surv^11) * sub.yr.surv.adj # 11 more weeks of freshwater mortality
  sub.yr.cap  <- dat['capacity_s',]*sub.yr.cap.adj
  
} #end if chinook






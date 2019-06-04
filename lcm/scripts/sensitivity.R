
# Adjust parameter values to run sensitivity test
# Randomly adjust parameters by +/- 20%


# Create random multiplier ----
adj <- function() {
  runif(n = 1, min = 0.8, max = 1.2)
}

adj.skook <- function() {}
adj.newaukum <- 

# Adjust params for all species ----

# Egg capacity
egg.cap.adj <- adj()
egg.cap <- dat["eggs", ] * egg.cap.adj

# Egg to fry survival
egg.fry.surv.adj <- adj()
egg.fry.surv <- dat["eggtofry_surv",] * egg.fry.surv.adj

# Upstream survival, calculated in the hab model using barriers
S.up.adj <- adj()
S.up <- dat['prespawn_surv',] * S.up.adj




# Adjust coho params ----
if (pop == "coho") {
  # Fry to parr survival and capacity
  parr.surv.adj <- adj()
  parr.cap.adj <- adj()
  
  parr.surv <- dat["surv_s", ] * parr.surv.adj
  parr.cap  <- dat["capacity_s", ] * parr.cap.adj

  
  # Parr-to-smolt survival and capacity
  parr.smolt.surv.adj <- adj()
  parr.smolt.cap.adj <- adj()
  
  parr.smolt.surv <- dat["surv_w", ] * parr.smolt.surv.adj
  parr.smolt.cap <- dat["capacity_w", ] * parr.smolt.cap.adj
  
  
} #end if coho


# Adjust chino params ----
if (pop == "fall.chinook" | pop == "spring.chinook") {
  
  cap <- dat["capacity_s",]
  newauk.cap.adj <- adj()
  cap[12] <- cap[12]*newauk.cap.adj # Newaukum
  skook.cap.adj <- adj()
  cap[18] <- cap[18]*skook.cap.adj # Skook
  mainstem.cap.adj <- adj()
  cap[52:63]  <- cap[52:63]*mainstem.cap.adj # mainstem
  
  weekly.surv <- dat['surv_s', ]^(1/8)
  newauk.surv.adj <- adj()
  weekly.surv[12] <- weekly.surv[12]*newauk.surv.adj
  skook.surv.adj <- adj()
  weekly.surv[18] <- weekly.surv[18]*skook.surv.adj
  mainstem.surv.adj <- adj()
  weekly.surv[52:63] <- weekly.surv[52:63]*mainstem.surv.adj
  
  # Fry to sub yearling migrant survival and capacity
  # fry.surv.adj <- adj() # 1 week of freshwater mortality
  # fry.cap.adj <- adj()
  # sub.yr.surv.adj <- adj() # 11 more weeks of freshwater mortality
  # sub.yr.cap.adj  <- adj()
  # fry.surv <- (dat['surv_s', ]^(1/12)) * fry.surv.adj # 1 week of freshwater mortality
  # fry.cap <- dat['capacity_s',] * 3 * fry.cap.adj
  # 
  # sub.yr.surv <- (dat['surv_s', ]^(11/12)) * sub.yr.surv.adj # 11 more weeks of freshwater mortality
  # sub.yr.cap  <- dat['capacity_s', ] * sub.yr.cap.adj
  
} #end if chinook


# Adjust sthd params ----
if (pop == "steelhead") {
  #first summer parr
  parr.cap.adj <- adj()
  parr.surv.adj <- adj()
  
  parr.cap <-  dat["capacity_s", ] * parr.cap.adj
  parr.surv <- dat["surv_s", ] * parr.surv.adj
  
  #1st overwinter Age1
  first.winter.cap.adj <-  adj()
  first.winter.surv.adj <- adj()
  
  first.winter.cap <-  dat["capacity_w", ] * first.winter.cap.adj
  first.winter.surv <- dat["surv_w", ] * first.winter.surv.adj
  
  #2nd summer: Age1.plus
  second.summer.cap.adj <-  adj()
  second.summer.surv.adj <- adj()
  
  second.summer.cap <-  dat["capacity_s_2", ] * second.summer.cap.adj
  second.summer.surv <- dat["surv_s_2", ] * second.summer.surv.adj
  
  #2nd overwinter: Age2
  second.winter.cap.adj <-  adj()
  second.winter.surv.adj <- adj()
  
  second.winter.cap <-  dat["capacity_w_2", ] * second.winter.cap.adj
  second.winter.surv <- dat["surv_w_2", ] * second.winter.surv.adj
} # end if sthd


# Check if any productivity is over 1, and force it to be 1.0 if it is
surv.params <- c('egg.fry.surv',
                 'S.up',
                 'parr.surv',
                 'parr.smolt.surv',
                 'fry.surv',
                 'sub.yr.surv',
                 'first.winter.surv',
                 'second.summer.surv',
                 'second.winter.surv',
                 'second.winter.surv')

param.exists <- lapply(surv.params, exists) %>% unlist

for (p in surv.params[param.exists]) {
  pvec <- get(p)
  pvec[pvec > 1] <- 1

  assign(p, pvec)
}

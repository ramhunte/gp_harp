
# Function to reduce capacity following Larry's disaggregated B-H framework ----
reduce_capacity <- function(p_pristine,c_pristine,scalar){
  # This should be used when there is an alteration in density or productivity/survival, 
  # but no associated drop in capacity (from the literature)
  # p_pristine = pristine survival, or survival from unaltered conditions
  # c_pristine = pristine capacity, or capacity from unaltered conditions
  # scalar = the ratio of p_altered to p_pristine (e.g. 0.6)
  
  p_scaled <- p_pristine*scalar
  d <- 20
  g <- 1.04
  pw <- p_pristine^(1/d)
  pw_scaled <- p_scaled^(1/d)
  
  c_pure_0 <- (c_pristine/p_pristine)*(pw*g)*((1-(p_pristine*g^d))/(1-(pw*g)))
  
  for (i in 1:20){             
    c_pure <- (c_pure_0/g^(i-1))*((1-pw_scaled*g)/(g*pw_scaled*((1/pw_scaled)-g)))
    c_cumul <- if(i == 1) {pw_scaled/(pw_scaled/c_pure)
    } else {pw_scaled/((1/c_cumul)+(pw_scaled/c_pure)) 
    } # end ifelse
  }# end for loop
  
  c_cumul
} # end reduce_capacity function

# Test reduce_capacity
#reduce_capacity(p_pristine = .7, c_pristine = 1000, scalar = 0.5)




# Function to calculate prespawn mortality based on temperature ----

bowerman_ps <- function(t,
                        phos = 0,
                        b0 = -9.053,
                        b1 = .387,
                        b2 = .029) {
  log_p <- b0 + b1 * t + b2 * phos
  p <- 1 - plogis(log_p)
  return(p)
}

#### ASRP functions ----
#define variables below func#
asrp_temp_func <- function(a= "Current temperature", b = "Historical temperature", c = "restoration percent", d = "intensity scalar") {
  
  a - ((a - b) * c * d)
}

temp_func <- function(t = "temperature"){
  if (fishtype %in% c("spring_chinook", 'fall_chinook')) {
    ifelse(t < 18,
           1,
           ifelse(t >= 18 & t < 24,
                  -(1/6) * t + 4,
                  0))
  } else if (fishtype == "steelhead") {
    ifelse(t <= 17,
           t/t,
           (97.8846 / (1 + exp(-((t - 24.3522) / -.5033)))) / 100 )
  } else if (fishtype == 'coho') {
    ifelse(t < 17,
           1,
           ifelse(t >= 17 & t < 28,
                  -(0.09 * t) + 2.55,
                  0))
  } 
  else {
    t/t #no tempmult for chum
  }
}

temp_func_high_food <- function(t) {
  ifelse(t < 18,
         1,
         ifelse(t >= 18 & t < 28,
                -(1/10) * t + 2.8,
                0))
}

temp_func_high_food2 <- function(t) {
  ifelse(t < 19,
         1,
         ifelse(t >= 19 & t < 28,
                -(1/9) * t + 3.11,
                0))
}

temp_func_high_food3 <- function(t) {
  ifelse(t < 20,
         1, 
         ifelse(t >= 20 & t < 28,
                -(1/8) * t + 3.5,
                0))
}




calc_coho_imperv <- function(imperv) {
  # Calc impervious area impact on coho prespawn productivity (Feist et al 2011, 2017)
  # https://trello.com/c/WrGi7d7U/31-model-prespawn-mortality-for-coho-using-impervious-landcover
  # Calc the mortality, then take 1 - mortality to return the productivity
  #
  # Args: 
  #  imperv: percent imperviousness in decimal form (1% = 0.01)
  #
  # Returns:
  #  ps_mort_imperv: prespawn mortality related to imperviousness in decimal form
  if (fishtype == 'coho') {
  ps_mort_imperv <- 1.5 * imperv
  
  ps_prod_imperv <- ifelse(ps_mort_imperv > 1,
                           1 - 1,
                           1 - ps_mort_imperv)
  
  
    return(ps_prod_imperv)
  } else {return(1)}
}

# mwmt_to_prespawn_func <- function(mwmt) { # Where necessary, convert temp_diff due to tree growth from mwmt to mean of daily mean
#   .82749 * mwmt
# }

mwmt_to_mdm_func <- function(mwmt) {
  .97550 * mwmt
}
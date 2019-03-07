
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

prespawn.chin.func <- function(temps, a = 0.7919, b = -13.2061, c = 0){
  # This function estimates Chehalis spring Chinook salmon prespawning survival
  # as a function of Chehalis basin August 7DADM
  #  temps = PSU/ICF 7DADM to be converted to prespawn survival
  # The following are from the Willamette Chinook fitted psm relationship:
  # logit(psm) ~ psm.b0 + psm.jul1sept15meanTemp*Temperature + psm.phos*pHOS, where
  #  psm.b0 <- -13.2061# b
  #  psm.jul1sept15meanTemp <- 0.7919# a
  #  psm.phos <- 2.0923# c; set to 0 for this application
  # Note: using a conservative approach we assume that pHOS = 0, meaning that
  # psm.phos*pHOS = 0, so the c term drops out
  # Change temperature missings to NAs
  temps[temps < 0] <- NA
  # PSM function from Willamette needs different temperature measure, so
  # convert PSU/ICF 7DADM to mean of daily means, July1 - Sept15
  #  using this fitted relationship:
  temps.converted <- -3.277448 + 1.011789*temps

  inverse.logit <- function(a, b, c, x){
    # This function back-transforms a prespawn
    # mortality estimate on the logit scale
    exp(b + a*x + c)/(1 + exp(b + a*x + c))
  }

  psm <- inverse.logit(a = a, b = b, c = c, x = temps.converted)
  # Convert mortality to survival
  pss <- 1 - psm
  pss

}

# Test prespawn.chin.func
#prespawn.chin.func(20)


cramer.prespawn <- function(x){
  # cites go to Cramer 2001
  # Cramer, S. 2001. The relationship of stream habitat features to potential 
  # for production of four salmonid species. S.P. Cramer & Associates, Inc., 
  # February 2001 report to Oregon Building Industry Association, Gresham, OR.
  # where x = prespawning temperature
  # Applications where it's been used in LCMs:
  # mean of daily max 15 July through 15 August; Bartz et al. 2006; Scheuerell et al. 2006
  # mean of daily max Aug. - Sept.; Jorgensen et al. 2009; Honea et al. 2009; Honea et al. 2016
  y <- rep(NA, length(x))
  for(i in 1:length(x)){
    if (x[i] < 16) {y[i] <- 1}
    if (x[i] >= 16 & x[i] < 22.6) {y[i] <- 1 - 0.15*(x[i] - 16)}
    if (x[i] >= 22.6) {y[i] <- 0.01}
  }
  y
}

# x <- seq(10, 25, length=200)

# plot(x, cramer.prespawn(x), las=1,
#      xlab=expression(Temperature~~(degree~C)),
#      ylab="Prespawn survival", type="l")



#### ASRP functions ----
#define variables below func#
asrp_temp_func <- function(a= "Current temperature", b = "Historical temperature", c = "restoration percent", d = "intensity scalar") {
  
  a - ((a - b) * c * d)
}

temp_func <- function(t = "temperature"){
  if (fishtype == "coho") {
    ifelse(t < 18,
           1,
           ifelse(t >= 18 & t < 24,
                  -(1/6) * t + 4,
                  0))
  } else if (fishtype == "steelhead") {
    if (t <= 17) {
      t/t
    } else{
      (97.8846 / (1 + exp(-((t - 24.3522) / -.5033)))) / 100 
    }
  } else {
    t/t #tempmult = 1 for chinook
  }
}

# Adjust parameter values to run sensitivity test
# Need to randomly adjust parameters within a specified range


# Adjust parameters by +/- 20% ----

egg.cap.adj <- runif(n=1, min=0.8, max=1.2)
egg.fry.surv.adj <- runif(n=1, min=0.8, max=1.2)
sub.yr.surv.adj <- runif(n=1, min=0.8, max=1.2)
sub.yr.cap.adj <- runif(n=1, min=0.8, max=1.2)

if (pop == "coho"){ 
  ps.surv.adj <- runif(n=1, min=0.8, max=1.2)
  ps.cap.adj <- runif(n=1, min=0.8, max=1.2)
}


if (pop == "fall.chinook" | pop == "spring.chinook"){ 
  fry.surv.adj <- runif(n=1, min=0.8, max=1.2)
  bay.parr.surv.adj <- runif(n=1, min=0.8, max=1.2)
  bay.fry.surv.adj <- runif(n=1, min=0.8, max=1.2)
}






# Adjust absolute value of parameters ----
# Warning: same value applied to all DUs

# egg.cap <- runif(n=1, min=0.8, max=1.2)
# egg.fry.surv <- runif(n=1, min=0.8, max=1.2)
# parr.surv <- runif(n=1, min=0.8, max=1.2)
# parr.cap <- runif(n=1, min=0.8, max=1.2)
# 
# 
# if (pop == "coho"){ 
#   ps.surv <- runif(n=1, min=0.8, max=1.2)
#   ps.cap <- runif(n=1, min=0.8, max=1.2)
# }


#if (pop == "fall.chinook" | pop == "spring.chinook"){
#shared.fresh.surv <- runif(n=1, min=0.7, max=0.9)
#bay.parr.surv <- runif(n=1, min=0.04, max=0.06)
#bay.fry.surv <- runif(n=1, min=0.0005, max=0.002)
#}

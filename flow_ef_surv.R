# Convert Skagit flow to Chehalis flow

library(EGRET)

# Grand mound ----
siteNumber <- "12027500" #Grand Mound USGS gage
startDate <- "" # Get earliest date
endDate <- "2019-9-30" # Get latest date
Daily_gm <- readNWISDaily(siteNumber,"00060",startDate,endDate)

peak_gm <- Daily_gm %>%
  group_by(waterYear) %>%
  summarize(Q_max = max(Q))

ecdf_gm <- ecdf(peak_gm$Q_max) # create prob function

flow_to_RI_gm <- function(q) {(1 / (1 - ecdf_gm(q)))}

usgs_gm <- peak_gm %>%
  mutate(returnYr = flow_to_RI_gm(Q_max),
         returnYr = ifelse(is.infinite(returnYr), 100, returnYr)) %>%
  mutate(diff_perc_rcp45_2050 = predict(mods$fit[[1]], newdata = .),
         diff_perc_rcp45_2080 = predict(mods$fit[[2]], newdata = .),
         diff_perc_rcp85_2050 = predict(mods$fit[[3]], newdata = .),
         diff_perc_rcp85_2080 = predict(mods$fit[[4]], newdata = .),
         Q_rcp45_2050 = Q_max + Q_max * diff_perc_rcp45_2050,
         Q_rcp45_2080 = Q_max + Q_max * diff_perc_rcp45_2080,
         Q_rcp85_2050 = Q_max + Q_max * diff_perc_rcp85_2050,
         Q_rcp85_2080 = Q_max + Q_max * diff_perc_rcp85_2080
         )

usgs_gm %>%
  select(waterYear,Q_max,Q_rcp45_2050:Q_rcp85_2080) %>%
  gather(model, Q, Q_max:Q_rcp85_2080) %>%
  mutate(era = ifelse(model == 'Q_max' , 'Observed',
               ifelse(str_detect(model,'2050'), 'Mid-century',
                      'Late-century')),
         era = factor(era, levels = c('Observed', 'Mid-century', 'Late-century')),
         climate = ifelse(model == 'Q_max' , 'Current',
                          ifelse(str_detect(model,'rcp45'), 'RCP 4.5',
                                 'RCP 8.5')),
         Q = Q * 35.3147,
         waterYear = waterYear - min(waterYear)) %>%
  filter(climate != 'Current') %>%
  ggplot +
  geom_line(aes(waterYear, Q, color = climate, size = climate)) +
  facet_wrap(~era, ncol = 1) +
  scale_color_manual(values = c('orange1','orangered2')) +
  scale_size_manual(values = c(1, 0.8)) +
  theme_bw() +
  labs(x = 'Year',
       y = bquote('Peak Annual Flow (' ~ ft^3 ~s^-1* ')'),
       color = 'Emissions\nscenario',
       size = 'Emissions\nscenario')

ggsave('../misc/AGU_poster/modeled_peakQ.jpg', dpi = 300, width = 8, height = 4)


# Observed flow ----
usgs_gm %>%
  select(waterYear, Q_max, returnYr) %>%
  mutate(Q_max = Q_max * 35.3147) %>%
  gather(metric, value, Q_max:returnYr) %>%
  mutate(metric = ifelse(metric == 'Q_max', 'Peak Annual Flow (cfs)', 'Return Interval')) %>%
  ggplot +
  geom_line(aes(waterYear, value)) +
  facet_wrap(~metric, 
             ncol = 1, 
             scales = 'free_y',
             strip.position = 'left') +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.placement = "outside")
  
ggsave('../misc/AGU_poster/observed_peakQ.jpg', dpi = 300, width = 8, height = 4)
# # Skagit ----
# siteNumber <- "12200500" # Skagit
# startDate <- "" # Get earliest date
# endDate <- "2019-9-30" # Get latest date
# Daily_sk <- readNWISDaily(siteNumber,"00060",startDate,endDate)
# 
# peak_sk <- Daily_sk %>%
#   group_by(waterYear) %>%
#   summarize(Q_max = max(Q))
# 
# ecdf_sk <- ecdf(peak_sk$Q_max)



# Join them ----
# ri_gm <- quantile(ecdf_gm, probs = seq(.01,.99,.01)) %>%
#   enframe() %>%
#   rename(Q_gm = value) %>%
#   mutate(prob = 1 - as.numeric(sub("%", "", name))/100,
#          RI = 1/prob) %>%
#   select(RI, Q_gm)
# 
# ri_sk <- quantile(ecdf_sk, probs = seq(.01,.99,.01)) %>%
#   enframe() %>%
#   rename(Q_sk = value) %>%
#   mutate(prob = 1 - as.numeric(sub("%", "", name))/100,
#          RI = 1/prob) %>%
#   select(RI, Q_sk)
# 
# 
# ri_translate <- full_join(ri_gm, ri_sk)



# EF vs flow ----

recurr <- c(4,45,0,0,0,1,29,1,0,1,2,0,2,1,11,2,1)
surv <- c(9,1.2,13.7,14.4,16.7,10.2,3.8,15.6,16.4,16.5,12.7,13.5,12.9,10.8,5.8,7.3,11.4)
dat <- data.frame(recurr=recurr, surv=surv/100)

lm1 <- lm(log(surv/(1-surv))~recurr, data=dat)
summary(lm1)

inv.logit <- function(x){
  temp <- exp(x)/(exp(x) + 1)
  temp
}

# Plot of observed and model fits:
plot(recurr, surv/100,
     las=1, xlab="Recurrence interval",
     xlim=c(0,60),
     ylab="Egg to fry outmigrant survival\nSkagit River Chinook salmon")
points(seq(0,60, by=1), 
       inv.logit(predict(lm1, 
                         newdata=data.frame(recurr=seq(0,60, by=1)))), 
       type="l")


range.rescale <- function(x, 
                          min=inv.logit(predict(lm1, 
                                                newdata=data.frame(recurr=seq(0,60, 
                                                                              by=1))))[61],
                          max=inv.logit(predict(lm1, 
                                                newdata=data.frame(recurr=seq(0,60, 
                                                                              by=1))))[1]) 
{
  temp <- (x-min)/diff(range(c(min,max)))*1
  temp
}


# Plot on the new 0:1 scale
plot(recurr, range.rescale(x=surv/100),
     las=1, xlab="Recurrence interval",
     xlim=c(0,60), xaxs="i",
     type="n",
     ylim=c(0,1), yaxs="i",
     ylab="Egg to fry survival\ndeclination multiplier")
points(seq(0,60, by=1), 
       range.rescale(inv.logit(predict(lm1, 
                                       newdata=data.frame(recurr=seq(0,60, by=1))))), 
       type="l")


range.rescale(inv.logit(predict(lm1, 
                                newdata=data.frame(recurr=seq(0,60, by=1)))))






RI_to_surv <- function(RI) {
  x <- predict(lm1, newdata = data.frame(recurr = RI)) %>%
    inv.logit() %>%
    range.rescale()
  
  x[x < 0] <- 0.01
  
  return(x)
}

RI_to_surv(2)

# Flows and survs from Kinsel 2008, Table 14 https://wdfw.wa.gov/publications/00093
# surv <- c(9,1.2,13.7,14.4,16.7,10.2,3.8,15.6,16.4,16.5,12.7,13.5,12.9,10.8,7.0,7.4,11.4,3.9)
# flows_cfs <- c(88200,142000,40100,27600,32100,55700,13200,47600,60600,51900,76800,19300,73700,5300,115000,66800,57400,125000)
# flows_cms <- flows_cfs * 0.028316847
# 
# 
# 
# sk_surv <- data.frame(RI = 1/(1 - ecdf_sk(flows_cms)),
#                       surv = surv) %>%
#   mutate(RI = ifelse(is.infinite(RI), 100, RI),
#          surv = surv /100)
# 
# 
# lm1 <- lm(log(surv/(1-surv)) ~ RI, data=sk_surv)
# 
# inv.logit <- function(x){
#   temp <- exp(x)/(exp(x) + 1)
#   temp
# }
# 
# inv.logit(predict(lm1, newdata = data.frame(RI=seq(0,60, by=1))))
# 
# sk_surv %>%
#   mutate(lm = predict(lm1, newdata = .))
# 
# plot(sk_surv$RI, sk_surv$surv)    
# points(seq(0,100, by=1), 
#        inv.logit(predict(lm1, 
#                          newdata=data.frame(RI=seq(0,100, by=1)))), 
#        type="l")

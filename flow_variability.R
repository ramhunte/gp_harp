
library(EGRET)

# siteNumber <- "12027500" #Grand Mound USGS gage
# startDate <- "" # Get earliest date
# endDate <- "2019-9-30" # Get latest date
# Daily_gm <- readNWISDaily(siteNumber,"00060",startDate,endDate)

# INFO <- readNWISInfo(siteNumber,"00060")
# eList <- as.egret(INFO, Daily, NA, NA)
# plotFlowSingle(eList, istat = 8, qUnit = "thousandCfs")




siteNumber <- "12020000" #Chehalis nr Doty
startDate <- "" # Get earliest date
endDate <- "2019-9-30" # Get latest date
paramCode <- '00060'
Daily_dy <- readNWISDaily(siteNumber, paramCode, startDate, endDate)

# INFO <- readNWISInfo(siteNumber,"00060")
# eList <- as.egret(INFO, Daily, NA, NA)
# plotFlowSingle(eList, istat = 8, qUnit = "thousandCfs")


library(tidyverse)

Daily_dy %>%
  group_by(waterYear) %>%
  summarize(Q_min = min(Q),
            Q_max = max(Q)) %>%
  ggplot() +
  geom_line(aes(waterYear, Q_max * 35.3147))

Daily_dy %>%
  #filter(waterYear == 2005) %>%
  ggplot() +
  geom_line(aes(Date, log(Q)))



daily_plot <- Daily_dy %>%
  mutate(period = ifelse(between(Day, 274, 365) | between(Day, 1, 91), 'Winter', 'Summer')) %>%
  group_by(waterYear) %>%
  summarize(Q_min = min(Q),
            Q_max = max(Q),
            Q_sd = sd(Q)) %>%
  gather(param, Q, Q_min:Q_sd)

daily_plot %>%
  filter(param != 'Q_sd') %>%
  ggplot(aes(waterYear, log(Q))) +
  geom_line() +
  #geom_smooth(method = 'lm') +
  facet_wrap(~param, scales = 'free_y', ncol = 1) +
  theme_bw()

ggsave('../misc/AGU_poster/flow_variation_Doty.jpg', width = 8, height = 6)

daily_plot %>%
  filter(param == 'Q_sd') %>%
  ggplot(aes(waterYear, log(Q))) +
  geom_line() +
  theme_bw() +
  labs(y = 'Standard Deviation')

ggsave('../misc/AGU_poster/standard_deviation_Doty.jpg', width = 8, height = 4)


# CIG results ----
library(lubridate)


read.csv('../misc/AGU_poster/CIG_model_results/streamflow_summaries/DHSVM/BCday/ChehalisR-nrDoty/MACA/ChehalisR-nrDoty_MACA-RCP85-full_DHSVM_BCday_PeakFlows.csv') %>%
  gather(model, value, bcc.csm1.1.m:NorESM1.M) %>%
  #mutate(Month = factor(Month, levels = c(10:12, 1:9))) %>%
  #mutate(ym = paste0(Year, "-", Month)) %>%
  ggplot() +
  geom_line(aes(WYear, value, group = model, color = model))


# Read in fish data ----

library(readxl)

x <- read_excel('../../LCM/Chehalis_data/SAR values_updated 08292017_Mara Zimmerman TJB.xlsx', 
                sheet = 'coho smolts',
                skip = 5) %>%
  select(`Smolt. Year`, Total.Smolts, `Total run`) %>%
  rename(year = `Smolt. Year`,
         total.run = `Total run`,
         smolts = Total.Smolts) %>%
  mutate(smolts = as.numeric(smolts),
         total.run_lag = lag(total.run),
         prod_fw = smolts / total.run_lag)  %>%
  filter(year != 1992)

y <- x %>%
  left_join(daily_plot %>%
              rename(year = waterYear) %>%
              spread(param, Q)) %>%
  select(year, prod_fw, Q_max, Q_min)

y %>%
  gather(param, value, prod_fw:Q_min) %>%
  ggplot +
  geom_line(aes(year, value)) +
  facet_wrap(~param, ncol = 1, scales = 'free_y')

y %>%
  filter(!is.na(prod_fw)) %>%
  ggplot(aes(Q_max, prod_fw)) +
  geom_point()


lm1 <- lm(log(prod_fw) ~ log(Q_max) + log(Q_min), data = y)

summary(lm1)



# Flow at ground mound



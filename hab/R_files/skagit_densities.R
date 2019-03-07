library(lubridate)

skagit_electrofish = read.delim("Excel Files/Skagit_electrofish_1993_99.txt", header = T)%>%
  mutate(month = month(as.POSIXlt(Date, format = "%m/%d/%Y")),
         Habitat = ifelse(Unit_Type == "Backwater", "Backwater",
                    ifelse(Unit_Type == "Bar_edge",
                           ifelse(Dominant_Substrate == "Boulder", "Bar_boulder",
                                  ifelse(Dominant_Substrate == "Gravel", "Bar_gravel",
                                         ifelse(Dominant_Substrate == "Sand", "Bar_sand", "other"))),
                           ifelse(Unit_Type == "Hydromodified_bank_edge", "HM_Bank",
                                  ifelse(Unit_Type == "Natural_bank_edge", "Bank",
                                         ifelse(Unit_Type == "Pool", "Pool", "other"))))))%>%
  group_by(month, Habitat)%>%
  summarize(coho_density = mean(Coho_0.)* 10000,
         chino_0_density = mean(Chinook_0.)*10000,
         chino_1_density = mean(Chinook_1.)*10000,
         steelh_0_density = mean(Rainbow_0.)*10000,
         steelh_1_density = mean(Rainbow_1._or_older)*10000)


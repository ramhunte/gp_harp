
# Disgnostic Plots
save.diagnostic <- file.path(out.path,'diagnostic_plots')
if (dir.exists(save.diagnostic) == F){dir.create(save.diagnostic)}


# Join all of the scenarios together and clean them up ----

if(pop == 'coho'){
  params.to.lifestages <- data.frame(stage_nm = row.names(dat),
                                     lifestage = c('eggs','spawners','eggs',rep('parr',2),rep('smolts',2)),
                                     type = c('prod',rep('cap',3),'prod','cap','prod'))
}

if (pop == "fall.chinook" | pop == "spring.chinook"){
  params.to.lifestages <- data.frame(stage_nm = row.names(dat),
                                     lifestage = c('eggs','spawners','eggs',rep('sub.yr',2),'prespawn'),
                                     type = c('prod',rep('cap',3),'prod','prod'))
}

filename <- dir(file.path('hab.scenarios',pop),full.names = T)

all.scenarios<- filename%>%
  map_dfr(read.csv,h=T,.id = 'name')%>%
  mutate(scenario = filename[as.numeric(name)]%>%basename(.)%>%gsub('_','\\.',.)%>%sub('\\.201.*','',.))%>%
  left_join(params.to.lifestages)%>%
  select(-X,-name,-stage_nm)

colnames(all.scenarios) <- c(reach.names,'scenario','lifestage','type')

all.scenarios <- all.scenarios%>%
  filter(substr(scenario,1,4) != 'ASRP') %>%
  gather(Subbasin,value,reach.names)%>%
  left_join(subbasins)%>%
  spread(type,value)%>%
  mutate(cap = ifelse(lifestage == 'spawners',NA,cap))%>%
  na_if(0)


if(pop == "fall.chinook" | pop == "spring.chinook"){
  all.scenarios <- all.scenarios%>%
    mutate(prod = ifelse(lifestage == 'sub.yr',prod^(7/8),prod),
           lifestage = ifelse(lifestage == 'sub.yr','natal.sub.yr',as.character(lifestage)))%>%
    rbind(all.scenarios%>%
            filter(lifestage == 'sub.yr')%>%
            mutate(cap = cap*3,
                   prod = prod^(1/8),
                   lifestage = 'natal.fry')
    )%>%
    rbind(all.scenarios%>%
            filter(lifestage == 'sub.yr')%>%
            mutate(cap = cap*3,
                   prod = prod^(1/8),
                   lifestage = 'ms.fry'))%>%
    rbind(all.scenarios%>%
            filter(lifestage == 'sub.yr')%>%
            mutate(prod = prod^(7/8),
                   lifestage = 'non.natal.sub.yr'))
}

# Create plot of basinwide fish populations for each lifestage ----

if(pop == 'coho') lifestages.to.plot <- unlist(c(lifestages[4:8],lifestages[1],lifestages[9:10]))
if(pop == "fall.chinook" | pop == "spring.chinook") lifestages.to.plot <- unlist(c(lifestages[c(9:17)],lifestages[1]))

scenario.to.plot <- scenario.file[scenario.file != "Historical.no.beaver"]

model.all.df <- model.all[,,lifestages.to.plot,,scenario.to.plot]%>%
  round(.,0)%>%
  as.data.frame.table(.)%>%
  rename(run = Var1, year = Var2,lifestage = Var3, subbasin = Var4, scenario = Var5, n = Freq)%>%
  group_by(run,year,lifestage,scenario)%>%
  summarize(n = sum(n))%>%
  ungroup()%>%
  left_join(plot.params)%>%
  left_join(all.scenarios%>%
              group_by(scenario,lifestage)%>%
              summarize(cap = sum(cap,na.rm=T),
                        prod = mean(prod,na.rm=T))%>%
              mutate(cap = ifelse(lifestage == 'spawners',NA,cap),
                     prod = ifelse(lifestage == 'spawners',NA,prod)))%>%
  mutate(lifestage = factor(lifestage,levels = lifestages.to.plot),
         scenario = factor(scenario,levels = c(plot.params$scenario)))


ggplot(model.all.df)+
  geom_boxplot(aes(scenario,n/1e5, fill = scenario),outlier.shape = NA)+
  geom_point(aes(scenario,cap/1e5),shape = 95, size = 7,color= 'red')+
  geom_text(aes(scenario,cap/1e5,label = round(prod,3)),vjust = -0.5, size = 3)+
  facet_wrap(~lifestage, scales = 'free_y',ncol = 4)+
  scale_fill_manual(values = plot.params$color,guide = F)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = '', y = 'Count of fish (in 100,000s)')

ggsave(file.path(save.diagnostic,paste('Diagnostic_AllLifeStages_',pop,paste0(format(Sys.time(), "%Y%m%d"),'.jpg'),sep = "_")),
       width = 20, height = 10, dpi = 300)




# Create the diagnostic plot for each Eco Region ----
all.scenarios.edr <- all.scenarios%>%
  group_by(scenario, lifestage, EcoRegion)%>%
  summarize(cap = sum(cap, na.rm=T),
            prod = mean(prod,na.rm=T))%>%
  mutate(cap = ifelse(lifestage == 'spawners',NA,cap),
         prod = ifelse(lifestage == 'spawners',NA,prod))

edr.to.plot <- unique(subbasins$EcoRegion)[-c(8,9)]


if(pop == 'coho') lifestages.to.plot.edr <- unlist(c(lifestages.to.plot,lifestages[11:13]))
if(pop == "fall.chinook" | pop == "spring.chinook") lifestages.to.plot.edr<- lifestages.to.plot


model.all.df.edr <- model.all[,,lifestages.to.plot.edr,,]%>%
  round(.,0)%>%
  as.data.frame.table()%>%
  rename(run = Var1, year = Var2, lifestage = Var3, Subbasin = Var4, scenario = Var5, n = Freq)%>%
  left_join(read.csv('data/subbasin_names.csv'))%>%
  group_by(run,lifestage,Subbasin,scenario,EcoRegion)%>%
  summarize(n = exp(mean(log(n))))%>%
  group_by(run,lifestage,scenario,EcoRegion)%>%
  summarize(n= sum(n,na.rm=T))%>%
  ungroup()%>%
  left_join(plot.params)%>%
  left_join(all.scenarios%>%
              group_by(scenario, lifestage, EcoRegion)%>%
              summarize(cap = sum(cap, na.rm=T),
                        prod = mean(prod,na.rm=T)))%>%
  mutate(lifestage = factor(lifestage,levels = lifestages.to.plot.edr),
         scenario = factor(scenario,levels = c(plot.params$scenario)))%>%
  filter(EcoRegion %in% edr.to.plot)

if(pop != 'spring.chinook'){
  model.all.df.edr<- model.all.df.edr%>%
    mutate(n = n/1e3,
           cap = cap/1e3)
  }

# Plots of all life stages for each EDR
save.path.perEDR <- file.path(save.diagnostic,'fish_per_scenario_by_EDR')
if (dir.exists(save.path.perEDR) == F){dir.create(save.path.perEDR)}


for(i in 1:length(edr.to.plot)){
  
  model.all.df.edr%>%
    filter(EcoRegion == edr.to.plot[i])%>%
    ggplot()+
    geom_boxplot(aes(scenario,n, fill = scenario),outlier.shape = NA)+
    geom_point(aes(scenario,cap),shape = 95, size = 7,color= 'red')+
    geom_text(aes(scenario,cap,label = round(prod,3)),vjust = -0.5, size = 3)+
    facet_wrap(~lifestage, scales = 'free_y',ncol=4)+
    scale_fill_manual(values = plot.params$color,guide = F)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(x = '', 
         y = if(pop == 'spring.chinook'){'Count of fish'} else{'Count of fish (in 1000s)'},
         title = edr.to.plot[i])
  
  ggsave(file.path(save.path.perEDR,
                   paste('DiagnosticPlot',pop,
                         gsub(' ','\\_',edr.to.plot[i])%>%gsub(':','',.),
                         paste0(format(Sys.time(), "%Y%m%d"),'.jpg'),sep = "_")), 
         width = 20, height = 10, dpi = 300)
}



# Plots of all EDRs for each scenario
save.path.perScenario <- file.path(save.diagnostic,'fish_per_EDR_by_scenario')
if (dir.exists(save.path.perScenario) == F){dir.create(save.path.perScenario)}

for(i in 1:length(scenario.file)){
  
  model.all.df.edr%>%
    filter(scenario == scenario.file[i])%>%
    ggplot()+
    geom_boxplot(aes(EcoRegion,n, fill = scenario),outlier.shape = NA)+
    geom_point(aes(EcoRegion,cap),shape = 95, size = 7,color= 'red')+
    geom_text(aes(EcoRegion,cap,label = round(prod,3)),vjust = -0.5, size = 3)+
    facet_wrap(~lifestage, scales = 'free_y',ncol=4)+
    scale_fill_manual(values = plot.params$color,guide = F)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(x = '', 
         y = if(pop == 'spring.chinook'){'Count of fish'} else{'Count of fish (in 1000s)'},
         title = scenario.file[i])
  
  ggsave(file.path(save.path.perScenario,
                   paste('DiagnosticPlot',pop,
                         gsub(' ','\\_',scenario.file[i])%>%gsub(':','',.),
                         paste0(format(Sys.time(), "%Y%m%d"),'.jpg'),sep = "_")), 
         width = 20, height = 10, dpi = 300)
}



# By Subbasin ----

# model.all.df.subbasin <- model.all[,50:100,lifestages.to.plot,,scenario.to.plot]%>%
#   as.data.frame.table(.)%>%
#   rename(run = Var1, year = Var2,lifestage = Var3, Subbasin = Var4, scenario = Var5, n = Freq)%>%
#   group_by(run,lifestage,scenario,Subbasin)%>%
#   summarize(n = geo.mean(n))%>%
#   group_by(lifestage,scenario,Subbasin)%>%
#   summarize(n = mean(n))%>%
#   filter(n > 1)%>%
#   left_join(plot.params)%>%
#   left_join(all.scenarios)%>%
#   ungroup()%>%
#   mutate(scenario = factor(scenario,levels = c(plot.params$scenario)))



# # Plots of all Basins for each scenario
# save.path.perSub <- file.path(save.diagnostic,'fish_per_basin_by_scenario')
# if (dir.exists(save.path.perSub) == F){dir.create(save.path.perSub)}
# 
# 
# basins.to.plot <- unique(model.all.df.subbasin$Subbasin)
# 
# for(i in 1:length(basins.to.plot)){
# 
#   model.all.df.subbasin%>%
#     filter(Subbasin == basins.to.plot[i])%>%
#     ggplot()+
#     geom_boxplot(aes(scenario,n, fill = scenario),outlier.shape = NA)+
#     geom_point(aes(scenario,cap),shape = 95, size = 7,color= 'red')+
#     geom_text(aes(scenario,cap,label = round(prod,3)),vjust = -0.5, size = 3)+
#     facet_wrap(~lifestage, scales = 'free_y',ncol=4)+
#     scale_fill_manual(values = plot.params$color,guide = F)+
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#     labs(x = '',
#          y = 'Count of fish',
#          title = basins.to.plot[i])
# 
#   ggsave(file.path(save.path.perSub,
#                    paste('DiagnosticPlot',pop,
#                          gsub(' ','\\_',basins.to.plot[i])%>%gsub(':','',.),
#                          paste0(format(Sys.time(), "%Y%m%d"),'.jpg'),sep = "_")),
#          width = 20, height = 10, dpi = 300)
# }


#Testing ----




# Spatial diagnostic plots
# 
# fgdb <- "data/LCM_spatial.gdb"
# 
# # List all feature classes in a file geodatabase
# gdb_list <- ogrListLayers(fgdb)
# print(sort(gdb_list))
# 
# # Read the feature class
# edr <- readOGR(dsn=fgdb,layer="NOAA_EDR_20180724")
# 
# 
# #Fortify (turn into dataframe) and rejoin with attribute table ----
# edr_fort <- fortify(edr,region = 'EcoRegion')%>%
#   rename(EcoRegion = id)%>%
#   left_join(all.scenarios%>%
#               group_by(scenario, lifestage, EcoRegion)%>%
#               summarize(cap = sum(cap, na.rm=T),
#                         prod = mean(prod,na.rm=T))%>%
#               mutate(cap = ifelse(lifestage == 'spawners',NA,cap),
#                      prod = ifelse(lifestage == 'spawners',NA,prod)))%>%
#   left_join(plot.params)
# 
# centroids.df <- as.data.frame(coordinates(edr))%>%
#   mutate(EcoRegion = edr@data$EcoRegion)%>%
#   left_join(all.scenarios.edr)
# 
# 
# 
# ggplot()+
#   theme_void()+
#   geom_polygon(data=edr_fort%>%filter(scenario == 'Beaver'),
#                aes(x=long,y=lat,group=group,fill = scenario),
#                color="black")+
#   geom_text(data = centroids.df%>%
#               filter(lifestage == 'parr',
#                      scenario == 'Beaver'),
#             aes(V1,V2,label = paste0('parr cap = ', round(prod,3))))+
#   coord_equal()

  

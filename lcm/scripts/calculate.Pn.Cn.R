

# Create summary stats for the basin, Pn and Cn from Moussalli and Hillborn --------

# Create dataframe with correct names and lifestages
params.to.lifestages <- data.frame(stage_nm = c("eggtofry_surv", "adults", "eggs", "capacity_sp", 
                                                "capacity_s", "surv_s", "capacity_w", "surv_w",
                                                'spawner_to_egg','fry_colonization','SAR','prespawn'),
                                   lifestage = c('egg_to_fry','spawners','spawner_to_egg',
                                                 'fry_colonization', rep('fry_to_parr',2),rep('parr_to_smolt',2),
                                                 'spawner_to_egg','fry_colonization','SAR','prespawn'),
                                   type = c('prod','cap','cap','cap', 'cap','prod','cap',rep('prod',5)),
                                   stage_num = c(2,NA,1,3,4,4,5,5,1,3,6,7))


# Create dataframe with fixed parameters (not in hab scenario csvs)
fixed_params <- data.frame(Subbasin = reach.names,
                           spawner_to_egg= fecund/2,
                           fry_colonization= fry.colonization,
                           SAR = ((1-b2)*bay.min*so.min^2)+(b2*bay.min*so.min),
                           prespawn = S.sb)%>%
  t(.)%>%
  .[-1,]%>%
  as.data.frame()%>%
  mutate(stage_nm = row.names(.))%>%
  select(stage_nm,1:63)%>%
  rename_at(vars(paste0('V',1:63)), ~ paste0('X',1:63))

# Read in each hab scenario csv and bind the fixed parameters to it
# Create one dataframe with all scenarios
filename <-dir(file.path('hab.scenarios',pop),full.names = T) #Folder where all the hab scenarios are sitting

outputs<- lapply(filename,function(x){ #lapply each csv into the read.csv() pipe
  read.csv(x)%>%
    select(-X)%>%
    rbind(fixed_params)%>%
    filter(stage_nm!='adults')%>%
    left_join(params.to.lifestages)%>%
    rename_at(vars(paste0('X',1:63)), ~ reach.names)%>%
    select(-stage_nm)%>%
    gather(Subbasin,value,reach.names)%>%
    spread(type,value)%>%
    left_join(read.csv('data/subbasin_names.csv'))%>%
    mutate(scenario = x%>%basename(.)%>%gsub('_','\\.',.)%>%sub('\\.20.*','',.))
})%>%
  do.call('rbind',.)%>% # take list of dataframes and rbind them
  mutate(prod = as.numeric(prod),
         cap = as.numeric(cap),
         cap = ifelse(lifestage == 'fry_colonization',0,cap))%>%
  na_if(.,0)




# Create partial Pn and Cn values ----
# These apply to migrant fish that move down to the mainstem
# These values are still location specific, so they need to be multiplied by the initial Pn from the natal basin

# Natal stream Pn and Cn - no migration
natal_PnCn <- outputs%>%
  select(-EcoRegion)%>%
  na_if(.,0)%>%
  arrange(scenario,Subbasin_num,stage_num)%>%
  group_by(scenario,Subbasin)%>%
  mutate(cumprod = cumprod(prod),
         Pici = cumprod/cap)%>%
  summarize(Pn = cumprod[stage_num == 7],
            Pici = sum(Pici,na.rm=T),
            #Cn = Pn/sum(Pici, na.rm=T),
            Pn_natal_to_spring = cumprod[stage_num == 2],
            Pn_natal_to_fall = cumprod[stage_num == 4],
            Pici_natal_to_spring = sum(Pici[stage_num <= 2],na.rm=T),
            Pici_natal_to_fall = sum(Pici[stage_num <= 4],na.rm=T))%>%
  ungroup()%>%
  mutate(natal.basin = Subbasin,
         rearing.basin = Subbasin)%>%
  select(scenario, natal.basin,
         Pici,
         Pn,
         Pn_natal_to_spring,
         Pn_natal_to_fall,
         Pici_natal_to_spring,
         Pici_natal_to_fall)%>%
  unite('natal rearing',c(Pn,Pici))%>%
  unite('spring',c(Pn_natal_to_spring,Pici_natal_to_spring))%>%
  unite('fall',c(Pn_natal_to_fall,Pici_natal_to_fall))%>%
  gather(migration_type, value,`natal rearing`:fall)%>%
  separate(value,c('Pn_natal','Pici_natal'),sep='_')%>%
  mutate(Pn_natal = as.numeric(Pn_natal),
         Pici_natal = as.numeric(Pici_natal),
         Pn = ifelse(migration_type == 'natal rearing',Pn_natal,NA),
         Cn = ifelse(migration_type == 'natal rearing',Pn_natal/Pici_natal,NA))


migrant_PnCn <- left_join(
  
  #Spring movers
  outputs%>% 
    select(-EcoRegion)%>%
    na_if(.,0)%>%
    arrange(scenario,Subbasin_num,stage_num)%>%
    filter(stage_num >2, # Fry colonization onward
           Subbasin %in% ms.reaches)%>%
    group_by(scenario,Subbasin)%>%
    mutate(cumprod = cumprod(prod),
           Pici = cumprod/cap)%>%
    summarize(Pn_spring = cumprod[stage_num == 7],
              Pici_spring = sum(Pici, na.rm=T)),
  
  #Fall movers
  outputs%>% 
    select(-EcoRegion)%>%
    na_if(.,0)%>%
    arrange(scenario,Subbasin_num,stage_num)%>%
    filter(stage_num > 4, # parr to smolt onward
           Subbasin %in% ms.reaches)%>%
    group_by(scenario,Subbasin)%>%
    mutate(cumprod = cumprod(prod),
           Pici = cumprod/cap)%>%
    summarize(Pn_fall = cumprod[stage_num == 7],
              Pici_fall = sum(Pici, na.rm=T))
  )%>%
  ungroup()%>%
  unite('spring',c(Pn_spring,Pici_spring))%>%
  unite('fall',c(Pn_fall,Pici_fall))%>%
  gather(migration_type,value,spring:fall)%>%
  separate(value,c('Pn_migrant','Pici_migrant'),sep='_')%>%
  rename(rearing.basin = Subbasin)%>%
  mutate(Pn_migrant = as.numeric(Pn_migrant),
         Pici_migrant = as.numeric(Pici_migrant))



# Create weights for migrants ----
# % of fish that move from each natal basin to each mainstem reach
  
create_weights_df <- function(i){
  
  x <- sweep(move.matrix,MARGIN=2,i,`*`)%>%
    as.data.frame.table()%>%
    rename(rearing.basin = Var1, natal.basin = Var2,weight = Freq)
  
  if(i == percent.fry.migrants) {
    z <- map_df(seq_len(length(scenario.file)), ~x,.id='scenario')%>%
      mutate(scenario = scenario.file[as.numeric(scenario)],
             migration_type = 'spring')
  }
  
  if(i == redist.histpond) {
    z <- map_df(seq_len(length(redist_3)), ~x,.id='scenario')%>%
      mutate(scenario = redist_3[as.numeric(scenario)],
             migration_type = 'fall')
  }
  
  if(i == redist.histwood) {
    z <- x%>%
      mutate(scenario = redist_7,
             migration_type = 'fall')%>%
      select(scenario,rearing.basin,natal.basin,weight,migration_type)
  }
  
  if(i == redist.current) {
    y <- subset(scenario.file,!(scenario.file %in% c(redist_3,redist_7)))
    z <- map_df(seq_len(length(y)), ~x,.id='scenario')%>%
      mutate(scenario = y[as.numeric(scenario)],
             migration_type = 'fall')
  }
  
  # For fall migrants, weight needs to be the product of spring and fall movement
  z <- z%>%
    mutate(weight = ifelse(migration_type == 'fall', weight * (1-percent.fry.migrants),weight))
  
  return(z)
}


move_weights<- c(percent.fry.migrants,
                 redist.histpond, 
                 redist.histwood,
                 redist.current)

migrant_weights <-lapply(move_weights,create_weights_df)%>%
  do.call('rbind',.)%>%
  select(scenario,natal.basin,rearing.basin,migration_type,weight)

natal_weights <- migrant_weights%>%
  group_by(scenario,natal.basin)%>%
  summarize(weight = 1-sum(weight))%>%
  ungroup()%>%
  mutate(rearing.basin = natal.basin,
         migration_type= 'natal rearing')%>%
  select(scenario,natal.basin,rearing.basin,migration_type,weight)

weights <- rbind(migrant_weights,natal_weights)%>%
  arrange(scenario,natal.basin,migration_type)

# Error check
# migrant_weights%>%
#   group_by(scenario,natal.basin,migration_type)%>%
#   summarize(weight = sum(weight))




PnCn <- left_join(weights,natal_PnCn)%>%
  left_join(migrant_PnCn)%>%
  mutate(Pn = ifelse(migration_type != 'natal rearing',Pn_migrant*Pn_natal,Pn),
         Pici = ifelse(migration_type != 'natal rearing',Pici_natal+Pici_migrant,Pn),
         Cn = ifelse(migration_type != 'natal rearing',Pn/Pici,Cn))%>%
  select(scenario, natal.basin,rearing.basin,migration_type, weight, Pn, Cn)%>%
  mutate(Pn_w = Pn* weight,
         Cn_w = Cn*weight)%>%
  group_by(scenario,natal.basin)%>%
  summarize(Pn = sum(Pn_w),
            Cn = sum(Cn_w))%>%
  left_join(read.csv('data/Subbasin_names.csv')%>%
              rename(natal.basin = Subbasin))



# Abundance of spawners and smolts ----

summary.stages <- c('spawners','natal.smolts','non.natal.smolts')

abundance_by_subbasin <- model.all[,50:100,summary.stages,,]%>%
  apply(.,c(1,3,4,5),geo.mean)%>% # geomean across years
  apply(.,c(2,3,4),mean)%>% # mean of runs
  round(.,0)%>%# round to whole fish
  as.data.frame.table()%>%
  rename(lifestage = Var1,Subbasin = Var2, scenario = Var3, n = Freq)%>%
  spread(lifestage, n)%>%
  mutate(scenario = factor(scenario, levels = read.csv('data/scenarios.csv')$scenario))%>%
  filter(scenario != 'Historical.no.beaver')%>%
  arrange(scenario)%>%
  rename(natal.basin = Subbasin)


cumul_metrics <- left_join(abundance_by_subbasin,PnCn)%>%
  left_join(plot.params)%>%
  left_join(read.csv('data/scenarios.csv'))%>%
  mutate(Eq_spawners = Cn-(Cn/Pn))%>%
  select(scenario.label,natal.basin,EcoRegion,summary.stages,Pn,Cn,Eq_spawners)%>%
  mutate(scenario.label = factor(scenario.label, levels = read.csv('data/scenarios.csv')$scenario.label))%>%
  rename(scenario = scenario.label)


# Create outputs by subbasin
cumul_metrics%>%
  select(-EcoRegion)%>%
  mutate(spawners = prettyNum(round(spawners,0),big.mark = ','),
         natal.smolts = prettyNum(round(natal.smolts,0),big.mark = ','),
         non.natal.smolts = prettyNum(round(non.natal.smolts,0),big.mark = ','),
         Pn = round(Pn,2),
         Cn = prettyNum(round(Cn,0),big.mark = ','),
         Eq_spawners = prettyNum(round(Eq_spawners,0),big.mark = ','))%>%
  write.csv(file.path(out.path,paste0('summary_metrics_subbasin_',format(Sys.time(), "%Y%m%d"),'.csv')))


# Create outputs by EDR
cumul_metrics%>%
  group_by(scenario,EcoRegion)%>%
  summarize(spawners = sum(spawners),
            natal.smolts = sum(natal.smolts),
            non.natal.smolts = sum(non.natal.smolts),
            Pn = mean(Pn,na.rm = T),
            Cn = sum(Cn,na.rm = T),
            Eq_spawners = sum(Eq_spawners,na.rm = T))%>%
  mutate(spawners = prettyNum(round(spawners,0),big.mark = ','),
         natal.smolts = prettyNum(round(natal.smolts,0),big.mark = ','),
         non.natal.smolts = prettyNum(round(non.natal.smolts,0),big.mark = ','),
         Pn = round(Pn,2),
         Cn = prettyNum(round(Cn,0),big.mark = ','),
         Eq_spawners = prettyNum(round(Eq_spawners,0),big.mark = ','))%>%
  write.csv(file.path(out.path,paste0('summary_metrics_EDR_',format(Sys.time(), "%Y%m%d"),'.csv')))

# Create outputs Basinwide
cumul_metrics%>%
  group_by(scenario)%>%
  summarize(spawners = sum(spawners),
            natal.smolts = sum(natal.smolts),
            non.natal.smolts = sum(non.natal.smolts),
            Pn = mean(Pn,na.rm = T),
            Cn = sum(Cn,na.rm = T),
            Eq_spawners = sum(Eq_spawners,na.rm = T))%>%
  mutate(spawners = prettyNum(round(spawners,0),big.mark = ','),
         natal.smolts = prettyNum(round(natal.smolts,0),big.mark = ','),
         non.natal.smolts = prettyNum(round(non.natal.smolts,0),big.mark = ','),
         Pn = round(Pn,2),
         Cn = prettyNum(round(Cn,0),big.mark = ','),
         Eq_spawners = prettyNum(round(Eq_spawners,0),big.mark = ','))%>%
  write.csv(file.path(out.path,paste0('summary_metrics_Basinwide_',format(Sys.time(), "%Y%m%d"),'.csv')))

  


# outputs%>%
#   select(-EcoRegion)%>%
#   na_if(.,0)%>%
#   arrange(scenario,Subbasin_num,stage_num)%>%
#   group_by(scenario,Subbasin)%>%
#   mutate(cumprod = cumprod(prod),
#          Pici = cumprod/cap,
#          rearing.basin = Subbasin)%>%
#   filter(Subbasin %in% c('Elk Creek',
#                          'Mainstem: Middle Chehalis 3',
#                          'Mainstem: Middle Chehalis 1'),
#          scenario %in% c('Current','Temperature'))%>%
#   rename(natal.basin = Subbasin)%>%
#   select(scenario,stage_num,lifestage,natal.basin,rearing.basin,cap,prod)%>%
#   write.csv(file.path(out.path,paste0('Example_metrics_subbasin_',format(Sys.time(), "%Y%m%d"),'.csv')))


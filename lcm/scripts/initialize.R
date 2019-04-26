
# Initialize the model

# Purpose: this script is used to create vectors of subbasin names, scenario names, lifestages. It also create empty arrays which will be filled
# with the for() loops, and defines the movement matrix which defines the movement rules. 


# Path to habitat scenario data ----

hab.path <- outputs_hab

# File names that hold habitat senario data ----
habitat.file <- list.files(path = hab.path,pattern = ".csv")

# Date/version of habitat files
# last master tag - commits ahead of master - current commit hash
hab.ver <- system(command = "git describe --tags", intern = TRUE) 


# Habitat restoration scenario names from files:
scenario.file <- gsub('_', '\\.', habitat.file) %>%
  sub('\\.csv', '', .) %>%
  sub('\\.201.*', '', .)

# If in sensitivity mode, only run two scenarios
if (sensitivity.mode == 'yes') {
  scenario.file <- scenario.file[scenario.file %in% c('Current', 'Historical')]
}


# Subbasin/reach names ----
reach.names <- read.csv("lcm/data/subbasin_names.csv") %>%
  select(Subbasin) %>%
  mutate(Subbasin = as.character(Subbasin)) %>%
  unlist(use.names = FALSE)



# number of areas being modeled
num.reaches <- length(reach.names)







# Initialize arrays to hold model outputs --------------------------------------------------------------------------------------

# Coho lifestages and sensitivity parameters
if (pop == "coho") {
  lifestages <- list(
    'ocean0',
    'ocean1',
    'ocean2',
    'total.run',
    'spawners',
    # Above here is needed for spawner matrix
    'eggs',
    # Here and below are stored for diagnostics
    'pre.fry',
    'fry',
    'parr',
    'smolts',
    'fry.movers',
    'parr.movers',
    'natal.smolts',
    'non.natal.smolts'
  )
  
  sens.params <- c(
    "Egg.Capacity",
    "Egg.to.Fry.Survival",
    "Summer.Productivity",
    "Summer.Capacity",
    "Winter.Productivity",
    "Winter.Capacity",
    "Prespawn.Survival",
    "geomean")# model output
} #end if coho




if (pop == "fall.chinook" | pop == "spring.chinook") {
  lifestages <- c('ocean0',
                  'ocean1',
                  'ocean2',
                  'ocean3',
                  'ocean4',
                  'ocean5',
                  'total.run',
                  'spawners',  # Above here is needed for spawner matrix
                  'eggs', # Here and below are stored for diagnostics
                  'pre.fry',
                  'natal.fry',
                  'non.natal.fry',
                  'fry.migrants',
                  'fry.migrants.ms',
                  'non.natal.sub.yr',
                  'smolts.fry.migrants',
                  'smolts.non.natal.sub.yr',
                  'smolts.natal.sub.yr'
  )
  
  
  sens.params <- c(
    "Egg.Capacity",
    "Egg.to.Fry.Survival",
    "Fry.Survival",
    "Fry.Capacity",
    "Subyearling.Survival",
    "Subyearling.Capacity",
    "Prespawn.Survival",
    "geomean")
  
} # end if chinook



if (pop == "steelhead") {
  
  ocean.stages <- lapply(2:6, function(x) paste0('age',x,'.ocean')) %>% unlist
  firstspawn.stages <- lapply(2:6, function(x) paste0('age',x,'.firstspawn')) %>% unlist
  kelt.stages <- lapply(3:8, function(x) paste0('age',x,'.kelt')) %>% unlist
  
  
  lifestages <- c(ocean.stages,
                  firstspawn.stages,
                  kelt.stages,
                  'total.run',
                  'spawners',
                  'eggs',
                  'pre.fry',
                  'fry',
                  'parr',
                  'age1',
                  'age1.smolts',
                  'age1.plus',
                  'age2.smolts'
  )
  
  sens.params <- c(
    "Egg.Capacity",
    "Egg.to.Fry.Survival",
    "First.Summer.Survival",
    "First.Summer.Capacity",
    "First.Winter.Survival",
    "First.Winter.Capacity",
    "Second.Summer.Survival",
    "Second.Summer.Capacity",
    "Second.Winter.Survival",
    "Second.Winter.Capacity",
    "Prespawn.Survival",
    "geomean")# model output
} #end if steelhead





# Create arrays to hold fish lifestages by spatial units ----

N <-
  N.initialize <-
  mat.new <-
  matrix(
    0,
    nrow = length(lifestages),
    ncol = num.reaches,
    dimnames = list(lifestages, reach.names)
  )


sensitivity <- array(NA,
                     c(runs, length(sens.params), length(scenario.file)),
                     dimnames = list(1:runs, sens.params, scenario.file))


# Store all model results in 5 dimensional array: runs x years x lifestage x DUs x scenario
model.all <- array(
  NA,
  c(
    runs,
    years,
    length(lifestages),
    num.reaches,
    length(scenario.file)
  ),
  dimnames = list(1:runs, 1:years, lifestages, reach.names, scenario.file)
) 

# Store recruits referenced to brood year:
Recruits.preharvest <- Recruits.postharvest <- Spawners <- array(0, years)

# Create movement matrices for spring and fall  ----

to.upperms1 <- c(1,2) # 52
to.upperms2 <- c(3,52) # 53
to.midms1 <- c(4,6,7,53) # 54
to.midms2 <- c(5,8:11,54) # 55
to.midms3 <- c(12:16,55) # 56
to.lowms1 <- c(17,18,56) # 57
to.lowms2 <- c(19,21,57)# 58
to.lowms3 <- c(20,22,58)# 59
to.lowms4 <- c(23:27,59)# 60
to.lowms5 <- c(28:31,60)# 61
to.lowms6 <- c(32:38,61)# 62
to.lowms7 <- c(39:41,62)# 63


ms.reaches <- reach.names[grep("Mainstem", reach.names)]
trib.reaches <- subset(reach.names,!(reach.names %in% ms.reaches))

if (pop %in% c('spring.chinook', 'fall.chinook')) {
  gh.trib.reaches <- read.csv("lcm/data/subbasin_names.csv") %>%
    filter(EcoRegion %in% c('Grays Harbor Tributaries', 'Olympic Mountains')) %>%
    select(Subbasin) %>%
    unlist(use.names = F) %>%
    as.character()
  to.bay.reaches <- subset(reach.names, (reach.names %in% c(ms.reaches, gh.trib.reaches)))
  to.ms.reaches <- setdiff(trib.reaches, to.bay.reaches)
}


move.matrix <- matrix(
  0,
  nrow = length(ms.reaches),
  ncol = length(reach.names),
  dimnames = list(ms.reaches, reach.names)
)

# Create spring movmemnt matrix for coho
if (pop == 'coho') {
  move.matrix.spring <- move.matrix
  
  move.matrix.spring[1, to.upperms1] <- 1
  move.matrix.spring[2, to.upperms2] <- 1
  move.matrix.spring[3, to.midms1] <- 1
  move.matrix.spring[4, to.midms2] <- 1
  move.matrix.spring[5, to.midms3] <- 1
  move.matrix.spring[6, to.lowms1] <- 1
  move.matrix.spring[7, to.lowms2] <- 1
  move.matrix.spring[8, to.lowms3] <- 1
  move.matrix.spring[9, to.lowms4] <- 1
  move.matrix.spring[10, to.lowms5] <- 1
  move.matrix.spring[11, to.lowms6] <- 1
  move.matrix.spring[12, to.lowms7] <- 1
  
  move.matrix.spring[is.nan(move.matrix.spring)] <- 0
}

# Create fall movement matrix for all species
move.matrix[1:length(ms.reaches), to.upperms1] <- 1
move.matrix[2:length(ms.reaches), to.upperms2] <- 1
move.matrix[3:length(ms.reaches), to.midms1] <- 1
move.matrix[4:length(ms.reaches), to.midms2] <- 1
move.matrix[5:length(ms.reaches), to.midms3] <- 1
move.matrix[6:length(ms.reaches), to.lowms1] <- 1
move.matrix[7:length(ms.reaches), to.lowms2] <- 1
move.matrix[8:length(ms.reaches), to.lowms3] <- 1
move.matrix[9:length(ms.reaches), to.lowms4] <- 1
move.matrix[10:length(ms.reaches), to.lowms5] <- 1
move.matrix[11:length(ms.reaches), to.lowms6] <- 1
move.matrix[12:length(ms.reaches), to.lowms7] <- 1

move.matrix <-
  t(apply(move.matrix, 1, function(x)
    x / colSums(move.matrix != 0)
  )
  ) # Divide each 1 in the move matrix by the count of ms reaches per column

move.matrix[is.nan(move.matrix)] <- 0



# Create matrix to be used in the distribute functions ----

distribute.matrix <- matrix(
  0,
  nrow = length(ms.reaches) * 2 + 2,
  ncol = length(reach.names),
  dimnames = list(
    c(
      'before_movement',
      'after_movement',
      lapply(ms.reaches, function(x)
        paste0('movers into ', x)),
      lapply(ms.reaches, function(x)
        paste0('return percent ', x))
    ),
    reach.names
  )
)

redistribute.matrix <- matrix(
  0,
  nrow = length(ms.reaches) + 2,
  ncol = length(reach.names),
  dimnames = list(c(
    'before_movement',
    'after_movement',
    lapply(ms.reaches, function(x)
      paste0('returns from ', x))
  ),
  reach.names)
)

return.rows <- grep("return percent", row.names(distribute.matrix))


# Create list of upper basins (4 weeks outmigration), mid basins (2 weeks outmigration), and lower (0 weeks) ----

if (pop %in% c('spring.chinook', 'fall.chinook')) {
  ds_weeks <- rep(0,length(reach.names))
  ds_weeks[c(1:18, 52:56)] <- 4 # Upper basins are Skookumchuck River and upstream
  ds_weeks[c(19:39, 57:62)] <- 2 # Mid basins are DS of Skook to LMS 6
  # All else is Lower. GH tribs, Olympic Mts, LMS 7
}

# Initialize the model

# Purpose: this script is used to create vectors of subbasin names, scenario names, lifestages. It also create empty arrays which will be filled
# with the for() loops, and defines the movement matrix which defines the movement rules. 


# Path to habitat scenario data ----

hab.path <- 'outputs/fall_chinook/'

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
  lifestages <- c(
    'ocean0',
    'ocean1',
    'ocean2',
    'total.run',
    'spawners',
    # Above here is needed for spawner matrix
    'eggs',# Here and below are stored for diagnostics
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
    "Egg.capacity",
    "Incubation.productivity",
    "Summer.rearing.productivity",
    "Summer.rearing.capacity",
    "Winter.rearing.productivity",
    "Winter.rearing.capacity",
    "Prespawn.productivity",
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
                  'fry.migrants',
                  'natal.fry.distrib',
                  'sub.yr.distrib',
                  'sub.yr',
                  'fry.migrants.bay',
                  'sub.yr.bay'
  )
  
  
  sens.params <- c(
    "Egg.capacity",
    "Incubation.productivity",
    "Rearing.productivity",
    "Rearing.capacity",
    "Rearing.productivity.June",
    "Prespawn.productivity",
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
                  'age1.stayers',
                  'age1.smolts',
                  'age2.stayers',
                  'age2.smolts',
                  'age3.smolts'
  )
  
  sens.params <- c(
    "Egg.capacity",
    "Incubation.productivity",
    "First.summer.productivity",
    "First.summer.capacity",
    "First.winter.productivity",
    "First.winter.capacity",
    "Second.summer.productivity",
    "Second.summer.capacity",
    "Second.winter.productivity",
    "Second.winter.capacity",
    "Prespawn.productivity",
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
if (pop == 'coho' | pop == 'steelhead') {
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


# Create the move matrix, but where percents expect all ms reaches AND the natal reach
move.matrix.w.natal <- t(apply(move.matrix, 1, function(x)
  x / (colSums(move.matrix != 0) + 1)
  )
  ) # Divide each 1 in the move matrix by the count of ms reaches per column + 1 for natal basin
move.matrix.w.natal[is.nan(move.matrix.w.natal)] <- 0


# Create a move matrix where fish only move to the MS
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

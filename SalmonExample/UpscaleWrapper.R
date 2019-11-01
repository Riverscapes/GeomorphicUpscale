# This script Runs the upscale with user defined inputs

# Set required paths ---------------------------

# User defined project directory path
proj.dir = "C:/Anabranch/UpperSalmon/wrk_Data/GUTUpscale/Run_01"

# Directory path to the downloaded Git Repo
repo.dir = "C:/etal/LocalCode/GeomorphicUpscale"

# Path to network csvs
chinook.network.file = "C:/etal/LocalCode/GeomorphicUpscale/SalmonExample/Chinook_Network.csv"
steelhead.network.file = "C:/etal/LocalCode/GeomorphicUpscale/SalmonExample/Steelhead_Network.csv"
braid.index.file = NA

# User defined variables ---------------------------

# optional vector argument to set RS factor order in graphs and displays, leave as  NA if alphabetical is desired
RSlevels = c('AF', 'WA', 'PC', 'MC', 'CF', 'CB', 'CC') 

# list of geoindicators to create plots for
geoindicators = c('Confinement', 'Grad', 'BfBraid', 'Sin', 'Bedform', 'LWFreq_Wet', 'SubEstBldr', 'SubEstCbl', 'SubEstGrvl', 'SubEstSandFines')


# Dependencies -------------------------------------------------------------------

library(tidyverse)
library(purrrlyr)
source(file.path(repo.dir, "scripts/plot.colors.R"))
source(file.path(repo.dir, "scripts/functions.R"))

# Read in selections data in case it was cleared
selections.file = file.path(proj.dir, "Inputs", "Selections.csv")
selections = read_csv(selections.file)


# Generate selection geoindicator plots ---------------------------

# geoindicator summary script
source(file.path(repo.dir, "scripts/selection.geoindicators.R")) 


# Generate assemblage output ---------------------------

# output per reach
source(file.path(repo.dir, "scripts/assemblage.reach.R"))

# output per unit type
source(file.path(repo.dir, "scripts/assemblage.unit.R"))


# Generate response output ---------------------------

# response by reach
source(file.path(repo.dir, "scripts/response.reach.R"))

# response by unit
source(file.path(repo.dir, "scripts/response.unit.R"))


# Generate upscale output ---------------------------

gu.type = "GU" 
response.pool = "byRSCond"
seg.id.col = "RECNO"        
length.col = "LENGTH"        
width.col = "WIDE_WW"       
cond.cols = c("COND_HIST", "COND_EX", "COND_RP")
area.col = "AREA_WW" # leave as NA if no area is specified per reach segment and it will be estimated but this requires braid index file
basin.col = "HUC_08"

if(!is.na(braid.index.file)){braid.index = read_csv(braid.index.file)}else{braid.index = NA}

# read in chinook nework and filter out rows with missing widths
chinook.network = read_csv(chinook.network.file) %>% filter(WIDE_WW > 0)

# run for chinook spawners
species = "chinook"   
model = "fuzzy"   
lifestage = "spawner"
network = chinook.network %>% filter(SPAWN == 1)
source(file.path(repo.dir, "scripts/upscale.response.R"))

# run for chinook juveniles
species = "chinook" 
model = "fuzzy"    
lifestage = "juvenile"
network = chinook.network %>% filter(REAR == 1)
source(file.path(repo.dir, "scripts/upscale.response.R"))

# read in steelhead nework and filter out rows with missing widths
steelhead.network = read_csv(steelhead.network.file) %>% filter(WIDE_WW > 0)

# run for steelhead spawners
species = "steelhead"   
model = "fuzzy"   
lifestage = "spawner"
network = steelhead.network %>% filter(SPAWN == 1)
source(file.path(repo.dir, "scripts/upscale.response.R"))

# run for steelhead juveniles
species = "steelhead" 
model = "nrei"    
lifestage = "juvenile"
network = steelhead.network %>% filter(REAR == 1)
source(file.path(repo.dir, "scripts/upscale.response.R"))


# This script Runs the upscale with user defined inputs
# Natalie Kramer (n.kramer.anderson@gmail.com)
# Last Updated JUne 4, 2019

# Documentation URL: https://github.com/natalie-kramer/GeomorphicUpscale/edit/master/docs/upscaling.md


# Set required paths ---------------------------

# User defined project directory path where outputs will be written
proj.dir = "C:/etal/LocalCode/GeomorphicUpscale/AsotinExample"

# Specify directory path to the downloaded Git Repo
repo.dir = "C:/etal/LocalCode/GeomorphicUpscale"

# Path to selections csv created by RSselections.R
selections.file = "C:/etal/LocalCode/GeomorphicUpscale/AsotinExample/Inputs/selections.csv"

# Path to network csv and braid index csv
network.file = "C:/etal/LocalCode/GeomorphicUpscale/AsotinExample/Inputs/network.csv"
braid.index.file = "C:/etal/LocalCode/GeomorphicUpscale/AsotinExample/Inputs/braid_index.csv"

# User defined variables ---------------------------

RSlevels = NA # optional vector argument to set RS factor order in graphs and displays, leave as  NA if alphabetical is desired
geoindicators = c('BfBraid', 'LWFreq_Wet', 'SlowWater_Freq', 'ChnlUnit_Freq', 'Sin') # list of geoindicators to create plots for


# Dependencies -------------------------------------------------------------------

library(tidyverse)

# Read back in selections data in case it was cleared
selections = read_csv(selections.file)

# Builds the project directory structure and re-organizes inputs ---------------------------

# source(file.path(repo.dir, "scripts/projbuild.R"))


# Generate selection output ---------------------------

# geoindicator summary script

source(file.path(repo.dir, "scripts/selection.geoindicators.R")) 

# turned this off for now - ask NK why she's calling this in both the RSelection and the UpscaleWrapper
# # soruce the code to copy and file the maps
# source(file.path(repo.dir, "scripts/selection.maps.R")) 


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
response.pool = "byRSCond"     # Options: "byRScond", "byRS", "byAll"
seg.id.col = "segmentID"        
length.col = "length.m"        
width.col = "bf.width.m"       
cond.cols = c("Condition0", "Condition1", "Condition2", "Condition3")   #user supplied
area.col = NA # leave as NA if no area is specified per reach segment and it will be estimated but this requires braid index file

network = read_csv(network.file)
if(!is.na(braid.index.file)){braid.index = read_csv(braid.index.file)}else{braid.index = NA}

# run for chinook spawners
species = "chinook"   
model = "fuzzy"   
lifestage = "spawner"
source(file.path(repo.dir, "scripts/upscale.response.R"))
source(file.path(repo.dir, "scripts/upscale.response.reach.R"))

# run for chinook juveniles
species = "chinook" 
model = "fuzzy"    
lifestage = "juvenile"
source(file.path(repo.dir, "scripts/upscale.response.R"))
source(file.path(repo.dir, "scripts/upscale.response.reach.R"))

# run for steelhead spawners
species = "steelhead"   
model = "fuzzy"   
lifestage = "spawner"
source(file.path(repo.dir, "scripts/upscale.response.R"))
source(file.path(repo.dir, "scripts/upscale.response.reach.R"))

# run for steelhead juveniles
species = "steelhead" 
model = "nrei"    
lifestage = "juvenile"
source(file.path(repo.dir, "scripts/upscale.response.R"))
source(file.path(repo.dir, "scripts/upscale.response.reach.R"))


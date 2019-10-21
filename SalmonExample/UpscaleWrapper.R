# This script Runs the upscale with user defined inputs
# Natalie Kramer (n.kramer.anderson@gmail.com)
# Last Updated JUne 4, 2019

# Documentation URL: https://github.com/natalie-kramer/GeomorphicUpscale/edit/master/docs/upscaling.md


# Set required paths ---------------------------

# User defined project directory path where outputs will be written
proj.dir = "C:/Anabranch/UpperSalmon/wrk_Data/GUTUpscale/PrelimRun_05"

# Specify directory path to the downloaded Git Repo
repo.dir = "C:/etal/LocalCode/GeomorphicUpscale"

# Path to selections csv created by RSselections.R
selections.file = "C:/Anabranch/UpperSalmon/wrk_Data/GUTUpscale/PrelimRun_05/Inputs/Selections.csv"

# Path to network csv and braid index csv
chinook.network.file = "C:/etal/LocalCode/GeomorphicUpscale/SalmonExample/Chinook_Network.csv"
steelhead.network.file = "C:/etal/LocalCode/GeomorphicUpscale/SalmonExample/Steelhead_Network.csv"
braid.index.file = NA

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
seg.id.col = "RECNO"        
length.col = "LENGTH"        
width.col = "WIDE_WW"       
cond.cols = c("COND_HIST", "COND_EX", "COND_RP")     #user supplied
area.col = "AREA_BF" # leave as NA if no area is specified per reach segment and it will be estimated but this requires braid index file

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


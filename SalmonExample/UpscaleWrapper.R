# This script Runs the upscale with user defined inputs
# Natalie Kramer (n.kramer.anderson@gmail.com)
# Last Updated JUne 4, 2019

# Documentation URL: https://github.com/natalie-kramer/GeomorphicUpscale/edit/master/docs/upscaling.md


# Set required paths ---------------------------

# User defined project directory path where outputs will be written
proj.dir = "C:/Anabranch/UpperSalmon/wrk_Data/GUTUpscale/PrelimRun_01"

# Specify directory path to the downloaded Git Repo
repo.dir = "C:/etal/LocalCode/GeomorphicUpscale"

# Path to selections csv created by RSselections.R
selections.file = "C:/Anabranch/UpperSalmon/wrk_Data/GUTUpscale/PrelimRun_01/Inputs/Selections.csv"

# Path to network csv and braid index csv
network.file = "C:/Anabranch/UpperSalmon/wrk_Data/GUTUpscale/PrelimRun_01/Inputs/Anadramous_Network.csv"
braid.index.file = NA

# User defined variables ---------------------------

gu.type = "UnitForm"      #Options: UnitForm, GU # UnitShape not available at this time since I don't have maps of these in the database
RSlevels = NA # optional vector argument to set RS factor order in graphs and displays, leave as  NA if alphabetical is desired
geoindicators = c('BfBraid', 'LWFreq_Wet', 'SlowWater_Freq', 'ChnlUnit_Freq', 'Sin') # list of geoindicators to create plots for


# Dependencies -------------------------------------------------------------------

library(tidyverse)

# Read back in selections data in case it was cleared
selections = read_csv(selections.file)

# Builds the project directory structure and re-organizes inputs ---------------------------

source(file.path(repo.dir, "scripts/projbuild.R"))


# Generate selection output ---------------------------

# geoindicator summary script

source(file.path(repo.dir, "scripts/selection.geoindicators.R")) 

# turned this off for now - ask NK why she's calling this in both the RSelection and the UpscaleWrapper
# # soruce the code to copy and file the maps
# source(file.path(repo.dir, "scripts/selection.maps.R")) 


# Generate assemblage output ---------------------------
# NK Note: plots are not printing????

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

response.pool = "byRSCond"     #Options: "byRScond", "byRS", "byAll"
seg.id.col = "RECNO"        
length.col = "LENGTH"        
width.col = "WIDE_WW"       
cond.cols = c("GEO_COND")     #user supplied
area.cols = "AREA_WW" # leave as NA if no area is specified per reach segment and it will be estimated

# read in network and braid.index files
network = read_csv(network.file)
if(!is.na(braid.index.file)){braid.index = read_csv(braid.index.file)}else{braid.index = NA}

# source and run code to generate output for upscaling response variabales
source(file.path(repo.dir, "scripts/upscale.response.R"))

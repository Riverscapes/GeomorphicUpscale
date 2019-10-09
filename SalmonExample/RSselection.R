#This script uses information within ReachSelectionCriteria.csv to select reaches within the GUT database
#that match geomorphically to specified geomorphic indicator characteristics. 

#Joe Wheaton (joe.wheaton@usu.edu)
#Last Updated Sep 26, 2019


# To do -------------------------------------------------------------------

#Update database_reachcharacteristics to include Karen's sites'


# Set required paths ---------------------------

# User defined project directory path
proj.dir = "C:/Anabranch/UpperSalmon/wrk_Data/GUTUpscale/PrelimRun_01"

# Specify directory path to the downloaded Git repository
repo.dir = "C:/etal/LocalCode/GeomorphicUpscale"

# Load required packages ---------------------------
library(tidyverse)
library(purrrlyr)

# read in data--------------------
data.in = read_csv(file.path(repo.dir, "Database/GUTUpscale_Database_ReachCharacteristics.csv"))
head(data.in) # print first few rows of look-up table

# Make selections for each river style and type --------------------

# Manually edit these to match criteria in your selection criteria look up table
# This is a template which provides you with maximum flexibility to select as stringently or as un stringent as you desire. 
# The trick is to not be too restrictive so that you end up with a large enough pool of sites to acquire a good empirical estimate, 
# while not so restrictive that you end up with sites that don't look like your defined River Style.

# Before makeing individual selections for diferent river styles, you can eliminate certain type streams from the entire pool of database streams
data = data.in

# todo - ask Joe if this should be turned back on
# sandystreams = data.in %>%
#   filter(Sndf > 50)
# sandystreams #this search will eliminate four reaches visits 820,1971,2288,3975
# 
# data = data.in %>%
#   filter(Sndf < 50)

# Example selection options.  You won't use all available selection options, just the geo indicators that 
# give you the best subset for your river style type. Comment out any that you don't have data for or don't want to use. This might be iterative 
# because you want enough data to give you an adequate subset, but not too general that the streams found aren't representative of your river style and condition.

# Updated Selection Variables & Options
	# Gradient is a real number > 0
	#& Confinement != "CV" or "PCV" or "UCV"
    #	& BfBraid == 1 or 1 > BfBraid <= 2 or 2 > BfBraid <= 3 or BfBraid > 3
    #	& Bedform == "Beaver-Dammed" or "Plane-Bed" or "Cascade" or "Pool-Riffle" or "Rapid" or "Step-Pool" 
    #	& Planform == "Anabranching" or "Meandering" or "Sinuous" or "Straight" or "Wandering"
	#	& Sinuosity (floating point between 1 and 10)
    #	& LWfreq is pieces per 100 m (floating point?)
    #	& DamCount is an integer between 0 and 1000

# EXAMPLE SPECS: See: https://docs.google.com/spreadsheets/d/1fQ1DjK9Y53Dgul3bWwHYyQpGKcCf9L18NtE8S2PCcgI/edit?usp=sharing
# 	Gradient > 0.001
# 	& Confinement == "CV" or "PCV" or "UCV" or != "CV"
#     & BfBraid == 1 or 1 > BfBraid <= 2 or 2 > BfBraid <= 3 or BfBraid > 3
#     & Bedform == "Beaver-Dammed" or "Plane-Bed" or "Cascade" or "Pool-Riffle" or "Rapid" or "Step-Pool" 
#     & Planform == "Anabranching" or "Meandering" or "Sinuous" or "Straight" or "Wandering"
# 	& Sinuosity < 1.2
#     & LWfreq > 25
#     & DamCount > 5

# Keep your River Style codes, short and easy to understand.  These will be carried through as the label identifier used for each River style
# if it has a condition variant add it after the code as "good" "moderate" or "poor" all in lower case.




# Updating to Chris's Six Reach Types (Wandering_Gravel, Planform_Controlled, Margin_Controlled, Conf_Floodplain, Conf_Bedrock, and AlluvialFan)


#--*----**------------------------------------
# Wandering Reach Types

# Wandering-poor (WA poor) -----------------------------------------------------

WApoor = data %>%
  filter(
	Confinement != "CV"
	& BfBraid == 1
	& LWFreq_Wet <= 5 
	& SlowWater_Freq <= 2.4
	& ChnlUnit_Freq <= 2.75) %>%
  mutate(RS = "WA", Condition = "poor")

WApoor
summary(WApoor)
nrow(WApoor)

# Wandering-moderate (WA moderate) -----------------------------------------------------

WAmoderate = data %>%
  filter(
    Confinement != "CV"
    & (BfBraid > 1 & BfBraid <= 2) 
    & LWFreq_Wet <= 20
    & (SlowWater_Freq > 0 & SlowWater_Freq <= 2.4)
    & (ChnlUnit_Freq > 2.75 & ChnlUnit_Freq <= 6)) %>%
  mutate(RS = "WA", Condition = "moderate")

WAmoderate
summary(WAmoderate)
nrow(WAmoderate)

# Wandering-good (WA good) -----------------------------------------------------

WAgood = data %>%
  filter(
    Confinement != "CV"
    & (BfBraid > 1 & BfBraid <= 2) 
    & LWFreq_Wet > 20
    & (SlowWater_Freq > 2.4)
    & ChnlUnit_Freq > 2.75) %>%
  mutate(RS = "WA", Condition = "good")

WAgood
summary(WAgood)
nrow(WAgood)

# Wandering-intact (WA intact) -----------------------------------------------------

WAintact = data %>%
  filter(
    Confinement != "CV"
    & (BfBraid > 2 & BfBraid <= 3) 
    & LWFreq_Wet > 20
    & (SlowWater_Freq > 2.4)
    & ChnlUnit_Freq > 2.75) %>%
  mutate(RS = "WA", Condition = "intact")

WAintact
summary(WAintact)
nrow(WAintact)

#--*----**------------------------------------
# Planform Controlled Reach Types

# Planform Controlled-poor (PC poor) -----------------------------------------------------
PCpoor = data %>%
  filter(
    Confinement == "PCV"
    & BfBraid == 1
    & (Sin >= 1 & Sin <= 1.1) 
    & LWFreq_Wet <= 5
    & SlowWater_Freq <= 2.4
    & ChnlUnit_Freq <= 2.75) %>%
  mutate(RS = "PC", Condition = "poor")

PCpoor
summary(PCpoor)
nrow(PCpoor)

# Planform Controlled-moderate (PC moderate) -----------------------------------------------------

PCmoderate = data %>%
  filter(
    Confinement == "PCV"
    & BfBraid == 1
    & (Sin > 1.1 & Sin <= 1.3) 
    & LWFreq_Wet <= 20
    & (SlowWater_Freq > 0 & SlowWater_Freq <= 2.4)
    & (ChnlUnit_Freq > 2.75 & ChnlUnit_Freq <= 6)) %>%
  mutate(RS = "PC", Condition = "moderate")

PCmoderate
summary(PCmoderate)
nrow(PCmoderate)

# Planform Controlled-good (PC good) -----------------------------------------------------

PCgood = data%>%
  filter(
    Confinement == "PCV"
    & BfBraid == 1
    & Sin > 1.3 
    & LWFreq_Wet <= 20
    & (SlowWater_Freq > 2.4)
    & ChnlUnit_Freq > 2.75) %>%
  mutate(RS = "PC", Condition = "good")

PCgood
summary(PCgood)
nrow(PCgood)

# Planform Controlled-intact (PC intact) -----------------------------------------------------

PCintact = data%>%
  filter(
    Confinement == "PCV"
    & (BfBraid > 1 & BfBraid < 2)
    & Sin > 1.3 
    & LWFreq_Wet > 20
    & SlowWater_Freq > 2.4
    & ChnlUnit_Freq > 2.75) %>%
  mutate(RS = "PC", Condition = "intact")

PCintact
summary(PCintact)
nrow(PCintact)

#--*----**------------------------------------
# NA - These are primiarily Step-Cascade and Conf_Bedrock. These seem to be ones that Chris missed (n=48 small reaches in Upper Salmon)

# NA-poor (NA poor) -----------------------------------------------------

NApoor = data %>%
  filter(
    Confinement == "CV"
    & BfBraid == 1 
    & LWFreq_Wet <= 5
    & SlowWater_Freq <= 2.4
    & ChnlUnit_Freq <= 2.75) %>%
  mutate(RS = "NA", Condition = "poor")

NApoor
summary(NApoor)
nrow(NApoor)

# NA-moderate (NA moderate)-----------------------------------------------------

NAmoderate = data %>%
  filter(
    Confinement == "CV"
    & BfBraid == 1 
    & LWFreq_Wet <= 5
    & (SlowWater_Freq > 0 & SlowWater_Freq <= 2.4)
    & (ChnlUnit_Freq > 2.75 & ChnlUnit_Freq <= 6)) %>%
  mutate(RS = "NA", Condition = "moderate")

NAmoderate
summary(NAmoderate)
nrow(NAmoderate)

# NA- Good(NA good) -----------------------------------------------------

NAgood = data %>%
  filter(
    Confinement == "CV"
    & BfBraid == 1 
    & LWFreq_Wet <= 20
    & (SlowWater_Freq > 2.4)
    & ChnlUnit_Freq > 2.75) %>%
  mutate(RS = "NA", Condition = "good")

NAgood
summary(NAgood)
nrow(NAgood)

# NA- Intact(NA intact) -----------------------------------------------------

NAintact = data %>%
  filter(
    Confinement == "CV"
    & BfBraid == 1 
    & LWFreq_Wet > 20
    & (SlowWater_Freq > 2.4)
    & ChnlUnit_Freq > 2.75) %>%
  mutate(RS = "NA", Condition = "intact")

NAintact
summary(NAintact)
nrow(NAintact)

#--*----**------------------------------------
# Margin Controlled Reach Types

# Margin Controlled-poor (MC poor) -----------------------------------------------------
MCpoor = data.in %>%
  filter(
    Confinement == "PCV"
    & BfBraid == 1
    & (Sin >= 1 & Sin <= 1.1) 
    & LWFreq_Wet <= 5
    & SlowWater_Freq <= 2.4
    & ChnlUnit_Freq <= 2.75) %>%
  mutate(RS = "MC", Condition = "poor")

MCpoor
summary(MCpoor)
nrow(MCpoor)

# Margin Controlled-moderate (MC moderate)-----------------------------------------------------

MCmoderate = data %>%
  filter(
    Confinement == "PCV"
    & BfBraid == 1
    & (Sin > 1.1 & Sin <= 1.3) 
    & LWFreq_Wet <= 20
    & (SlowWater_Freq > 0 & SlowWater_Freq <= 2.4)
    & (ChnlUnit_Freq > 2.75 & ChnlUnit_Freq <= 6)) %>%
  mutate(RS = "MC", Condition = "moderate")

MCmoderate
summary(MCmoderate)
nrow(MCmoderate)

# Margin Controlled-good (MC good) -----------------------------------------------------
MCgood = data %>%
  filter(
    Confinement == "PCV"
    & (BfBraid > 1 & BfBraid < 2)
    & Sin >= 1.3
    & LWFreq_Wet > 20
    & (SlowWater_Freq > 2.4)
    & (ChnlUnit_Freq > 2.75)) %>%
  mutate(RS = "MC", Condition = "good")

MCgood
summary(MCgood)
nrow(MCgood)

# Margin Controlled-intact (MC intact) -----------------------------------------------------
MCintact = data %>%
  filter(
    Confinement == "PCV"
    & (BfBraid > 1 & BfBraid < 2)
    & Sin >= 1.3
    & LWFreq_Wet > 20
    & (SlowWater_Freq > 2.4)
    & (ChnlUnit_Freq > 2.75)) %>%
  mutate(RS = "MC", Condition = "intact")

MCintact
summary(MCintact)
nrow(MCintact)

#--*----**------------------------------------
# Confined Floodplain Reach Types

# Confined Floodplain-poor (CF poor) -----------------------------------------------------
CFpoor = data.in %>%
  filter(
    Confinement == "CV"
    & BfBraid == 1 
    & LWFreq_Wet <= 5
    & SlowWater_Freq <= 2.4
    & (ChnlUnit_Freq <= 2.75)) %>%
  mutate(RS = "CF", Condition = "poor")

CFpoor
summary(CFpoor)
nrow(CFpoor)

# Confined Floodplain-moderate (CF moderate)-----------------------------------------------------

CFmoderate = data %>%
  filter(
    Confinement == "CV"
    & BfBraid == 1 
    & LWFreq_Wet <= 5
    & SlowWater_Freq <= 2.4
    & (ChnlUnit_Freq > 2.75 & ChnlUnit_Freq <= 6)) %>%
  mutate(RS = "CF", Condition = "moderate")

CFmoderate
summary(CFmoderate)
nrow(CFmoderate)

# Confined Floodplain-good (CF good) -----------------------------------------------------

CFgood = data %>%
  filter(
    Confinement == "CV"
    & BfBraid == 1 
    & LWFreq_Wet <= 20
    & (SlowWater_Freq >= 2.4)
    & ChnlUnit_Freq > 6) %>%
  mutate(RS = "CF", Condition = "good")

CFgood
summary(CFgood)
nrow(CFgood)

# Confined Floodplain-intact (CF intact) -----------------------------------------------------

CFintact = data %>%
  filter(
    Confinement == "CV"
    & (BfBraid > 1 & BfBraid < 2)  
    & LWFreq_Wet > 20
    & (SlowWater_Freq >= 2.4)
    & ChnlUnit_Freq > 6) %>%
  mutate(RS = "CF", Condition = "intact")

CFintact
summary(CFintact)
nrow(CFintact)

#--*----**------------------------------------
# Confined Bedrock Reach Types

# Confined Bedrock-poor (MC poor) -----------------------------------------------------
CBpoor = data.in %>%
  filter(
    Confinement == "CV"
    & BfBraid == 1 
    & LWFreq_Wet <= 5
    & SlowWater_Freq <= 2.4
    & ChnlUnit_Freq <= 2.75) %>%
  mutate(RS = "CB", Condition = "poor")

CBpoor
summary(CBpoor)
nrow(CBpoor)

# Confined Bedrock-moderate (CB moderate)-----------------------------------------------------

CBmoderate = data %>%
  filter(
    Confinement == "CV"
    & BfBraid == 1 
    & LWFreq_Wet <= 5
    & (SlowWater_Freq > 0 & SlowWater_Freq <= 2.4)
    & ChnlUnit_Freq <= 2.75) %>%
  mutate(RS = "CB", Condition = "moderate")

CBmoderate
summary(CBmoderate)
nrow(CBmoderate)

# Confined Bedrock-good (CB good) -----------------------------------------------------

CBgood = data %>%
  filter(
    Confinement == "CV"
    & BfBraid == 1 
    & LWFreq_Wet <= 20
    & (SlowWater_Freq > 0 & SlowWater_Freq <= 2.4)
    & (ChnlUnit_Freq > 2.75 & ChnlUnit_Freq <= 6)) %>%
  mutate(RS = "CB", Condition = "good")

CBgood
summary(CBgood)
nrow(CBgood)

# Confined Bedrock-intact (CB intact) -----------------------------------------------------

CBintact = data %>%
  filter(
    Confinement == "CV"
    & BfBraid == 1 
    & LWFreq_Wet > 20
    & (SlowWater_Freq > 2.4)
    & ChnlUnit_Freq >= 6) %>%
  mutate(RS = "CB", Condition = "intact")

CBintact
summary(CBintact)
nrow(CBintact)

#--*----**------------------------------------
# Alluvial Fan Reach Types

# Alluvial Fan-poor (AF poor) -----------------------------------------------------

AFpoor = data %>%
  filter(
    Confinement == "UCV"
    & BfBraid == 1 
    & LWFreq_Wet <= 5
    & SlowWater_Freq <= 2.4
    & ChnlUnit_Freq <= 2.75) %>%
  mutate(RS = "AF", Condition = "poor")

AFpoor
summary(AFpoor)
nrow(AFpoor)

# Alluvial Fan-moderate (AF moderate) -----------------------------------------------------

AFmoderate = data %>%
  filter(
    Confinement == "UCV"
    & (BfBraid > 1 & BfBraid < 2)
    & LWFreq_Wet <= 20
    & (SlowWater_Freq > 2.4)
    & (ChnlUnit_Freq > 2.75)) %>%
  mutate(RS = "AF", Condition = "moderate")

AFmoderate
summary(AFmoderate)
nrow(AFmoderate)

# Alluvial Fan-good (AF good) -----------------------------------------------------

AFgood = data %>%
  filter(
    Confinement == "UCV"
    & (BfBraid >= 2 & BfBraid < 3)
    & LWFreq_Wet <= 20
    & (SlowWater_Freq > 2.4)
    & ChnlUnit_Freq > 2.75) %>%
  mutate(RS = "AF", Condition = "good")

AFgood
summary(AFgood)
nrow(AFgood)

# Alluvial Fan-intact (AF intact) -----------------------------------------------------

AFintact = data %>%
  filter(
    Confinement == "UCV"
    & BfBraid >= 2
    & LWFreq_Wet > 20
    & SlowWater_Freq > 2.4
    & ChnlUnit_Freq > 2.75) %>%
  mutate(RS = "AF", Condition = "intact")

AFintact
summary(AFintact)
nrow(AFintact)


# Combine selections into single dataset and save output --------------------

# combine selections into single df
selections = rbind(AFpoor,AFmoderate, AFgood, AFintact,
                 CBpoor, CBmoderate, CBgood, CBintact,
                 CFpoor, CFmoderate, CFgood, CFintact,
                 MCpoor, MCmoderate, MCgood, MCintact,
                 NApoor, NAmoderate, NAgood, NAintact,
                 PCpoor,PCmoderate, PCgood, PCintact,
                 WApoor, WAmoderate, WAgood, WAintact) %>%
  mutate(RSCond = paste(RS, Condition, sep = ""))

# print sample size summary
selections %>% group_by(RS, Condition) %>% count()

# create inputs folder if doesn't already exist
input.dir = file.path(proj.dir, "Inputs")
if(!file.exists(input.dir)){dir.create(input.dir)}

selection.filename = "Selections.csv"
write_csv(selections, file.path(input.dir, selection.filename), col_names = TRUE) 


# Copy maps corresponding to selections for review --------------------

# specify output directory
selection.maps.dir = file.path(proj.dir, "Outputs/Selections/Maps")

# create output directory if it doesn't exist
if(!file.exists(selection.maps.dir)){dir.create(selection.maps.dir, recursive = TRUE)}

# delete any existing files in Output from previous runs
unlink(paste(selection.maps.dir, "\\*", sep = ""), recursive = T)

# specify location of maps from database
repo.maps.dir = file.path(repo.dir, "Database/Maps")  

#soruce the geomorphic MapsbyRSselection from where it is locally saved.
source(file.path(repo.dir, "scripts/selection.maps.R"))

selection.maps(id.name = "VisitID", pool.by = "RS")
selection.maps(id.name = "VisitID", pool.by = "RSCond")

       
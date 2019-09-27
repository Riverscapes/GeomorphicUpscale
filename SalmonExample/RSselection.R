#This script uses information within ReachSelectionCriteria.csv to select reaches within the GUT database
#that match geomorphically to specified geomorphic indicator characteristics. 

#Joe Wheaton (joe.wheaton@usu.edu)
#Last Updated Sep 26, 2019


# To do -------------------------------------------------------------------

#Update database_reachcharacteristics to include Karen's sites'


# Set required paths ---------------------------

# User defined project directory path
proj.dir = "C:/Anabranch/UpperSalmon/wrk_Data/temp/GeomorphicUpscale-master/AsotinExample"

# Specify directory path to the downloaded Git repository
repo.dir = "C:/Anabranch/UpperSalmon/wrk_Data/temp/GeomorphicUpscale-master"

# Load required packages ---------------------------
library(tidyverse)

# read in data--------------------
data.in = read_csv(file.path(repo.dir, "Database\\Database_ReachCharacteristics.csv"))
head(data.in) # print first few rows of look-up table

# Make selections for each river style and type --------------------

# Manually edit these to match criteria in your selection criteria look up table
# This is a template which provides you with maximum flexibility to select as stringently or as un stringent as you desire. 
# The trick is to not be too restrictive so that you end up with a large enough pool of sites to acquire a good empirical estimate, 
# while not so restrictive that you end up with sites that don't look like your defined River Style.

# Before makeing individual selections for diferent river styles, you can eliminate certain type streams from the entire pool of database streams

sandystreams = data.in %>%
  filter(Sndf > 50)
sandystreams #this search will eliminate four reaches visits 820,1971,2288,3975

data = data.in %>%
  filter(Sndf < 50)

# Example selection options.  You won't use all available selection options, just the geo indicators that 
# give you the best subset for your river style type. Comment out any that you don't have data for or don't want to use. This might be iterative 
# because you want enough data to give you an adequate subset, but not too general that the streams found aren't representative of your river style and condition.

# Updated Selection Variables & Options
	# Gradient is a real number > 0
	#& Confinement != "CV" or "PCV" or "UCV"
    #	& Threads == "Single" or "Multi" or "Transitional"
    #	& Bedform == "Beaver-Dammed" or "Plane-Bed" or "Cascade" or "Pool-Riffle" or "Rapid" or "Step-Pool" 
    #	& Planform == "Anabranching" or "Meandering" or "Sinuous" or "Straight" or "Wandering"
	#	& Sinuosity (floating point between 1 and 10)
    #	& LWfreq is pieces per 100 m (floating point?)
    #	& DamCount is an integer between 0 and 1000

# GARBAGE
	Gradient > 0.001
	& Confinement == "CV" or "PCV" or "UCV" or != "CV"
    & Threads == "Single" or "Multi" or "Transitional"
    & Bedform == "Beaver-Dammed" or "Plane-Bed" or "Cascade" or "Pool-Riffle" or "Rapid" or "Step-Pool" 
    & Planform == "Anabranching" or "Meandering" or "Sinuous" or "Straight" or "Wandering"
	& Sinuosity < 1.2
    & LWfreq > 25
    & DamCount > 5

# Keep your River Style codes, short and easy to understand.  These will be carried through as the label identifier used for each River style
# if it has a condition variant add it after the code as "good" "moderate" or "poor" all in lower case.


# Updating to Chris's Six Reach Types (Wandering_Gravel, Planform_Controlled, Margin_Controlled, Conf_Floodplain, Conf_Bedrock, and AlluvialFan)


#--*----**------------------------------------
#### Wandering Reach Types
### Variables that define Reach Type: 
### Variables that differentiate condition of this reach type:
# Wandering-poor (WA poor) -----------------------------------------------------

WApoor = data %>%
  filter(
	Confinement != "CV"
    & Threads == "Single"
    & Bedform == "Plane-Bed" 
    & (Planform == "Straight" | Planform == "Sinuous" )
	& Sinuosity < 1.2
    & (LWfreq < 30 | is.na(LWfreq))
	& (DamCount < 2 | is.na(DamCount))) %>%
  mutate(RS = "WA", Condition = "poor")


WApoor
summary(WApoor)
nrow(WApoor)


# Wandering-moderate (WA moderate) -----------------------------------------------------

WAmoderate = data %>%
  filter(
	Confinement != "CV" 
    & (Threads == "Single" | Threads == "Transitional")
    & (Bedform == "Pool-Riffle" | Bedform == "Plane-Bed") 
    & (Sinuosity > 1.2 & Sinuosity < 1.3) 
    & ((LWfreq < 60) | is.na(LWfreq))) %>%
  mutate(RS= "WA" , Condition = "moderate")

WAmoderate
summary(WAmoderate)
nrow(WAmoderate)

# Wandering-good (WA good) -----------------------------------------------------

WAgood = data %>%
  filter(
    Confinement != "CV" 
    & Threads != "Single" 
    & Bedform != "Plane-Bed"
    & (Sinuosity > 1.1 & Sinuosity < 1.5)
    & ((LWfreq > 20) | is.na(LWfreq))) %>%
  mutate(RS = "WA", Condition = "good")

WAgood
summary(WAgood)
nrow(WAgood)

# Wandering-intact (WA intact) -----------------------------------------------------

WAintact = data %>%
  filter(
    Confinement != "CV" 
    & Threads != "Single" 
    & Bedform != "Plane-Bed"
    & (Sinuosity > 1.1 & Sinuosity < 1.5)
    & ((LWfreq > 20) | is.na(LWfreq))) %>%
  mutate(RS = "WA", Condition = "intact")

WAintact
summary(WAintact)
nrow(WAintact)

#--*----**------------------------------------
#### Planform Controlled Reach Types
# Planform Controlled-poor (PC poor) -----------------------------------------------------
PCpoor = data %>%
  filter(
    Gradient < 2.5  
	& Confinement == "PCV"
    & Threads == "Single" 
    & Bedform == "Plane-Bed" 
    & Sinuosity < 1.1 
    & (LWfreq < 30 | is.na(LWfreq))) %>%
  mutate(RS = "PC", Condition = "poor")

PCpoor
summary(PCpoor)
nrow(PCpoor)


# Planform Controlled-moderate (PC moderate) -----------------------------------------------------

PCmoderate = data %>%
  filter(
    Gradient < 2.5 
	& Confinement == "PCV"
    & (Threads == "Single" | Threads == "Transitional")
    & (Bedform == "Plane-Bed"| Bedform == "Pool-Riffle")  
    & (Sinuosity > 1.1 & Sinuosity < 1.5) 
    & ((LWfreq > 10 & LWfreq < 60) | is.na(LWfreq))) %>%
  mutate(RS = "PC", Condition = "moderate")

PCmoderate
summary(PCmoderate)
nrow(PCmoderate)


# Planform Controlled-good (PC good) -----------------------------------------------------

PCgood = data%>%
  filter(
    Gradient < 2.5 
	& Confinement == "PCV"
    & (Threads == "Single" | Threads=="Transitional"| Threads=="Multi")
    & Bedform == "Pool-Riffle"
    & (Sinuosity < 1.5 & Sinuosity > 1.1) 
    &((LWfreq > 20) | is.na(LWfreq))) %>%
  mutate(RS = "PC", Condition = "good")

PCgood
summary(PCgood)
nrow(PCgood)

# Planform Controlled-intact (PC intact) -----------------------------------------------------

PCintact = data%>%
  filter(
    Gradient < 2.5 
	& Confinement == "PCV"
    & (Threads == "Single" | Threads=="Transitional"| Threads=="Multi")
    & Bedform == "Pool-Riffle"
    & (Sinuosity < 1.5 & Sinuosity > 1.1) 
    &((LWfreq > 20) | is.na(LWfreq))) %>%
  mutate(RS = "PC", Condition = "intact")

PCintact
summary(PCintact)
nrow(PCintact)

#--*----**------------------------------------
#### NA - These are primiarily Step-Cascade and Conf_Bedrock. These seem to be ones that Chris missed (n=48 small reaches in Upper Salmon)

# NA-poor (NA poor) -----------------------------------------------------

NApoor = data %>%
  filter(
    (Gradient > 2 & Gradient < 6)
    & Confinement == "CV" 
    & Threads == "Single" 
    & (Bedform == "Plane-Bed"| Bedform == "Rapid") 
    & Sinuosity < 1.1
    & ((LWfreq < 30) | is.na(LWfreq))) %>%
  mutate(RS = "NA", Condition = "poor")

NApoor
summary(NApoor)
nrow(NApoor)


# NA-moderate (NA moderate)-----------------------------------------------------

NAmoderate = data %>%
  filter(
    (Gradient > 2 & Gradient < 6)
    & Confinement == "CV" 
    & Threads == "Single" 
    & Bedform != "Plane-Bed" 
    & Sinuosity < 1.1 
    & ((LWfreq < 60) | is.na(LWfreq))) %>%
  mutate(RS = "NA", Condition = "moderate")

NAmoderate
summary(NAmoderate)
nrow(NAmoderate)


# NA- Good(NA good) -----------------------------------------------------

NAgood = data %>%
  filter(
    (Gradient > 2 & Gradient < 6)
    & Threads != "Multi" 
    & Bedform != "Plane-Bed" 
    & Sinuosity < 1.2 
    & Confinement == "CV" 
    & ((LWfreq > 10) | is.na(LWfreq))) %>%
  mutate(RS = "NA", Condition = "good")

NAgood
summary(NAgood)
nrow(NAgood)

# NA- Intact(NA intact) -----------------------------------------------------

NAintact = data %>%
  filter(
    (Gradient > 2 & Gradient < 6)
    & Threads != "Multi" 
    & Bedform != "Plane-Bed" 
    & Sinuosity < 1.2 
    & Confinement == "CV" 
    & ((LWfreq > 10) | is.na(LWfreq))) %>%
  mutate(RS = "NA", Condition = "intact")

NAintact
summary(NAintact)
nrow(NAintact)

#--*----**------------------------------------
#### Margin Controlled Reach Types
# Margin Controlled-poor (MC poor) -----------------------------------------------------
MCpoor = data.in %>%
  filter(((Gradient < 3.5 & Gradient >= 1))
		 & Confinement == "PCV" 
         & Threads == "Single" 
         & Bedform == "Plane-Bed"
         & Sinuosity < 1.1 
         & (LWfreq < 30 | is.na(LWfreq)))%>%
  mutate(RS = "MC", Condition = "poor")

MCpoor
summary(MCpoor)
nrow(MCpoor)



# Margin Controlled-moderate (MC moderate)-----------------------------------------------------

MCmoderate = data %>%
  filter(((Gradient < 3.5 & Gradient>=1))
		 & Confinement == "PCV" 
         & Threads == "Single" 
         & (Bedform == "Plane-Bed"| Bedform == "Pool-Riffle")  
         & (Sinuosity < 1.3 & Sinuosity > 1.04) 
         & (LWfreq < 60  | is.na(LWfreq)))%>%
  mutate(RS = "MC", Condition = "moderate")

MCmoderate
summary(MCmoderate)
nrow(MCmoderate)

# Margin Controlled-good (MC good) -----------------------------------------------------
MCgood = data %>%
  filter(((Gradient < 3.5 & Gradient >= 1))
		 & Confinement == "PCV" 
         & Threads != "Multi" 
         & Bedform != "Plane-Bed"
         & (Sinuosity < 1.5 & Sinuosity > 1.1) 
         & (LWfreq > 10 | is.na(LWfreq)))%>%
  mutate(RS = "MC", Condition = "good")

MCgood
summary(MCgood)
nrow(MCgood)

# Margin Controlled-intact (MC intact) -----------------------------------------------------
MCintact = data %>%
  filter(((Gradient < 3.5 & Gradient >= 1))
		 & Confinement == "PCV" 
         & Threads != "Multi" 
         & Bedform != "Plane-Bed"
         & (Sinuosity < 1.5 & Sinuosity > 1.1) 
         & (LWfreq > 10 | is.na(LWfreq)))%>%
  mutate(RS = "MC", Condition = "intact")

MCintact
summary(MCintact)
nrow(MCintact)

#--*----**------------------------------------
#### Confined Floodplain Reach Types
# Confined Floodplain-poor (MC poor) -----------------------------------------------------
CFpoor = data.in %>%
  filter(((Gradient < 3.5 & Gradient >= 1))
		 & Confinement == "CV" 
         & Threads == "Single" 
         & Bedform == "Plane-Bed"
         & Sinuosity < 1.1 
         & (LWfreq < 30 | is.na(LWfreq)))%>%
  mutate(RS = "CF", Condition = "poor")

CFpoor
summary(CFpoor)
nrow(CFpoor)



# Confined Floodplain-moderate (CF moderate)-----------------------------------------------------

CFmoderate = data %>%
  filter(((Gradient < 3.5 & Gradient>=1))
		 & Confinement == "CV" 
         & Threads == "Single" 
         & (Bedform == "Plane-Bed"| Bedform == "Pool-Riffle")  
         & (Sinuosity < 1.3 & Sinuosity > 1.04) 
         & (LWfreq < 60  | is.na(LWfreq)))%>%
  mutate(RS = "CF", Condition = "moderate")

CFmoderate
summary(CFmoderate)
nrow(CFmoderate)

# Confined Floodplain-good (CF good) -----------------------------------------------------
CFgood = data %>%
  filter(((Gradient < 3.5 & Gradient >= 1))
		 & Confinement == "CV" 
         & Threads != "Multi" 
         & Bedform != "Plane-Bed"
         & (Sinuosity < 1.5 & Sinuosity > 1.1) 
         & (LWfreq > 10 | is.na(LWfreq)))%>%
  mutate(RS = "CF", Condition = "good")

CFgood
summary(CFgood)
nrow(CFgood)

# Confined Floodplain-intact (CF intact) -----------------------------------------------------
CFintact = data %>%
  filter(((Gradient < 3.5 & Gradient >= 1))
		 & Confinement == "CV" 
         & Threads != "Multi" 
         & Bedform != "Plane-Bed"
         & (Sinuosity < 1.5 & Sinuosity > 1.1) 
         & (LWfreq > 10 | is.na(LWfreq)))%>%
  mutate(RS = "CF", Condition = "intact")

CFintact
summary(CFintact)
nrow(CFintact)

#--*----**------------------------------------
#### Confined Bedrock Reach Types
# Confined Bedrock-poor (MC poor) -----------------------------------------------------
CBpoor = data.in %>%
  filter(((Gradient < 3.5 & Gradient >= 1))
		 & Confinement == "CV" 
         & Threads == "Single" 
         & Bedform == "Plane-Bed"
         & Sinuosity < 1.1 
         & (LWfreq < 30 | is.na(LWfreq)))%>%
  mutate(RS = "CB", Condition = "poor")

CBpoor
summary(CBpoor)
nrow(CBpoor)



# Confined Bedrock-moderate (CB moderate)-----------------------------------------------------

CBmoderate = data %>%
  filter(((Gradient < 3.5 & Gradient>=1))
		 & Confinement == "CV" 
         & Threads == "Single" 
         & (Bedform == "Plane-Bed"| Bedform == "Pool-Riffle")  
         & (Sinuosity < 1.3 & Sinuosity > 1.04) 
         & (LWfreq < 60  | is.na(LWfreq)))%>%
  mutate(RS = "CB", Condition = "moderate")

CBmoderate
summary(CBmoderate)
nrow(CBmoderate)

# Confined Bedrock-good (CB good) -----------------------------------------------------
CBgood = data %>%
  filter(((Gradient < 3.5 & Gradient >= 1))
		 & Confinement == "CV" 
         & Threads != "Multi" 
         & Bedform != "Plane-Bed"
         & (Sinuosity < 1.5 & Sinuosity > 1.1) 
         & (LWfreq > 10 | is.na(LWfreq)))%>%
  mutate(RS = "CB", Condition = "good")

CBgood
summary(CBgood)
nrow(CBgood)

# Confined Bedrock-intact (CB intact) -----------------------------------------------------
CBintact = data %>%
  filter(((Gradient < 3.5 & Gradient >= 1))
		 & Confinement == "CV" 
         & Threads != "Multi" 
         & Bedform != "Plane-Bed"
         & (Sinuosity < 1.5 & Sinuosity > 1.1) 
         & (LWfreq > 10 | is.na(LWfreq)))%>%
  mutate(RS = "CB", Condition = "intact")

CBintact
summary(CBintact)
nrow(CBintact)

#--*----**------------------------------------
#### Alluvial Fan Reach Types
# Alluvial Fan-poor (AF poor) -----------------------------------------------------

AFpoor = data %>%
  filter(
    Gradient < 3  
    & Confinement == "UCV"  
    & Threads == "Single" 
    & Bedform == "Plane-Bed" 
    & Sinuosity < 1.2
    & (LWfreq < 30 | is.na(LWfreq))) %>%
  mutate(RS = "AF", Condition = "poor")

AFpoor
summary(AFpoor)
nrow(AFpoor)


# Alluvial Fan-moderate (AF moderate) -----------------------------------------------------

AFmoderate = data %>%
  filter(
    Gradient < 3 
    & Confinement == "UCV"  
    & (Threads == "Single" | Threads == "Transitional")
    & Bedform == "Plane-Bed" 
    & Sinuosity < 1.3
    & ((LWfreq < 60 & LWfreq > 10)| is.na(LWfreq))) %>%
  mutate(RS = "AF", Condition = "moderate")

AFmoderate
summary(AFmoderate)
nrow(AFmoderate)


# Alluvial Fan-good (AF good) -----------------------------------------------------

AFgood = data %>%
  filter(
    Gradient < 3 
    & (Threads=="Single" | Threads=="Transitional" | Threads=="Multi")
    # & (Braid < 5)
    & (Bedform=="Plane-Bed" | Bedform=="Pool-Riffle")
    & (Sinuosity<1.5 & Sinuosity >1.1)
    & Confinement == "UCV"  
    & (LWfreq > 20| is.na(LWfreq))) %>%
  mutate(RS = "AF", Condition = "good")

AFgood
summary(AFgood)
nrow(AFgood)

# Alluvial Fan-intact (AF intact) -----------------------------------------------------
AFintact = data %>%
  filter(((Gradient < 3.5 & Gradient >= 1))
    & Confinement == "UCV"  
         & Threads != "Multi" 
         & Bedform != "Plane-Bed"
         & (Sinuosity < 1.5 & Sinuosity > 1.1) 
         & (LWfreq > 10 | is.na(LWfreq)))%>%
  mutate(RS = "AF", Condition = "intact")

AFintact
summary(AFintact)
nrow(AFintact)





# Combine selections into single dataset and save output --------------------

# combine selections into single df
selections = rbind(FCpoor,FCmoderate, FCgood, 
                 AFpoor,AFmoderate, AFgood,
                 PCpoor,PCmoderate, PCgood, 
                 WApoor, WAmoderate, WAgood,
                 CVpoor, CVmoderate, CVgood) %>%
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

selection.maps(id.name = "visit.id", pool.by = "RS")
selection.maps(id.name = "visit.id", pool.by = "RSCond")

       
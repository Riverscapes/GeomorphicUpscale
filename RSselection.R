#This script uses information within ReachSelectionCriteria.csv to select reaches within the GUT database
#that match geomorphically to specified geomorphic indicator characteristics. 

#Natalie Kramer (n.kramer.anderson@gmail.com)
#Last Updated Aug 14, 2019


# To do -------------------------------------------------------------------

#Update database_reachcharacteristics to include Karen's sites'


# Set required paths ---------------------------

# User defined project directory path (where outputs will be created)
proj.dir = "C:/etal/LocalCode/GeomorphicUpscale/AsotinExample"

# Specify directory path to the downloaded Git repository
repo.dir = "C:/etal/LocalCode/GeomorphicUpscale"

# Load required packages ---------------------------
library(tidyverse)

# read in data--------------------
data.in = read_csv(file.path(repo.dir, "Database\\GUTUpscale_Database_ReachCharacteristics.csv"))
head(data.in) # print first few rows of look-up table

# Make selections for each river style and type --------------------

# Manually edit these to match criteria in your selection criteria look up table
# This is a template which provides you with maximum flexibility to select as stringently or as un stringent as you desire. 
# The trick is to not be too restrictive so that you end up with a large enough pool of sites to acquire a good empirical estimate, 
# while not so restrictive that you end up with sites that don't look like your defined River Style.

# Before makeing individual selections for diferent river styles, you can eliminate certain type streams from the entire pool of database streams

sandystreams = data.in %>%
  filter(SubEstSandFines > 50)
sandystreams #this search will eliminate four reaches visits 820,1971,2288,3975

data = data.in %>%
  filter(SubEstSandFines < 50)

# Example selection options.  You won't use all available selection options, just the geo indicators that 
# give you the best subset for your river style type. Comment out any that you don't have data for or don't want to use. This might be iterative 
# because you want enough data to give you an adequate subset, but not too general that the streams found aren't representative of your river style and condition.

# Keep your River Style codes, short and easy to understand.  These will be carried through as the label identifier used for each River style
# if it has a condition varient add it after the code as "good" "moderate" or "poor" all in lower case.


# Fan Controlled-poor (FC poor) -----------------------------------------------------
FCpoor = data.in %>%
  filter(((Grad < 3.5 & Grad >= 1))
         & Confinement != "UCV" 
         & Threads == "Single" 
         & Bedform == "Plane-Bed"
         & Sin < 1.1 
         & (LWFreq_Wet < 30 | is.na(LWFreq_Wet)))%>%
  mutate(RS = "FC", Condition = "poor")

FCpoor
summary(FCpoor)
nrow(FCpoor)


# Fan Controlled-moderate (FC moderate)-----------------------------------------------------

FCmoderate = data %>%
  filter(((Grad < 3.5 & Grad>=1))
         & Confinement != "UCV" 
         & Threads == "Single" 
         & (Bedform == "Plane-Bed"| Bedform == "Pool-Riffle")  
         & (Sin < 1.3 & Sin > 1.04) 
         & (LWFreq_Wet < 60  | is.na(LWFreq_Wet)))%>%
  mutate(RS = "FC", Condition = "moderate")

FCmoderate
summary(FCmoderate)
nrow(FCmoderate)

# Fan Controlled-good (FC good) -----------------------------------------------------
FCgood = data %>%
  filter(((Grad < 3.5 & Grad >= 1))
         & Confinement != "UCV" 
         & Threads != "Multi" 
         & Bedform != "Plane-Bed"
         & (Sin < 1.5 & Sin > 1.1) 
         & (LWFreq_Wet > 10 | is.na(LWFreq_Wet)))%>%
  mutate(RS = "FC", Condition = "good")

FCgood
summary(FCgood)
nrow(FCgood)


# Alluvial Fan-poor (AF poor) -----------------------------------------------------

AFpoor = data %>%
  filter(
    Grad < 3  
    & Confinement != "CV"  
    & Threads == "Single" 
    & Bedform == "Plane-Bed" 
    & Sin < 1.2
    & (LWFreq_Wet < 30 | is.na(LWFreq_Wet))) %>%
  mutate(RS = "AF", Condition = "poor")

AFpoor
summary(AFpoor)
nrow(AFpoor)


# Alluvial Fan-moderate (AF moderate) -----------------------------------------------------

AFmoderate = data %>%
  filter(
    Grad < 3 
    & Confinement != "CV"  
    & (Threads == "Single" | Threads == "Transitional")
    & Bedform == "Plane-Bed" 
    & Sin < 1.3
    & ((LWFreq_Wet < 60 & LWFreq_Wet > 10)| is.na(LWFreq_Wet))) %>%
  mutate(RS = "AF", Condition = "moderate")

AFmoderate
summary(AFmoderate)
nrow(AFmoderate)


# Alluvial Fan-good (AF good) -----------------------------------------------------

AFgood = data %>%
  filter(
    Grad < 3 
    & (Threads=="Single" | Threads=="Transitional" | Threads=="Multi")
    # & (Braid < 5)
    & (Bedform=="Plane-Bed" | Bedform=="Pool-Riffle")
    & (Sin<1.5 & Sin >1.1)
    & Confinement!="CV" 
    & (LWFreq_Wet > 20| is.na(LWFreq_Wet))) %>%
  mutate(RS = "AF", Condition = "good")

AFgood
summary(AFgood)
nrow(AFgood)


# Planform Controlled-poor (PC poor) -----------------------------------------------------
PCpoor = data %>%
  filter(
    Grad < 2.5  
    & Confinement != "CV"
    & Threads == "Single" 
    & Bedform == "Plane-Bed" 
    & Sin < 1.1 
    & (LWFreq_Wet < 30 | is.na(LWFreq_Wet))) %>%
  mutate(RS = "PC", Condition = "poor")

PCpoor
summary(PCpoor)
nrow(PCpoor)


# Planform Controlled-moderate (PC moderate) -----------------------------------------------------

PCmoderate = data %>%
  filter(
    Grad < 2.5 
    & Confinement != "CV" 
    & (Threads == "Single" | Threads == "Transitional")
    & (Bedform == "Plane-Bed"| Bedform == "Pool-Riffle")  
    & (Sin > 1.1 & Sin < 1.5) 
    & ((LWFreq_Wet > 10 & LWFreq_Wet < 60) | is.na(LWFreq_Wet))) %>%
  mutate(RS = "PC", Condition = "moderate")

PCmoderate
summary(PCmoderate)
nrow(PCmoderate)


# Planform Controlled-good (PC good) -----------------------------------------------------

PCgood = data%>%
  filter(
    Grad < 2.5 
    & Confinement != "CV"
    & (Threads == "Single" | Threads=="Transitional"| Threads=="Multi")
    & Bedform == "Pool-Riffle"
    & (Sin < 1.5 & Sin > 1.1) 
    &((LWFreq_Wet > 20) | is.na(LWFreq_Wet))) %>%
  mutate(RS = "PC", Condition = "good")

PCgood
summary(PCgood)
nrow(PCgood)


# Wandering-poor (WA poor) -----------------------------------------------------

WApoor = data %>%
  filter(
    Grad < 2  
    & Confinement != "CV"
    & Threads == "Single"
    & Bedform == "Plane-Bed" 
    & Sin < 1.2
    & (LWFreq_Wet < 30 | is.na(LWFreq_Wet))) %>%
  mutate(RS = "WA", Condition = "poor")

WApoor
summary(WApoor)
nrow(WApoor)


# Wandering-moderate (WA moderate) -----------------------------------------------------

WAmoderate = data %>%
  filter(
    Grad < 2 
    & Confinement != "CV" 
    & (Threads == "Single" | Threads == "Transitional")
    & (Bedform == "Pool-Riffle" | Bedform == "Plane-Bed") 
    & (Sin > 1.2 & Sin < 1.3) 
    & ((LWFreq_Wet < 60) | is.na(LWFreq_Wet))) %>%
  mutate(RS= "WA" , Condition = "moderate")

WAmoderate
summary(WAmoderate)
nrow(WAmoderate)


# Wandering-good (WA good) -----------------------------------------------------

WAgood = data %>%
  filter(
    Grad < 2 
    & Confinement != "CV" 
    & Threads != "Single" 
    & Bedform != "Plane-Bed"
    & (Sin > 1.1 & Sin < 1.5)
    & ((LWFreq_Wet > 20) | is.na(LWFreq_Wet))) %>%
  mutate(RS = "WA", Condition = "good")

WAgood
summary(WAgood)
nrow(WAgood)


# Confined Valley-poor (CV poor) -----------------------------------------------------

CVpoor = data %>%
  filter(
    (Grad > 2 & Grad < 6)
    & Confinement == "CV" 
    & Threads == "Single" 
    & (Bedform == "Plane-Bed"| Bedform == "Rapid") 
    & Sin < 1.1
    & ((LWFreq_Wet < 30) | is.na(LWFreq_Wet))) %>%
  mutate(RS = "CV", Condition = "poor")

CVpoor
summary(CVpoor)
nrow(CVpoor)


# Confined Valley-moderate (CV moderate)-----------------------------------------------------

CVmoderate = data %>%
  filter(
    (Grad > 2 & Grad < 6)
    & Confinement == "CV" 
    & Threads == "Single" 
    & Bedform != "Plane-Bed" 
    & Sin < 1.1 
    & ((LWFreq_Wet < 60) | is.na(LWFreq_Wet))) %>%
  mutate(RS = "CV", Condition = "moderate")

CVmoderate
summary(CVmoderate)
nrow(CVmoderate)


# Confined Valley-(CV good) -----------------------------------------------------

CVgood = data %>%
  filter(
    (Grad > 2 & Grad < 6)
    & Threads != "Multi" 
    & Bedform != "Plane-Bed" 
    & Sin < 1.2 
    & Confinement == "CV" 
    & ((LWFreq_Wet > 10) | is.na(LWFreq_Wet))) %>%
  mutate(RS = "CV", Condition = "good")

CVgood
summary(CVgood)
nrow(CVgood)


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
unlink(paste(selection.maps.dir, "\\*", sep = ""), recursive = TRUE)

# specify location of maps from database
repo.maps.dir = file.path(repo.dir, "Database/Maps")  

#soruce the geomorphic MapsbyRSselection from where it is locally saved.
source(file.path(repo.dir, "scripts/selection.maps.R"))


selection.maps(id.name = "visit.id", pool.by = "RSCond")

       
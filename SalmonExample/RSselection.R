#This script uses information within ReachSelectionCriteria.csv to select reaches within the GUT database
#that match geomorphically to specified geomorphic indicator characteristics. 


# Set required paths --------------------------------------------------------

# User defined project directory path
proj.dir = "C:/Anabranch/UpperSalmon/wrk_Data/GUTUpscale/FinalRun"

# Specify directory path to the downloaded Git repository
repo.dir = "C:/etal/LocalCode/GeomorphicUpscale"

# Load required packages -----------------------------------------------------

library(tidyverse)
library(purrrlyr)

# Read in training site data -------------------------------------------------

data = read_csv(file.path(repo.dir, "TrainingData/GUTUpscale_ReachCharacteristics.csv"))


# Make selections for each river style and type --------------------------------

#--*----**----------------------------------------------------------------------
# Wandering Reach Types (WA)

# Wandering-poor ------ 

WApoor = data %>%
  filter(
	Confinement != "CV"
	& (Grad > 1 & Grad < 2)
	& BfBraid == 1
	& Sin <= 1.1
	& Bedform == 'Plane-Bed'
	& LWFreq_Wet <= 5
  ) %>%
  mutate(RS = "WA", Condition = "poor")

View(WApoor)

# Wandering-moderate ------ 

WAmoderate = data %>%
  filter(
    Confinement != "CV"
    & Grad < 2
    & (BfBraid > 1 & BfBraid < 2)
    & Sin > 1.1
    & Bedform == "Pool-Riffle"
    & LWFreq_Wet <= 5
  ) %>%
  mutate(RS = "WA", Condition = "moderate")

View(WAmoderate)

# Wandering-good ------ 

WAgood = data %>%
  filter(
    Confinement != "CV"
    & Grad < 2
    & (BfBraid > 1 & BfBraid < 2)
    & Sin > 1.1
    & Bedform == 'Pool-Riffle'
    & LWFreq_Wet > 5
  ) %>%
  mutate(RS = "WA", Condition = "good")

View(WAgood)

# Wandering-intact ------ 

WAintact = data %>%
  filter(
    Confinement != "CV"
    & Grad < 2
    & BfBraid >= 2
  ) %>%
  mutate(RS = "WA", Condition = "intact")

View(WAintact)


#--*----**----------------------------------------------------------------------
# Planform Controlled Reach Types (PC)

# Planform Controlled-poor ------

PCpoor = data %>%
  filter(
    Confinement != "CV"
    & (Grad > 1 & Grad < 2)
    & BfBraid == 1
    & Sin <= 1.1
    & Bedform == 'Plane-Bed'
    & LWFreq_Wet <= 5 
    ) %>%
  mutate(RS = "PC", Condition = "poor")

View(PCpoor)

# Planform Controlled-moderate ------

PCmoderate = data %>%
  filter(
    Confinement != "CV"
    & (Grad > 1 & Grad < 2)
    & (BfBraid > 1 & BfBraid < 2)
    & (Sin > 1.1 & Sin <= 1.5)
    & Bedform == "Plane-Bed"  
    & LWFreq_Wet > 5
  ) %>%
  mutate(RS = "PC", Condition = "moderate")

View(PCmoderate)

# Planform Controlled-good ------

PCgood = data %>%
  filter(
    Confinement != "CV"
    & Grad < 1
    & (BfBraid > 1 & BfBraid < 2)
    & (Sin > 1.1 & Sin <= 1.5)
    & Bedform == "Pool-Riffle"  
    & LWFreq_Wet > 5 
  ) %>%
  mutate(RS = "PC", Condition = "good")

View(PCgood)

# Planform Controlled-intact ------

PCintact = data%>%
  filter(
    Confinement != "CV"
    & Grad < 1
    & BfBraid > 1
    & Sin > 1.1
    & Bedform == "Pool-Riffle"  
    & LWFreq_Wet > 20 
  ) %>%
  mutate(RS = "PC", Condition = "intact")

View(PCintact)


#--*----**----------------------------------------------------------------------
# Margin Controlled Reach Types (MC)

# Margin Controlled-poor ------ 

MCpoor = data %>%
  filter(
    Confinement == "PCV"
    & Grad > 1
    & BfBraid == 1
    & Sin <= 1.1
    & Bedform == 'Plane-Bed'
    & LWFreq_Wet <= 5 
  ) %>%
  mutate(RS = "MC", Condition = "poor")

View(MCpoor)

# Margin Controlled-moderate ------ 

MCmoderate = data %>%
  filter(
    Confinement == "PCV"
    & (Grad > 1 & Grad < 2)
    & (BfBraid > 1 & BfBraid < 2)
    & (Sin > 1.1 & Sin <= 1.5)
    & Bedform == "Plane-Bed"  
    & LWFreq_Wet > 5
  ) %>%
  mutate(RS = "MC", Condition = "moderate")

View(MCmoderate)

# Margin Controlled-good ------ 

MCgood = data %>%
  filter(
    Confinement == "PCV"
    & Grad < 1
    & (BfBraid > 1 & BfBraid < 2)
    & (Sin > 1.1 & Sin <= 1.5)
    & Bedform == "Pool-Riffle"  
    & LWFreq_Wet > 5 
  ) %>%
  mutate(RS = "MC", Condition = "good")

View(MCgood)

# Margin Controlled-intact ------ 

MCintact = data %>%
  filter(
    Confinement == "PCV"
    & Grad < 1
    & BfBraid > 1
    & Sin > 1.1
    & Bedform == "Pool-Riffle"  
    & LWFreq_Wet > 20 
  ) %>%
  mutate(RS = "MC", Condition = "intact")

View(MCintact)


#--*----**----------------------------------------------------------------------
# Confined Cascade Reach Types (CC)

# Confined Cascade-good ------ 

CCgood = data %>%
  filter(
    Confinement == "CV"
    & Grad > 2
    & BfBraid >= 1
    & Sin <= 1.2
    & Bedform != "Plane-Bed" 
    & LWFreq_Wet < 5
  ) %>%
  mutate(RS = "CC", Condition = "good")

View(CCgood)

# Confined Cascade-intact ------ 

CCintact = data %>%
  filter(
    Confinement == "CV"
    & Grad > 2
    & BfBraid >= 1
    & Sin <= 1.2
    & Bedform != "Plane-Bed" 
    & LWFreq_Wet >= 5
  ) %>%
  mutate(RS = "CC", Condition = "intact")

View(CCintact)


#--*----**----------------------------------------------------------------------
# Confined Floodplain Reach Types (CF)

# poor condition ------ 

CFpoor = data %>%
  filter(
    Confinement == "CV"
    & (Grad > 2 & Grad < 8)
    & BfBraid == 1 
    & Sin <= 1.05
    & (Bedform == "Plane-Bed" | Bedform == "Rapid" | Bedform == "Cascade")
    & LWFreq_Wet <= 5
  ) %>%
  mutate(RS = "CF", Condition = "poor")

View(CFpoor)

# moderate  condition ------ 

CFmoderate = data %>%
  filter(
    Confinement == "CV"
    & (Grad > 2 & Grad < 8)
    & BfBraid >= 1 
    & Sin > 1.05
    & Bedform != "Plane-Bed"
    & LWFreq_Wet <= 5
  ) %>%
  mutate(RS = "CF", Condition = "moderate")

View(CFmoderate)

# good condition ------ 

CFgood = data %>%
  filter(
    Confinement == "CV"
    & (Grad > 2 & Grad < 4)
    & BfBraid >= 1
    & Sin >= 1.05
    & Bedform != "Plane-Bed" 
    & LWFreq_Wet > 5
  ) %>%
  mutate(RS = "CF", Condition = "good")

View(CFgood)

# intact condition ------ 

CFintact = data %>%
  filter(
    Confinement == "CV"
    & Grad < 2
    & BfBraid >= 1
    & Sin >= 1.05
    & Bedform != "Plane-Bed" 
    & LWFreq_Wet > 20
  ) %>%
  mutate(RS = "CF", Condition = "intact")

View(CFintact)


#--*----**----------------------------------------------------------------------
# Confined Bedrock Reach Types (CB)

# poor condition ------ 

CBpoor = data %>%
  filter(
    Confinement == "CV"
    & Grad > 4
    & BfBraid == 1
    & Sin < 1.1
    & (Bedform == "Plane-Bed" | Bedform == "Rapid" | Bedform == "Cascade")
    & LWFreq_Wet < 5
  ) %>%
  mutate(RS = "CB", Condition = "poor")

View(CBpoor)

# moderate condition ------ 

CBmoderate = data %>%
  filter(
    Confinement == "CV"
    & (Grad > 3)
    & BfBraid == 1 
    & Sin >= 1.05
    & Bedform != "Plane-Bed"
    & LWFreq_Wet >= 5
  ) %>%
  mutate(RS = "CB", Condition = "moderate")

View(CBmoderate)

# good condition ------ 

CBgood = data %>%
  filter(
    Confinement == "CV"
    & (Grad > 2 & Grad < 3)
    & BfBraid >= 1 
    & Sin >= 1.05
    & (Bedform == "Step-Pool" | Bedform == "Pool-Riffle") 
    & LWFreq_Wet >= 5
  ) %>%
  mutate(RS = "CB", Condition = "good")

View(CBgood)

# intact condition ------ 

CBintact = data %>%
  filter(
    Confinement == "CV"
    & Grad < 2
    & BfBraid >= 1
    & Sin >= 1.05
    & (Bedform == "Step-Pool" | Bedform == "Pool-Riffle") 
    & LWFreq_Wet > 20
  ) %>%
  mutate(RS = "CB", Condition = "intact")

View(CBintact)


#--*----**----------------------------------------------------------------------
# Alluvial Fan Reach Types (AF)

# poor condition ------ 

AFpoor = data %>%
  filter(
    Confinement == "UCV"
    & Grad > 1
    & BfBraid == 1
    & Bedform == "Plane-Bed"
  ) %>%
  mutate(RS = "AF", Condition = "poor")

View(AFpoor)

# moderate condition ------ 

AFmoderate = data %>%
  filter(
    Confinement == "UCV"
    & Grad > 0.3
    & BfBraid >= 1
    & Sin <= 1.3
    & Bedform != "Plane-Bed" 
    & LWFreq_Wet > 5
  ) %>%
  mutate(RS = "AF", Condition = "moderate")

View(AFmoderate)

# good condition ------ 

AFgood = data %>%
  filter(
    Confinement == "UCV"
    & Grad > 0.3
    & BfBraid > 1
    & Sin > 1.3
    & Bedform == "Pool-Riffle"
    & LWFreq_Wet > 5
  ) %>%
  mutate(RS = "AF", Condition = "good")

View(AFgood)

# intact condition ------ 

AFintact = data %>%
  filter(
    Confinement == "UCV"
    & Grad <= 0.3
    & BfBraid >= 1
    & Sin > 1.3
    & Bedform == "Pool-Riffle"
    & LWFreq_Wet > 5
  ) %>%
  mutate(RS = "AF", Condition = "intact")

View(AFintact)


# Combine selections into single dataset and save output --------------------

# combine selections into single df
selections = rbind(AFpoor,AFmoderate, AFgood, AFintact,
                   MCpoor, MCmoderate, MCgood, MCintact,
                   PCpoor,PCmoderate, PCgood, PCintact,
                   WApoor, WAmoderate, WAgood, WAintact,
                   CCgood, CCintact,
                   CBpoor, CBmoderate, CBgood, CBintact,
                   CFpoor, CFmoderate, CFgood, CFintact) %>%
  mutate(RSCond = paste(RS, Condition, sep = ""))

# print sample size summary
selections %>% group_by(RS, Condition) %>% count()

# create inputs folder if doesn't already exist
input.dir = file.path(proj.dir, "Inputs")
if(!file.exists(input.dir)){dir.create(input.dir)}

# save selections csv to inputs folder
selection.filename = "Selections.csv"
write_csv(selections, file.path(input.dir, selection.filename), col_names = TRUE) 


# Copy geomorphic unit maps corresponding to selections --------------------

# specify output directory
selection.maps.dir = file.path(proj.dir, "Outputs/Selections/Maps")

# create output directory if it doesn't exist
if(!file.exists(selection.maps.dir)){dir.create(selection.maps.dir, recursive = TRUE)}

# delete any existing files in Output from previous runs
unlink(file.path(selection.maps.dir, "*"), recursive = TRUE)

# specify location of training data maps
repo.maps.dir = file.path(repo.dir, "TrainingData/Maps")  

# call function to copy maps 
source(file.path(repo.dir, "scripts/selection.maps.R"))
selection.maps(id.name = "VisitID", pool.by = "RS")
selection.maps(id.name = "VisitID", pool.by = "RSCond")

       
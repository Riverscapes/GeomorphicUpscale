---
title: Selection and Review
weight: 30
---

## Overview

Geomorphic indicators from the [training dataset](https://github.com/Riverscapes/GeomorphicUpscale/blob/master/TrainingData/GUTUpscale_ReachCharacteristics.csv) are used to represent analogue reaches of River Styles (Brierly and Fryiers, 2005) that you define for your network of interest. For the Asotin example, we used the criterion in this [River Styles geoindicator table](https://github.com/Riverscapes/GeomorphicUpscale/tree/master/docs/assets/images/AsotinGeoindicators.png) to help us make our selections.

## Step 1. Define river styles on your network

Categorize your network into similar reach types or process domains and characterize bounds of indicator variables for each reach type. The geoindicators that are available to use can be found in the [training dataset](https://github.com/Riverscapes/GeomorphicUpscale/blob/master/TrainingData/GUTUpscale_ReachCharacteristics.csv).  Most of these geoindicators (or metrics) were derived by the Columbia Habitat Monitoring Program [(CHaMP)](https://www.champmonitoring.org/).  Metric definitions can be found on the [CHaMP Metrics Wiki](https://github.com/SouthForkResearch/CHaMP_Metrics/wiki).  Several non-Champ categorical variables that we found useful for describing predominant reach character were also populated for each reach in the training dataset (see descriptions below). 

You use these indicators to help you select a subset of the stream reaches  in the training dataset that are similar in character to each of your defined reach types.  You do not need to utilize all of the geoindicator variables, just the ones that you think are most important for your reach types. We highly recommend that at the minimum you include gradient, bedform, and either the braid or thread variables. 

Although we used the same set of geoindicators for each river style and condition in the Asotin basin, you don't have to.  For example you could choose to use a D50 geoindicator for only one type of river style. It is important to recognize that you could have a River Style that is wandering, but the actual reach planform is something else (e.g., straight).  This is because River Styles designation takes a slightly broader view of the landscape around the reach, characterizing what the character of the planform should be if it was in good condition.  A disconnect between the River Style name and the geoindicator planforms (or other characteristic) most often occur in reaches that are in poor or moderate condition. The key here is to pick geoindicators that differentiate river styles and condition variants.

### Additional Categorical Variables
- **Planform** Sinuous; Straight; Meandering; Wandering; Anabranching
- **Bedform** Plane Bed; Pool-Riffle; Step-Pool; Rapid; Cascade
- **Threads** Single; Transitional; Multi
- **Substrate** boulder-cobble-gravel-sand (Different combinations  of all four variables are possible)
- **Confinement** CV; PC; UCV (confined valley, partly confined valley, uconfined valley) 

## Step 2. Select analogue reaches from the database

Once you have your indicators defined for your reach types and conditions, you are ready to use the indicator variables to help select empirical subsets.  This is accomplished using a series of logical statements that will subselect reaches from the training dataset that meet your criteria.  The trick is not to be too restrictive - so that you end up with a large enough pool of sites to acquire a good empirical estimate; but restrictive enough - so that you don't end up with sites that are not good analogues.  We provide [*RSselection.R*](https://github.com/Riverscapes/GeomorphicUpscale/blob/master/RSselection.R) as a starting point template script to help with the selections. This script will authomatically save a *Selections.csv* file to the Inputs folder within your project directory for use in the upscale.

An alternate way to select reaches is to simply hand select reaches from the training dataset after reviewing their characteristics and/or the pyGUT ouput maps.  All Tier2 and Tier3 GUT maps of the reaches in the training dataset can be accessed by downloading the [*Maps*](https://github.com/Riverscapes/GeomorphicUpscale/tree/master/TrainingData/Maps)  housed in the [TrainingData](https://github.com/Riverscapes/GeomorphicUpscale/tree/master/TrainingData) folder.  If you choose to do hand selection you will want to make sure that your final selection table is a *.csv* file that relates visits in the training dataset to each reach type and condition that you have mapped for your network.  At the minimum you need three columns: 'VisitID', 'RS', and 'Condition. *VisitID* specifies the visit of the analogue reach in the empirical subset, *RS* specifies River Style category in a shorthand code, and *Condition* specifies the condition varient of the River Style. If you aren't differentiating condition, set this to 'NA'.  You can use the  example data [*selections.csv*](https://github.com/Riverscapes/GeomorphicUpscale/blob/master/AsotinExample/Inputs/Selections.csv), as a template.  In this example table, we carry over many database metrics other than 'VisitID', 'RS', and 'Condition', but these extra variables are not necessary for moving forward. You should save your manually created *Selections.csv* file in an *Inputs* folder within your project directory.

## Step 3. Review and refine your selections

Once you have your lists of selections for each of your reach types and conditions, it is useful to view the pyGUT output maps of to make sure that the reaches chosen are similar in character to your reaches.  To ease the review process, the script  [*selection.maps.R*](https://github.com/Riverscapes/GeomorphicUpscale/blob/master/scripts/selection.maps.R) will copy maps from training data folders to separate folders in your local directory that correspond to the reach type categories in your *.csv* file from Step 2.  The [*RSselection.R*](https://github.com/Riverscapes/GeomorphicUpscale/blob/master/RSselection.R) will do this for you.

You will want to filter out any sites within each grouping that don't fit before moving on to estimating geomorphic assemblages. If the selections just seem off as a whole or you would like greater numbers, you will want to go back and revise your initial selection. If the streams from the training dataset just don't seem like good analogues, then be aware that the geomorphic assemblage estimates may not reflect reality for your situation. You may consider just upscaling the reaches where the selections decently mimic the geomorphic character of your reaches. When you are happy with your *selection.csv* file you are ready to move on!

## What's next?

Once you are happy with your selections and they are saved in your local project directory within an Inputs folder you can begin running the upscale script.

Go to the [next step]({{ site.baseurl }}/upscale)


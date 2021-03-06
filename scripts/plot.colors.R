#' Plot colors and factor order
#' 
#' @description Plotting color and factor level order for geomorphic unit type and condition variables


# Tier 2 unit shape ---------------------------

shape.fill = c('Concavity' = '#004DA8', 
              'Planar' = '#E6E600', 
              'Convexity' = '#A80000')

shape.levels=c("Concavity", "Planar", "Convexity")

# Tier 2 unit form ---------------------------

form.fill = c('Bowl' = '#004DA8', 
              'Bowl Transition' = '#00A9E6', 
              'Trough' = '#73DFFF', 
              'Plane' = '#E6E600', 
              'Mound Transition' = '#FF7F7F', 
              'Saddle' = '#E69800', 
              'Mound' = '#A80000', 
              'Wall' = '#000000')

form.levels = c('Bowl', 'Bowl Transition', 'Trough', 'Plane', 
                'Mound Transition', 'Saddle', 'Mound', 'Wall')

# Tier 2 geomorphic unit type ---------------------------

gu.fill = c('Bank' = '#000000', 
            'Barface' = "#730000",
            'Pool' = '#004DA8', 
            'Pond' = '#0070FF', 
            'Pocket Pool' = '#73B2FF', 
            'Chute' = '#73FFDF', 
            'Rapid' = '#66CDAB', 
            'Cascade' = '#448970',
            'Glide-Run' = '#E6E600', 
            'Riffle' = '#E69800',
            'Step' = '#D7B09E', 
            'Mid Channel Bar' = '#895A44', 
            'Margin Attached Bar' = '#A80000',
            'Transition' = '#CCCCCC')

gu.levels = c("Pocket Pool","Pool", "Pond", 
         "Margin Attached Bar","Mid Channel Bar","Barface",  "Riffle",
         "Cascade", "Rapid", "Chute" ,
         "Glide-Run", "Transition", "Bank")

# Reach type condition variants ---------------------------

condition.fill = c('poor' = '#ff0000', 
                   'moderate' = '#ffff00', 
                   'good' = '#38a800', 
                   'intact' = '#005ce6')

condition.levels = c('poor', 'moderate', 'good', 'intact')

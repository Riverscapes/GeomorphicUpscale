# Specify directory path to the downloaded Git repository
repo.dir = "C:/etal/LocalCode/GeomorphicUpscale"

# Specify output folder
out.dir = "C:/Anabranch/UpperSalmon/wrk_Data/GUTUpscale"


# Load required packages and read in data---------------------------
library(tidyverse)
library(GGally)

# read in reach characteristics csv
data.in = read_csv(file.path(repo.dir, "Database/GUTUpscale_Database_ReachCharacteristics.csv"))
site.fish = read_csv(file.path(repo.dir, "Database/Metrics/Site_Fish_Metrics_byVisit.csv"))
gut.units = read_csv(file.path(repo.dir, "Database/Metrics/Unit_GUTMetrics_Tier3_InChannel_GU.csv"))
unit.fish = read_csv(file.path(repo.dir, "Database/Metrics/Unit_Fish_Metrics_Tier3_InChannel_GU_All.csv"))

# subset and reformat data for plotting-------------------------

# subset site fish data
site.fish.sub = site.fish %>%
  select(visit.id, nrei.pred.fish.reach.steelhead.juvenile, fuzzy.pred.fish.reach.chinook.juvenile,
         fuzzy.pred.fish.reach.steelhead.spawner, fuzzy.pred.fish.reach.chinook.spawner) %>%
  rename(VisitID = visit.id, 
         Sthd_Juv = nrei.pred.fish.reach.steelhead.juvenile, 
         Chnk_Juv = fuzzy.pred.fish.reach.chinook.juvenile,
         Sthd_Redds = fuzzy.pred.fish.reach.steelhead.spawner, 
         Chnk_Redds = fuzzy.pred.fish.reach.chinook.spawner)

# subset gut data
gut.units.sub = gut.units %>%
  select(visit.id, unit.type, area.ratio) %>%
  filter(!is.na(unit.type)) %>%
  spread(unit.type, area.ratio) %>%
  replace(is.na(.), 0) %>%
  rename(VisitID = visit.id,
         Gl.Ru = `Glide-Run`,
         Br.Ma = `Margin Attached Bar`,
         Br.Mc = `Mid Channel Bar`,
         Po.Po = `Pocket Pool`,
         Tr = Transition) %>%
  dplyr::select(-Bank, -Barface)

# list of geoindicators used for USAL selection
indicator.list = c('Sthd_Juv', 'Chnk_Juv', 'Sthd_Redds', 'Chnk_Redds', 
                   'BfBraid', 'Sin', 'Grad', 'WidthRatio_WetBF', 
                   'LWFreq_Wet', 'SlowWater_Freq', 'ChnlUnit_Freq', 
                   'SlowWater_Ratio', 'FstNT_Ratio', 'FstTurb_Ratio',
                   'SubEstBldr', 'SubEstCbl', 'SubEstGrvl', 'SubEstSandFines',
                   'Gl.Ru', 'Br.Ma', 'Br.Mc', 'Po.Po', 'Tr',
                   'Cascade', 'Chute', 'Pond', 'Pool', 'Rapid')

# filter data to geoindicators used for selection and convert to long format
indicator.data = data.in %>%
  mutate(SlowWater_Ratio = SlowWater_Area / Area_Wet,
         FstNT_Ratio = FstNT_Area / Area_Wet,
         FstTurb_Ratio = FstTurb_Area / Area_Wet,
         WidthRatio_WetBF = WetWdth_Avg / BfWdth_Avg) %>%
  left_join(site.fish.sub, by = 'VisitID') %>%
  left_join(gut.units.sub, by = 'VisitID') %>%
  gather(key = "variable", value = "value", -SiteName, -Watershed, -VisitID, -Confinement) %>%
  filter(variable %in% indicator.list) %>%
  mutate(variable = factor(variable, levels = indicator.list),
         value = as.numeric(value))

# create confinement classes for 'pooled' (i.e., all) data and !CV
conf.not.cv = indicator.data %>% 
  filter(Confinement != 'CV') %>%
  mutate(Confinement = '!CV')

# create confinement classes for 'pooled' (i.e., all) data and !CV
conf.pooled = indicator.data %>%
  mutate(Confinement = 'Pooled')

# merge new classes with indicator data
indicator.data = indicator.data %>%
  bind_rows(list(conf.not.cv, conf.pooled))

# set confinement factor level order for plotting
indicator.data = indicator.data %>%
  mutate(Confinement = factor(Confinement, levels = c('Pooled', '!CV', 'CV', 'PCV', 'UCV')))

# boxplots and summary for selection geoindicators grouped by confinement category-------------------------

box.plots = ggplot(indicator.data, aes(as.factor(variable), as.numeric(value), fill = Confinement)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free")

indicator.summary = indicator.data  %>% 
  mutate(value = as.numeric(value)) %>%
  group_by(Confinement, variable) %>%
  summarize(min = min(value, na.rm = TRUE),
            q25 = quantile(value, probs = 0.25, na.rm = TRUE),
            med = median(value, na.rm = TRUE),
            q75 = quantile(value, probs = 0.75, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            na.count = sum(is.na(value)),
            lower.whisker = q25 - 1.5 * (q75 - q25),
            upper.whisker = q75 + 1.5 * (q75 - q25)
  ) %>%
  mutate(lower.whisker = ifelse(lower.whisker < min, min, lower.whisker),
         upper.whisker = ifelse(upper.whisker > max, max, upper.whisker)) %>%
  select(variable, Confinement, min, lower.whisker, q25, med, q75, upper.whisker, max, mean, sd, na.count)

write_csv(indicator.summary, file.path(out.dir, 'Geoindicator_SummaryStatistics.csv'), col_names = TRUE)
ggsave(file.path(out.dir, 'Geoindicator_Boxplots.png'), plot = box.plots, width = 10, height = 7)



# ggpairs correlation matrices -------------------------

# convert to long format

indicator.data.w = indicator.data %>% spread(variable, value) %>%
  select(SiteName, Watershed, VisitID, Confinement, 
         Sthd_Juv, Chnk_Juv, Sthd_Redds, Chnk_Redds,
         BfBraid, Grad, Sin, WidthRatio_WetBF,
         LWFreq_Wet, SlowWater_Freq, ChnlUnit_Freq, 
         SlowWater_Ratio, FstNT_Ratio, FstTurb_Ratio,
         SubEstBldr, SubEstCbl, SubEstGrvl, SubEstSandFines,
         Gl.Ru, Br.Ma, Br.Mc, Po.Po, Tr,
         Cascade, Chute, Pond, Pool, Rapid)

# add bedform and planform
bp.data = data.in %>% dplyr::select(VisitID, Bedform, Planform)

# indicator.data.w = indicator.data.w %>% left_join(bp.data, by = 'VisitID')

p.pool = indicator.data.w %>% filter(Confinement == 'Pooled') %>%
  ggpairs(columns = 5:ncol(.), title = 'Pooled (All Reaches)') +
     theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
ggsave(filename = file.path(out.dir, 'Geoindicator_CorrelationPlots_Pooled.png'), plot = p.pool, width = 30, height = 30)

p.notcv = indicator.data.w %>% filter(Confinement != 'CV') %>%
  ggpairs(columns = 5:ncol(.), title = 'Not Confined Reaches (!CV)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = file.path(out.dir, 'Geoindicator_CorrelationPlots_!CV.png'), plot = p.notcv, width = 30, height = 30)

p.cv = indicator.data.w %>% filter(Confinement == 'CV') %>%
  ggpairs(columns = 5:ncol(.), title = 'Confined Reaches (CV)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = file.path(out.dir, 'Geoindicator_CorrelationPlots_CV.png'), plot = p.cv, width = 30, height = 30)

p.pcv = indicator.data.w %>% filter(Confinement == 'PCV') %>%
  ggpairs(columns = 5:ncol(.), title = 'Partly Confined Reaches (PCV)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = file.path(out.dir, 'Geoindicator_CorrelationPlots_PCV.png'), plot = p.cv, width = 30, height = 30)

p.ucv = indicator.data.w %>% filter(Confinement == 'UCV') %>%
  ggpairs(columns = 5:ncol(.), title = 'Un-Confined Reaches (UCV)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = file.path(out.dir, 'Geoindicator_CorrelationPlots_UCV.png'), plot = p.ucv, width = 30, height = 30)


# plot confinement and bedform type
bedform.df = data.in %>% select(VisitID, Bedform)

bedform.plot = indicator.data %>% left_join(bedform.df, by = 'VisitID') %>%
  ggplot(aes(x = Confinement, fill = Bedform)) +
    geom_bar(alpha = 0.8) +
    coord_flip() +
    theme(legend.position = 'bottom') 

ggsave(bedform.plot, filename = file.path(out.dir, 'Confinement_Bedform_Plot.png'), width = 8, height = 6)



# unit fish density by confinement type -------------------------

unit.fish.df = unit.fish %>%
  rename(VisitID = visit.id) %>%
  filter(!is.na(species)) %>%
  mutate(pred.m2 = round(pred.fish / area.unit, 3)) %>%
  select(VisitID, UnitID, GU, species, lifestage, pred.fish, pred.m2) %>%
  left_join(data.in, by = 'VisitID')

# create confinement classes for 'pooled' (i.e., all) data and !CV
conf.not.cv = unit.fish.df %>% 
  filter(Confinement != 'CV') %>%
  mutate(Confinement = '!CV')

# create confinement classes for 'pooled' (i.e., all) data and !CV
conf.pooled = unit.fish.df %>%
  mutate(Confinement = 'Pooled')

unit.fish.df = unit.fish.df %>% bind_rows(conf.not.cv, conf.pooled)

ch.sp.bp.dens = ggplot(unit.fish.df %>% filter(species == 'chinook', lifestage == 'spawner', pred.m2 > 0), aes(as.factor(GU), as.numeric(pred.m2))) +
  geom_boxplot() +
  facet_wrap(~ Confinement, scales = "free") +
  coord_flip(ylim = c(0, 1)) +
  labs(title = "Chinook Spawner Redd Density by GU Type and Confinement",
       subtitle = "Note: 0 values removed for plotting")

ch.sp.bp.n = ggplot(unit.fish.df %>% filter(species == 'chinook', lifestage == 'spawner', pred.fish > 0), aes(as.factor(GU), as.numeric(pred.fish))) +
  geom_boxplot() +
  facet_wrap(~ Confinement, scales = "free") +
  coord_flip(ylim = c(0, 20)) +
  labs(title = "Chinook Spawner Redd Count by GU Type and Confinement",
       subtitle = "Note: 0 values removed for plotting")

ggsave(filename = file.path(out.dir, 'PredictedFish_Plots_ChinookSpawner_Density.png'), plot = ch.sp.bp.dens, width = 12, height = 12)
ggsave(filename = file.path(out.dir, 'PredictedFish_Plots_ChinookSpawner_Count.png'), plot = ch.sp.bp.n, width = 12, height = 12)


ch.j.bp.dens = ggplot(unit.fish.df %>% filter(species == 'chinook', lifestage == 'juvenile'), aes(as.factor(GU), as.numeric(pred.m2))) +
  geom_boxplot() +
  facet_wrap(~ Confinement, scales = "free") +
  coord_flip(ylim = c(0, 2)) +
  labs(title = "Chinook Juvenile Density by GU Type and Confinement",
       subtitle = "Note: Upper axis limit set to 2 for plot")

ch.j.bp.n = ggplot(unit.fish.df %>% filter(species == 'chinook', lifestage == 'juvenile'), aes(as.factor(GU), as.numeric(pred.fish))) +
  geom_boxplot() +
  facet_wrap(~ Confinement) +
  coord_flip(ylim = c(0, 100)) +
  labs(title = "Chinook Juvenile Count by GU Type and Confinement",
       subtitle = "Note: Upper axis limit set to 100 for plot")

ggsave(filename = file.path(out.dir, 'PredictedFish_Plots_ChinookJuvenile_Density.png'), plot = ch.j.bp.dens, width = 12, height = 12)
ggsave(filename = file.path(out.dir, 'PredictedFish_Plots_ChinookJuvenile_Count.png'), plot = ch.j.bp.n, width = 12, height = 12)


# # scatterplot of all geoindicator data-------------------------
# 
# # create fields for BfBraid and Sinuosity categories using Joe's breaks
# data = data.in %>%
#   select(-SiteName, -Watershed, -VisitID, -UTMEasting, -UTMNorthing, -UTMZone, -Latitude, -Longitude) %>%
#   mutate(Sin.Cat = ifelse(Sin <= 1.1, 'Straight', ifelse(Sin > 1.3, 'Sinuous/Meandering', 'Low Sinuosity')),
#          BfBraid.Cat = ifelse(BfBraid <= 1, 'Non-Braided', ifelse(BfBraid >= 2, 'Braided', 'Wandering')),
#          Sin.Cat = factor(Sin.Cat, levels = c('Straight', 'Low Sinuosity', 'Sinuous/Meandering')),
#          BfBraid.Cat = factor(BfBraid.Cat, levels = c('Non-Braided', 'Wandering', 'Braided')),
#          LWFreq_Wet = cut(LWFreq_Wet, breaks = c(-Inf, 0, 5, 20, Inf)))
#          # LWFreq_Wet = cut(LWFreq_Wet, breaks = c(0, 1.6, 6.2, 20)))
# 
# # set universal axis limites
# xmin = min(data$SlowWater_Freq, na.rm = TRUE)
# xmax = max(data$SlowWater_Freq, na.rm = TRUE)
# ymin = min(data$ChnlUnit_Freq, na.rm = TRUE)
# ymax = max(data$ChnlUnit_Freq, na.rm = TRUE)
# 
# 
# p.cv = ggplot(filter(data, Confinement == 'CV'), aes(x = SlowWater_Freq, y = ChnlUnit_Freq)) +
#   geom_hline(aes(yintercept = 2.75), linetype = 4, color = 'orange') +
#   geom_hline(aes(yintercept = 6), linetype = 4, color = 'blue') +
#   geom_vline(aes(xintercept = 2.4), linetype = 4, color = 'orange') +
#   geom_vline(aes(xintercept = 4), linetype = 4, color = 'blue') +
#   geom_point(aes(shape = Sin.Cat, colour = LWFreq_Wet), alpha = 0.7, size = 4) +
#   scale_colour_brewer(palette = "Set1") +
#   facet_wrap(~ BfBraid.Cat, drop = FALSE, nrow = 2) +
#   xlim(xmin, xmax) + ylim(ymin, ymax) +
#   labs(title = 'Confined Valley (CV) Geoindicators') + 
#   theme(legend.position="bottom")
# 
#   # + theme_light()
# 
# p.pcv = ggplot(filter(data, Confinement == 'PCV'), aes(x = SlowWater_Freq, y = ChnlUnit_Freq)) +
#   geom_hline(aes(yintercept = 2.75), linetype = 4, color = 'orange') +
#   geom_hline(aes(yintercept = 6), linetype = 4, color = 'blue') +
#   geom_vline(aes(xintercept = 2.4), linetype = 4, color = 'orange') +
#   geom_vline(aes(xintercept = 4), linetype = 4, color = 'blue') +
#   geom_point(aes(shape = Sin.Cat, colour = LWFreq_Wet), alpha = 0.7, size = 4) +
#   scale_colour_brewer(palette = "Set1") +
#   facet_wrap(~ BfBraid.Cat, drop = FALSE, nrow = 2) +
#   xlim(xmin, xmax) + ylim(ymin, ymax) +
#   labs(title = 'Partly Confined Valley (PCV) Geoindicators') + 
#   theme(legend.position="bottom")
# 
# p.ucv = ggplot(filter(data, Confinement == 'UCV'), aes(x = SlowWater_Freq, y = ChnlUnit_Freq)) +
#   geom_hline(aes(yintercept = 2.75), linetype = 4, color = 'orange') +
#   geom_hline(aes(yintercept = 6), linetype = 4, color = 'blue') +
#   geom_vline(aes(xintercept = 2.4), linetype = 4, color = 'orange') +
#   geom_vline(aes(xintercept = 4), linetype = 4, color = 'blue') +
#   geom_point(aes(shape = Sin.Cat, colour = LWFreq_Wet), alpha = 0.7, size = 4) +
#   scale_colour_brewer(palette = "Set1") +
#   facet_wrap(~ BfBraid.Cat, drop = FALSE, nrow = 2) +
#   xlim(xmin, xmax) + ylim(ymin, ymax) +
#   labs(title = 'Unconfined Valley (UCV) Geoindicators') + 
#   theme(legend.position="bottom")
# 
# p.not.cv = ggplot(filter(data, Confinement != 'CV'), aes(x = SlowWater_Freq, y = ChnlUnit_Freq)) +
#   geom_hline(aes(yintercept = 2.75), linetype = 4, color = 'orange') +
#   geom_hline(aes(yintercept = 6), linetype = 4, color = 'blue') +
#   geom_vline(aes(xintercept = 2.4), linetype = 4, color = 'orange') +
#   geom_vline(aes(xintercept = 4), linetype = 4, color = 'blue') +
#   geom_point(aes(shape = Sin.Cat, colour = LWFreq_Wet), alpha = 0.7, size = 4) +
#   scale_colour_brewer(palette = "Set1") +
#   facet_wrap(~ BfBraid.Cat, drop = FALSE, nrow = 2) +
#   xlim(xmin, xmax) + ylim(ymin, ymax) +
#   labs(title = 'NOT Confined Valley (CV) Geoindicators') + 
#   theme(legend.position="bottom")
# 
# ggsave(file.path(out.dir, 'Geoindicator_CV.png'), p.cv, width = 10, height = 7)
# ggsave(file.path(out.dir, 'Geoindicator_PCV.png'), p.pcv, width = 10, height = 7)
# ggsave(file.path(out.dir, 'Geoindicator_UCV.png'), p.ucv, width = 10, height = 7)
# ggsave(file.path(out.dir, 'Geoindicator_!CV.png'), p.not.cv, width = 10, height = 7)

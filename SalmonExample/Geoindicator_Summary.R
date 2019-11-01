# Specify directory path to the downloaded Git repository
repo.dir = "C:/etal/LocalCode/GeomorphicUpscale"

# Specify output folder
out.dir = "C:/Anabranch/UpperSalmon/wrk_Data/GUTUpscale"


# Load required packages and read in data---------------------------
library(tidyverse)
library(GGally)

# read in reach characteristics csv
data.in = read_csv(file.path(repo.dir, "TrainingData/GUTUpscale_ReachCharacteristics.csv"))
site.fish = read_csv(file.path(repo.dir, "TrainingData/Metrics/Site_Fish_Metrics_byVisit.csv"))
gut.units = read_csv(file.path(repo.dir, "TrainingData/Metrics/Unit_GUTMetrics_Tier3_InChannel_GU.csv"))
unit.fish = read_csv(file.path(repo.dir, "TrainingData/Metrics/Unit_Fish_Metrics_Tier3_InChannel_GU_All.csv"))

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

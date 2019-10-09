# Specify directory path to the downloaded Git repository
repo.dir = "C:/etal/LocalCode/GeomorphicUpscale"

# Specify output folder
out.dir = "C:/Anabranch/UpperSalmon/wrk_Data/GUTUpscale"


# Load required packages and read in data---------------------------
library(tidyverse)
library(GGally)

# read in reach characteristics csv
data.in = read_csv(file.path(repo.dir, "Database/GUTUpscale_Database_ReachCharacteristics.csv"))


# subset and reformat data for plotting-------------------------

# list of geoindicators used for USAL selection
inidcator.list = c('BfBraid', 'Sin', 'LWFreq_Wet', 'SlowWater_Freq', 'ChnlUnit_Freq')

# filter data to geoindicators used for selection and convert to long format
indicator.data = data.in %>%
  gather(key = "variable", value = "value", -SiteName, -Watershed, -VisitID, -Confinement) %>%
  filter(variable %in% inidcator.list) %>%
  mutate(value = as.numeric(value))

# create confinement classes for 'pooled' (i.e., all) data and !CV
conf.not.cv = indicator.data %>% 
  filter(Confinement != 'CV') %>%
  mutate(Confinement = '!CV')

conf.pooled = indicator.data %>%
  mutate(Confinement = 'Pooled')

# merge new classes with indicator data
indicator.data = indicator.data %>%
  bind_rows(list(conf.not.cv, conf.pooled))

# set confinement factor level order for plotting
indicator.data = indicator.data %>%
  mutate(Confinement = factor(Confinement, levels = c('Pooled', '!CV', 'CV', 'PCV', 'UCV')))

# boxplots and summary for selection geoindicators grouped by confinement category-------------------------

box.plots = ggplot(indicator.data, aes(as.factor(variable), value, fill = Confinement)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free")

indicator.summary = indicator.data  %>%
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

# scatterplot of all geoindicator data-------------------------

# create fields for BfBraid and Sinuosity categories using Joe's breaks
data = data.in %>%
  select(-SiteName, -Watershed, -VisitID, -UTMEasting, -UTMNorthing, -UTMZone, -Latitude, -Longitude) %>%
  mutate(Sin.Cat = ifelse(Sin <= 1.1, 'Straight', ifelse(Sin > 1.3, 'Sinuous/Meandering', 'Low Sinuosity')),
         BfBraid.Cat = ifelse(BfBraid <= 1, 'Non-Braided', ifelse(BfBraid >= 2, 'Braided', 'Wandering')),
         Sin.Cat = factor(Sin.Cat, levels = c('Straight', 'Low Sinuosity', 'Sinuous/Meandering')),
         BfBraid.Cat = factor(BfBraid.Cat, levels = c('Non-Braided', 'Wandering', 'Braided')),
         LWFreq_Wet = cut(LWFreq_Wet, breaks = c(-Inf, 0, 5, 20, Inf)))
         # LWFreq_Wet = cut(LWFreq_Wet, breaks = c(0, 1.6, 6.2, 20)))

# set universal axis limites
xmin = min(data$SlowWater_Freq, na.rm = TRUE)
xmax = max(data$SlowWater_Freq, na.rm = TRUE)
ymin = min(data$ChnlUnit_Freq, na.rm = TRUE)
ymax = max(data$ChnlUnit_Freq, na.rm = TRUE)


p.cv = ggplot(filter(data, Confinement == 'CV'), aes(x = SlowWater_Freq, y = ChnlUnit_Freq)) +
  geom_hline(aes(yintercept = 2.75), linetype = 4, color = 'orange') +
  geom_hline(aes(yintercept = 6), linetype = 4, color = 'blue') +
  geom_vline(aes(xintercept = 2.4), linetype = 4, color = 'orange') +
  geom_vline(aes(xintercept = 4), linetype = 4, color = 'blue') +
  geom_point(aes(shape = Sin.Cat, colour = LWFreq_Wet), alpha = 0.7, size = 4) +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(~ BfBraid.Cat, drop = FALSE, nrow = 2) +
  xlim(xmin, xmax) + ylim(ymin, ymax) +
  labs(title = 'Confined Valley (CV) Geoindicators') + 
  theme(legend.position="bottom")

  # + theme_light()

p.pcv = ggplot(filter(data, Confinement == 'PCV'), aes(x = SlowWater_Freq, y = ChnlUnit_Freq)) +
  geom_hline(aes(yintercept = 2.75), linetype = 4, color = 'orange') +
  geom_hline(aes(yintercept = 6), linetype = 4, color = 'blue') +
  geom_vline(aes(xintercept = 2.4), linetype = 4, color = 'orange') +
  geom_vline(aes(xintercept = 4), linetype = 4, color = 'blue') +
  geom_point(aes(shape = Sin.Cat, colour = LWFreq_Wet), alpha = 0.7, size = 4) +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(~ BfBraid.Cat, drop = FALSE, nrow = 2) +
  xlim(xmin, xmax) + ylim(ymin, ymax) +
  labs(title = 'Partly Confined Valley (PCV) Geoindicators') + 
  theme(legend.position="bottom")

p.ucv = ggplot(filter(data, Confinement == 'UCV'), aes(x = SlowWater_Freq, y = ChnlUnit_Freq)) +
  geom_hline(aes(yintercept = 2.75), linetype = 4, color = 'orange') +
  geom_hline(aes(yintercept = 6), linetype = 4, color = 'blue') +
  geom_vline(aes(xintercept = 2.4), linetype = 4, color = 'orange') +
  geom_vline(aes(xintercept = 4), linetype = 4, color = 'blue') +
  geom_point(aes(shape = Sin.Cat, colour = LWFreq_Wet), alpha = 0.7, size = 4) +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(~ BfBraid.Cat, drop = FALSE, nrow = 2) +
  xlim(xmin, xmax) + ylim(ymin, ymax) +
  labs(title = 'Unconfined Valley (UCV) Geoindicators') + 
  theme(legend.position="bottom")

p.not.cv = ggplot(filter(data, Confinement != 'CV'), aes(x = SlowWater_Freq, y = ChnlUnit_Freq)) +
  geom_hline(aes(yintercept = 2.75), linetype = 4, color = 'orange') +
  geom_hline(aes(yintercept = 6), linetype = 4, color = 'blue') +
  geom_vline(aes(xintercept = 2.4), linetype = 4, color = 'orange') +
  geom_vline(aes(xintercept = 4), linetype = 4, color = 'blue') +
  geom_point(aes(shape = Sin.Cat, colour = LWFreq_Wet), alpha = 0.7, size = 4) +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(~ BfBraid.Cat, drop = FALSE, nrow = 2) +
  xlim(xmin, xmax) + ylim(ymin, ymax) +
  labs(title = 'NOT Confined Valley (CV) Geoindicators') + 
  theme(legend.position="bottom")

ggsave(file.path(out.dir, 'Geoindicator_CV.png'), p.cv, width = 10, height = 7)
ggsave(file.path(out.dir, 'Geoindicator_PCV.png'), p.pcv, width = 10, height = 7)
ggsave(file.path(out.dir, 'Geoindicator_UCV.png'), p.ucv, width = 10, height = 7)
ggsave(file.path(out.dir, 'Geoindicator_!CV.png'), p.not.cv, width = 10, height = 7)

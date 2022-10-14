# Tom Offrede
# Make figures for the paper

library(tidyverse)
library(ggsignif)
library(ggdist)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/FiguresForPaper/"

order <- c("Sitting", "Light", "Heavy")
orderBase <- c("Baseline", order)
labels <- c("Sitting"="Sitting", "Light"="Light B.", "Heavy"="Heavy B.")

load(paste0(folder, "DataSpeech.RData"))
load(paste0(folder, "DataBreathing.RData"))
load(paste0(folder, "DataReadSpeech.RData"))

# Plots I want to have:
# 
# CONFEDERATE:
# f0 across conditions, maybe one for read and one for free speech (but one plot)
# speech rate across conditions (free speech)
# breathing rate across conditions (free speech)

df <- fsm %>% 
  filter(Speaker=="Confederate")
dr <- frb %>% 
  filter(Speaker=="Confederate")
df <- df[!duplicated(df),] %>% 
  select(f0raw, Condition)
dr <- dr[!duplicated(dr),] %>% 
  select(f0raw, Condition)

dfr <- rbind(df, dr)
ggplot(dfr, aes(Condition, f0raw))+
  stat_halfeye(adjust = .6,  width = .6, justification = -.2, .width = c(.5, .95))+
  geom_boxplot(width=.1)+
  scale_x_discrete(limits = order, labels=labels)+
  geom_signif(comparisons = list(c("Sitting", "Light"), c("Light", "Heavy")), annotations = c("***", "***"))+
  labs(title = "Confederate's f0 (read and spontaneous speech)", y = "Fundamental frequency")

df <- fsm %>% 
  filter(Speaker=="Confederate")
df <- df[!duplicated(df),] %>% 
  select(articRate, speechRateIPU, Condition)

ggplot(df, aes(Condition, speechRateIPU))+
  stat_halfeye(adjust = .6,  width = .6, justification = -.2, .width = c(.5, .95))+
  geom_boxplot(width=.1)+
  scale_x_discrete(limits = order, labels=labels)+
  geom_signif(comparisons = list(c("Sitting", "Light"), c("Light", "Heavy")), annotations = c("*", "***"))+
  labs(title = "Confederate's speech rate (spontaneous speech)", y = "Speech rate")


# 
# PARTICIPANTS:
# breathing rate of listening vs watching
# inhalation amplitude across conditions during watching
# inhalation duration across conditions OR in alone vs interaction (read and free speech, maybe one plot with both)
# inhalation duration per number of IPUs
# f0 across conditions during read and free speech (one plot with both or separate?)
# change in f0 per change in breathing rate and maybe per change in speech rate
# 
# 
# 
# 
# 
# 
# 
# 

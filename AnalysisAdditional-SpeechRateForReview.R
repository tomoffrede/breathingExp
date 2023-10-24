# Tom Offrede
# Additional analysis for review rebuttal of paper: participants' and confederate's speech rate

library(tidyverse)
library(lme4)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"

load(paste0(folder, "DataSpeechRate_Reading.RData"))

# Participants

dat <- srr %>% 
  filter(speaker != "Confederate", condition !="Baseline")

ggplot(dat, aes(condition, articRateIPU))+
  geom_boxplot()

summary(lmer(articRateIPU ~ condition + (1 + condition | speaker), dat))

# Confederate

dat <- srr %>% 
  filter(speaker == "Confederate")

ggplot(dat, aes(condition, articRateIPU))+
  geom_boxplot()

summary(lm(articRateIPU ~ condition, dat))


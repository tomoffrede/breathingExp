# Tom Offrede
# Additional analysis for review rebuttal of paper: confederate's breathing behavior during "watching" period

library(tidyverse)
library(lme4)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"

load(paste0(folder, "DataConfBaselineBreathing.RData"))

bc$condition <- relevel(bc$condition, ref="Light")

ggplot(bc, aes(condition, cycleDur))+
  geom_boxplot()+
  scale_x_discrete(limits = c("Sitting", "Light", "Heavy"))
summary(lm(cycleDur ~ condition, bc))

ggplot(bc, aes(condition, inhalAmp))+
  geom_boxplot()+
  scale_x_discrete(limits = c("Sitting", "Light", "Heavy"))
summary(lm(inhalAmp ~ condition, bc))


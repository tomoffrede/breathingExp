# Tom Offrede
# Make figures for the paper

library(tidyverse)
library(ggsignif)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folder2 <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/FiguresForPaper/"

order <- c("Sitting", "Light", "Heavy")
orderBase <- c("Baseline", order)

load(paste0(folder, "DataSpeech.RData"))
load(paste0(folder, "DataBreathing.RData"))
load(paste0(folder, "DataReadSpeech.RData"))

# Plots I want to have:
# 
# CONFEDERATE:
# f0 across conditions, maybe one for read and one for free speech (but one plot)
# speech rate across conditions (free speech)
# breathing rate across conditions (free speech)
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

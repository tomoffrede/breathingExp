library(tidyverse)
library(viridis)
library(cowplot)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folder1 <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Plots/"
folder2 <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Plots/FreeSpeech/"
folder3 <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Plots/FreeSpeech/Differences/"

order <- c("Baseline", "Sitting", "Light", "Heavy")

load(paste0(folder, "DataNoDiff.RData"))

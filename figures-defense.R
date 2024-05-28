# Tom Offrede
# Figures for defense (not the ones relating to the original project)

library(tidyverse)
library(ggsignif)
library(ggdist)
library(tuneR)
library(broom.mixed)
library(ggpubr)
library(lme4)
library(viridis)
Sys.setenv(LANG="en")

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/FiguresForPaper/"
folder3 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/PeaksValleys/"

order <- c("Sitting", "Light B.", "Heavy B.")
order2 <- c("Sitting", "Light", "Heavy")
orderBase <- c("Baseline", order)
labels <- c("Sitting"="Sitting", "Light"="Light B.", "Heavy"="Heavy B.")

load(paste0(folder, "DataSpeech.RData"))
load(paste0(folder, "DataBreathing.RData"))
load(paste0(folder, "DataReadSpeech.RData"))
load(paste0(folder, "DataConfBaselineBreathing.RData"))

theme_set(theme_minimal()+
            theme(axis.ticks.y=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title = element_text(size=24),
                  axis.text.x = element_text(color="black", size=24),
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.title = element_text(hjust = 0.5, size=24),
                  strip.background = element_blank(),
                  strip.text = element_text(size=24)
            ))

ggplot(frb %>% filter(Role=="Confederate"), aes(Condition, f0raw))+
  geom_boxplot()+
  labs(title= "Confederate",
       y = "f0 mean (Hz)",
       x = NULL)#+
  # geom_signif(comparisons = list(c("Sitting", "Light"), c("Sitting", "Heavy")),
  #             annotations = c("*", "**"),
  #             y=c(5.2, 5.75))+
  # scale_x_discrete(limits = order2, labels=c("Sitting", "Light B.", "Heavy B."))+
  # ylim(c(2.8, 8.5))


ggplot(frb %>% filter(Role=="Participant", Task=="ReadAlone"), aes(Condition, f0raw))+
  geom_boxplot()+
  labs(title= "Participants",
       y = "f0 mean (Hz)",
       x = NULL)

ggplot(frb %>% filter(Role=="Participant", Task=="ReadJoint", f0raw>180), aes(Condition, f0raw))+
  geom_boxplot()+
  labs(title= "Participants",
       y = "f0 mean (Hz)",
       x = NULL)




























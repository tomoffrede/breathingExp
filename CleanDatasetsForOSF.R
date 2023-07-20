# Tom Offrede
# Clean data to publish on OSF for paper (JSLHR)

library(tidyverse)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"

folderSMC <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/SMC-JSLHR/"

###################################
######## Breathing file
###################################

load(paste0(folder, "DataBreathing.RData"))
brm <- brm %>% 
  select(Speaker, act, breathCycle, cycleDur, inhalAmp, Condition, Task, Role) %>% 
  rename(speaker = Speaker,
         task = Task,
         condition = Condition,
         role = Role) %>%
  mutate_at(c("task", "act"), as.character) %>% 
  filter((task != "Free") %>% replace_na(TRUE)) %>%  # if you don't replace_na, filter() filters out all NAs
  filter((condition!= "Baseline"), replace_na(TRUE))

# mutating isnt working, so change it separately below:
brm$task[brm$act == "watching"] <- "Watch"
brm$act <- NULL

colSums(is.na(brm)) # check if there are any NAs in the dataset

datBreath <- brm

save(datBreath, file=paste0(folderSMC, "DataBreathing.RData"))

###################################
######## Speech file
###################################

load(paste0(folder, "DataReadSpeech.RData"))

frb <- frb %>% 
  select(Speaker, f0raw, Task, IPU, Condition, Role) %>% 
  rename(speaker = Speaker,
         f0 = f0raw,
         task = Task,
         voicedPeriod = IPU,
         condition = Condition,
         role = Role) %>% 
  mutate_at("voicedPeriod", as.factor) %>% 
  distinct() %>% 
  group_by(speaker, task, condition) %>% 
  mutate(voicedPeriod = 1:n()) %>% 
  ungroup() %>% 
  filter((condition!= "Baseline"), replace_na(TRUE))

colSums(is.na(frb)) # check if there are any NAs in the dataset

datSpeech <- frb

save(datSpeech, file=paste0(folderSMC, "DataSpeech.RData"))
# Tom Offrede
# Add confederate's breathing data during baseline (act=="watching") to previous dataset


library(rPraat) # to work with TextGrids
library(tidyverse)
library(pracma)
library(tuneR) # to read WAV (breathing) file

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/" # folder with all needed files
`%!in%` <- Negate(`%in%`)
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy} # taken from the plyr package: https://github.com/hadley/plyr/blob/34188a04f0e33c4115304cbcf40e5b1c7b85fedf/R/round-any.r#L28-L30
# see https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr/46489816#46489816

load(paste0(folder, "DataBreathing.RData"))

folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/PeaksValleys/"

listT <- list.files(folder2, pattern="Baseline_THORAX\\.TextGrid")

listW <- list.files(folder2, pattern="Baseline_THORAX\\.wav")

files <- as.data.frame(cbind(listT, listW))
files <- files %>% 
  rename(breath = listT, wav = listW) %>% 
  mutate(worked = ifelse(gsub(".TextGrid", "", breath) == gsub(".wav", "", wav), "worked!", "NO!!!!"))
table(files$worked)

##########

bc <- data.frame(matrix(ncol=12, nrow=0))
colnames(bc) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "inhalAmp", "inhalDur")

for(i in 1:nrow(files)){
  act <- "watching"
  
  breath <- tg.read(paste0(folder2, files$breath[i]))
  b <- (w <- readWave(paste0(folder2, files$wav[i])))@left
  b <- (b - min(b)) / (max(b) - min(b))
  
  # get time of each point (peaks and valleys)
  PVtimes <- data.frame(matrix(ncol=12, nrow=0))
  colnames(PVtimes) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "inhalAmp", "inhalDur")
  for(t in 1:(tg.getNumberOfPoints(breath, 2)-1)){ # number of valleys minus one (the last valley is only counted as offset of its previous cycle)
    PVtimes[nrow(PVtimes)+1,] <- c(substr(files$breath[i], 1, 10),
                                   act,
                                   paste0("cycle", t), # number of breathing cycle
                                   as.numeric(tg.getPointTime(breath, 2, t)),
                                   as.numeric(tg.getPointTime(breath, 1, t)),
                                   as.numeric(tg.getPointTime(breath, 2, t+1)), # next valley
                                   NA, NA, NA, NA,
                                   as.numeric(b[tg.getPointTime(breath, 1, t)*w@samp.rate] - b[tg.getPointTime(breath, 2, t)*w@samp.rate]), # inhalation amplitude
                                   NA)
  }
  PVtimes[, c("onset", "peak", "offset")] <- lapply(PVtimes[, c("onset", "peak", "offset")], as.numeric)
  PVtimes$cycleOK[PVtimes$onset < PVtimes$peak & PVtimes$peak < PVtimes$offset] <- "OK!"
  PVtimes$cycleOK[PVtimes$cycleOK=="NA"] <- "Not OK!"
  table(PVtimes$cycleOK) # make sure that onset < peak < offset (i.e., the cycles make sense)
  
  PVtimes$cycleDur <- PVtimes$offset - PVtimes$onset # duration of each cycle
  PVtimes$numberBreathCycles <- nrow(PVtimes)
  PVtimes$breathCycleDurMean <- mean(PVtimes$cycleDur)
  PVtimes$breathRate <- (nrow(PVtimes) / ((PVtimes$offset[nrow(PVtimes)] - PVtimes$onset[1]) / 60)) # breathing rate = number of cycles / time from first to last valley divided by 60 (to turn into minutes)
  PVtimes$inhalDur <- PVtimes$peak - PVtimes$onset
  
  PVtimes <- PVtimes %>% select(-c("cycleOK"))
  
  bc <- rbind(bc, PVtimes)
}

bc <- bc %>% 
  mutate(condition = case_when(
    substr(file, 1, 1) == "S" ~ "Sitting",
    substr(file, 1, 1) == "L" ~ "Light",
    substr(file, 1, 1) == "H" ~ "Heavy"
  )) %>% 
  mutate_at(c("condition", "file", "act"), as.factor) %>% 
  mutate_at("inhalAmp", as.numeric)

save(bc, file=paste0(folder, "DataConfBaselineBreathing.RData"))

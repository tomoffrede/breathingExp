# Tom Offrede
# join speech data of free and read speech datasets

library(tidyverse)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"

load(paste0(folder, "DataReadSpeech.RData"))
load(paste0(folder, "DataSpeech.RData"))

free <- fsm %>% 
  select(file, Speaker, IPU, f0raw, IPUDur, articRateIPU, Condition, Task) %>% 
  mutate_at("IPU", as.integer)

read <- frb %>% 
  select(file, Speaker, IPU, f0raw, IPUDur, articRateIPU, Condition, Task) #%>%
  # mutate(file = ifelse(grepl("Hirsch|Pferd|Schwalbe", file), substr(file, 1, 6), file))

# save the number of IPUs (i.e., of rows) for each BR file of each speaker
# save IPU number as 1:n across all BR files, with the first BR file being Hirsch, the second Schwalbe, and the third Pferd

brIPU <- data.frame(matrix(nrow=0, ncol=3))
names(brIPU) <- c("speaker", "text", "n")

for(f in unique(read$file[grepl("Hirsch|Pferd|Schwalbe", read$file)])){
  brIPU[nrow(brIPU)+1,] <- c(substr(f, 4, 6), substr(f, 8, 8), sum(with(read, file==f)))
}

brIPU <- brIPU %>% 
  pivot_wider(names_from = text, values_from = n) %>% 
  mutate_at(c("H", "S", "P"), as.numeric) %>% 
  mutate(Sstart = H + 1,
         Pstart = Sstart + S)

readB <- read %>% 
  filter(Condition == "Baseline")


for(f in unique(readB$Speaker)){
  readB <- readB %>% 
    group_by(file) %>% 
    arrange(.by_group = TRUE) %>% 
    ungroup() %>% 
    group_by(Speaker) %>% 
}

read <- read %>% 
  mutate(IPU = ifelse(grepl("Hirsch", file), 1:brIPU$H[brIPU$speaker==Speaker], ifelse(grepl("Schwalbe", file), brIPU$Sstart[brIPU$speaker==Speaker]:brIPU$S[brIPU$speaker==Speaker], brIPU$Pstart[brIPU$speaker==speaker]:brIPU$P[brIPU$speaker==speaker])))


read <- read %>% 
  filter(Condition != "Baseline")

# dplyr pipeline to mutate() IPU with ifelse(grepl("text", f))?
# and after that rename all BR- to remove text name

j <- full_join(free, read, by=c("Speaker", "IPU", "Condition"))


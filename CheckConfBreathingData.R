# Figuring out if confederate's breathing data is right (biking with lower breath rate than sitting)

library(rPraat) # to work with TextGrids
library(tidyverse)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/" # folder with all needed files
`%!in%` <- Negate(`%in%`)

### Get the data (copied from Preprocessing.R)

listCB <- list.files(folder, pattern="THORAX") # we use the measurement from the thorax only for the confederate
listCB <- listCB[grepl(".TextGrid", listCB)]

cbr <- data.frame(matrix(ncol=3, nrow=0))
colnames(cbr) <- c("file", "peaksSpeech", "valleysSpeech")

for(i in listCB){
  tg <- tg.read(paste0(folder, i))
  cbr[nrow(cbr)+1,] <- c(i, tg.getNumberOfPoints(tg, 1), tg.getNumberOfPoints(tg, 2))
}

cbr <- cbr %>%
  mutate(file = gsub("_200.TextGrid", "", file)) %>%
  mutate(file = gsub("_THORAX", "", file))

cbr$fileOG <- cbr$file
cbr$file <- paste0(substr(cbr$file, 1, 6))
cbr$file[grepl("alone", cbr$fileOG)] <- paste0(substr(cbr$fileOG[grepl("alone", cbr$fileOG)], 1, 1), "A", substr(cbr$fileOG[grepl("alone", cbr$fileOG)], 3, 6))
cbr$file[grepl("joint", cbr$fileOG)] <- paste0(substr(cbr$fileOG[grepl("joint", cbr$fileOG)], 1, 1), "J", substr(cbr$fileOG[grepl("joint", cbr$fileOG)], 3, 6))
cbr$fileOG <- NULL

########################

ggplot(cbr %>% filter(substr(file, 2, 2)!="-"), aes(file, peaksSpeech))+
  geom_point(size=5)






























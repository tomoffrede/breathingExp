library(sylly.de)
library(emuR)
library(rPraat)
library(tidyverse)

# We first extract the word length for all tokens, sum them up and then divide by the syllable numbers (counted by "hyphen"-function).
# An example looks like this - to extract data on word length, we use the emuR-package

# meta <- tok.df %>% group_by(speaker) %>%
#  dplyr::summarise(sprechzeit=sum(end-start), tokens=length(labels), syllables = sum(hyphen(labels, hyph.pattern="de", as = 'numeric')))

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"

listTG <- list.files(folder, "TextGrid")
i = "BF-ATN003_AUDIO.TextGrid" 

tg <- tg.read(paste0(folder, i))

numbInt <- tg.getNumberOfIntervals(tg, 1)

script <- data.frame(matrix(ncol=2, nrow=0))
names(script) <- c("file", "words")

start <- tg.getIntervalStartTime(tg, 2, 2)
end <- tg.getIntervalEndTime(tg, 2, 2)
for(n in 1:numbInt){
  #only get labels from within "task" interval
  if(tg.getIntervalStartTime(tg, 1, n) >= tg.getIntervalStartTime(tg, 2, 2) &&
     tg.getIntervalStartTime(tg, 1, n) < tg.getIntervalEndTime(tg, 2, 2)){
    label <- tg.getLabel(tg, 1, n)
    if(label != "" && label != "<usb>"){ # every other interval is empty (silence); don't save those to the data frame
      script[nrow(script)+1,] <- c(i, label)
    }
  }
}

# script <- script %>% filter(words != "<usb>")
# 
# script <- script %>% 
#   mutate(words = strsplit(as.character(words), " ")) %>%
#   unnest(words) %>%
#   mutate(file = gsub(".TextGrid", "", file))

# hyphen(script$words, hyph.pattern="de")
sep <- hyphen(script$words, hyph.pattern="de")@hyphen


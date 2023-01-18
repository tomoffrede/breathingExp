# Tom Offrede
# remove unwanted tiers from textgrids

library(rPraat)
library(tidyverse)

folderOld <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folderNew <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/modified/"

files <- list.files(folderOld, "TextGrid")

for(f in files){
  if(grepl("alone|joint", f)){
    tg <- tg.read(paste0(folderOld, f), encoding=detectEncoding(paste0(folderOld, f)))
    if(tg.getTierName(tg, 1) == "union"){
      if(tg.getNumberOfTiers(tg) == 3){
        new0 <- tg.removeTier(tg, 3)
        new <- tg.removeTier(new0, 2)
        tg.write(new, paste0(folderNew, f))
      }
    }
  }
}
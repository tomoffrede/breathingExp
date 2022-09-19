# Get all TextGrids
# Write new TextGrids with different name

library(rPraat)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/BR-task/"
newfolder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"


listfiles <- list.files(folder, "TextGrid")
# listfiles <- listfiles[substr(listfiles, 2, 2)=="F"]

for(i in listfiles){
  tg <- tg.read(paste0(folder, i), encoding=detectEncoding(paste0(folder, i)))
  name <- gsub(".TextGrid", "_New.TextGrid", i)
  tg.write(tg, paste0(newfolder, name))
}
# Done :)
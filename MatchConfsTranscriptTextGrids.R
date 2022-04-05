# get textgrid of the original raw files of confederate's speech, containing the time when the task (speech) started and when breathing started. these intervals were used to cut the audio and breathing files
# get old transcript textgrids (with text of free speech) of confederate's free speech
# calculate the time shift between them -- the new speech files were cut to the same timestamps as the breathing files (i.e. including inhalation before and exhalation after speech boundaries)

library(rPraat)

folderraw <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/Confederate/TGsFromOriginalRawFiles/"
folderold <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/Confederate/CorrectedTextGridsWithTranscripts/"
foldernew <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/Confederate/TranscriptTextGridsToAudioOfBreathingLength/"
folderbr <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/PeaksValleys/"

listRAW <- list.files(folderraw, ".TextGrid")
listOLD <- list.files(folderold, "shifted")
listBR <- list.files(folderbr, "corrected")

list <- data.frame(cbind(listOLD, listRAW)) # check to see if the files are right and corresponding
list <- data.frame(cbind(list, listBR))

# get raw TGs, calculate difference between start of task and start of breath intervals

for(i in 1:nrow(list)){
  raw <- tg.read(paste0(folderraw, list$listRAW[i]), encoding=detectEncoding(paste0(folderraw, list$listRAW[i])))
  
  # almost all, but not all, of the TextGrids start the first tier with a "instructions" interval. let's delete that interval:
  if(tg.getLabel(raw, 1, 2) == "instructions"){
    raw <- tg.removeIntervalBothBoundaries(raw, 1, 2)
  }
  
  # now calculate the time between start of "task" and start of "breath"
  shift <- tg.getIntervalStartTime(raw, 1, 2) - tg.getIntervalStartTime(raw, 2, 2)
  
  # now use this difference to create new textgrids exactly like the OLD ones (with the speech transcript), but shifted in time
  old <- tg.read(paste0(folderold, list$listOLD[i]), encoding=detectEncoding(paste0(folderold, list$listOLD[i])))
  breath <- tg.read(paste0(folderbr, list$listBR[i]), encoding = detectEncoding(paste0(folderbr, list$listBR[i])))
  new <- tg.createNewTextGrid(tg.getStartTime(breath), tg.getEndTime(breath)) #add here the tiers. then, add points with the times + diff of tg
  new <- tg.insertNewIntervalTier(new, newInd=1, "silences")
  for(p in 1:tg.getNumberOfIntervals(old, 1)){
    if(tg.getIntervalEndTime(old, 1, p) < tg.getEndTime(new)){ # the new textgrid is the size of the breathing textgrid, i.e. shorter than the `old` textgrid, so make sure it ignores the last interval of `old` (which is just a blank)
      new <- tg.insertInterval(new,
                               1,
                               (tg.getIntervalStartTime(old, 1, p) + shift),
                               (tg.getIntervalEndTime(old, 1, p) + shift),
                               tg.getLabel(old, 1, p))
    } else{break}
  }
  file <- gsub("shifted", "alignedToBreath", list$listOLD[i])
  tg.write(new, paste0(foldernew, file))
}
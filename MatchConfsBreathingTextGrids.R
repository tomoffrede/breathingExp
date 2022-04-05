# get old and new breathing textgrids (with peaks and valleys) of confederate's free speech
# calculate the time shift between them -- the new breathing files were cut so that they included the inhalation before and exhalation after speech, whereas the old files did not include this extra peak and valley

library(rPraat)

folderold <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Confederate/Breathing/"
foldernew <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Confederate/BreathingWithEntireCycle-Free/"
foldernewTG <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Confederate/BreathingWithEntireCycle-Free/OldTextGridsCorrected/"


listOLD <- list.files(folderold, "TextGrid")
listOLD <- listOLD[grepl("Home", listOLD) | grepl("Holidays", listOLD) | grepl("Hobbies", listOLD)]
listNEW <- list.files(foldernew, "\\.TextGrid")

list <- data.frame(cbind(listOLD, listNEW)) # check to see if the files are right and corresponding

# the first peak of each of these files is correct (was checked manually)

diff <- data.frame(matrix(ncol=4, nrow=0))
names(diff) <- c("file", "old", "new", "difference")

for(i in 1:nrow(list)){
  old <- tg.read(paste0(folderold, list$listOLD[i]), encoding=detectEncoding(paste0(folderold, list$listOLD[i])))
  new <- tg.read(paste0(foldernew, list$listNEW[i]), encoding=detectEncoding(paste0(foldernew, list$listNEW[i])))
  diff[nrow(diff)+1,] <- c(list$listOLD[i], as.numeric(tg.getPointTime(old, 1, 1)), as.numeric(tg.getPointTime(new, 1, 1)), NA)
}

diff$old <- as.numeric(diff$old)
diff$new <- as.numeric(diff$new)
diff$difference <- diff$new - diff$old

# now, to each point of each textgrid, we add the corresponding difference

for(i in 1:nrow(list)){
  old <- tg.read(paste0(folderold, list$listOLD[i]), encoding=detectEncoding(paste0(folderold, list$listOLD[i])))
  oldnew <- tg.read(paste0(foldernew, list$listNEW[i]), encoding=detectEncoding(paste0(foldernew, list$listNEW[i])))
  new <- tg.createNewTextGrid(tg.getStartTime(oldnew), tg.getEndTime(oldnew)) #add here the tiers. then, add points with the times + diff of tg
  new <- tg.insertNewPointTier(new, newInd=1, "peaks")
  new <- tg.insertNewPointTier(new, newInd=2, "valleys")
  for(p in 1:tg.getNumberOfPoints(old, 1)){
    new <- tg.insertPoint(new, 1, (tg.getPointTime(old, 1, p) + diff$difference[i]), "peak")
  }
  for(v in 1:tg.getNumberOfPoints(old, 2)){
    new <- tg.insertPoint(new, 2, (tg.getPointTime(old, 2, v) + diff$difference[i]), "valley")
  }
  file <- gsub(".TextGrid", "_corrected.TextGrid", list$listOLD[i])
  tg.write(new, paste0(foldernewTG, file))
}


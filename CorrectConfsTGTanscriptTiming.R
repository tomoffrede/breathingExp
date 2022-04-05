# Correct confederate's transcription timing in TextGrids

library(rPraat)

folderwithTG <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/Confederate/CorrectedTextGridsWithTranscripts/"

listTG <- list.files(folderwithTG)
# free <- c("H-Hobbies.TextGrid", "H-Holidays.TextGrid", "H-Home.TextGrid", "L-Hobbies.TextGrid", "L-Holidays.TextGrid", "L-Home.TextGrid", "S-Hobbies.TextGrid", "S-Holidays.TextGrid", "S-Home.TextGrid")

timing <- data.frame(matrix(ncol=3, nrow=0))
names(timing) <- c("file", "start", "end")
  
for(i in listTG){
  tg <- tg.read(paste0(folderwithTG, i), encoding=detectEncoding(paste0(folderwithTG, i)))
  timing[nrow(timing)+1,] <- c(i, tg.getIntervalStartTime(tg, 1, 2), tg.getIntervalEndTime(tg, 1, tg.getNumberOfIntervals(tg, 1)))
}


# didn't work:
# c <- rPraat::tg.read("C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/Confederate/S-Holidays.TextGrid")
# 
# FullPath = function(FileName){ return( paste( "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/Confederate/", FileName, sep="") ) }
# 
# praat("Extract part...",
#       arguments=list(startTime=rPraat::tg.getIntervalStartTime(c, 1, 2),
#                                      endTime=rPraat::tg.getIntervalEndTime(c, 1, rPraat::tg.getNumberOfIntervals(c, 1)),
#                                      "no"),
#       input=FullPath("S-Holidays.TextGrid"),
#       output=FullPath("S-Holidays_shifted.TextGrid"))
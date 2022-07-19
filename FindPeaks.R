library(tuneR) # read wav file
# library(kza) # smooth/filter
library(pracma) # finds peaks
library(quantmod)
library(rPraat)
library(tidyverse)

setwd(folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/PeaksValleys/runScript/")


### creating plots of all the breathing files

listWAV <- list.files(folder, pattern=".wav")
listWAV <- listWAV[grepl("200_", listWAV) & !grepl("breath", listWAV)]

# 
# for (i in listWAV){
#   dat <- readWave(paste0(folder, i))
#   datscaled <- (dat@left - min(dat@left))/(max(dat@left) - min(dat@left)) # scale data from 0 to 1 for easier visualization
#   filename <- paste0(folder, substr(i, 1, 6), ".png")
#   png(file=filename, width=2000, height=1000)
#   print({
#     plot(datscaled, type="l")
#   })
#   dev.off()
# }

### end of plots

### get some manually counted peaks for comparison
# easy <- c("BF-JPW003_SUM_200.wav", "BF-TKJ003_SUM_200.wav", "BF-OTC030_SUM_200.wav", "LB-TSE011_SUM_200.wav", "LB-YRI008_SUM_200.wav", "LB-YED008_SUM_200.wav", "LF-QRC013_SUM_200.wav", "LR-TSE013_SUM_200.wav", "HF-QRC014_SUM_200.wav", "HB-YRI011_SUM_200.wav", "SB-CZW008_SUM_200.wav", "SB-ATN008_SUM_200.wav", "SB-DIN008_SUM_200.wav", "SB-PER011_SUM_200.wav", "SB-YED011_SUM_200.wav", "SB-WQG011_SUM_200.wav")
# mpeaks <- c(20, 19, 19, 11, 19, 13, 20, 36, 20, 18, 13, 19, 6, 19, 10, 11)

# 
# easy <- c("BF-BND003_SUM_200_breath.wav", "BF-PER003_SUM_200_breath.wav", "BR-DAM002_SUM_200_breath.wav", "BR-JPW002_SUM_200_breath.wav", "HF-CBE007_SUM_200_breath.wav", "HF-DAM010_SUM_200_breath.wav", "HF-PER007_SUM_200_breath.wav", "HR-BND011_SUM_200_alone_breath.wav", "HR-CBE006_SUM_200_alone_breath.wav", "HR-DIN012_SUM_200_alone_breath.wav", "HR-JPW010_SUM_200_alone_breath.wav", "HR-KRU006_SUM_200_alone_breath.wav", "LR-CBE009_SUM_200_alone_breath.wav", "LR-DKG012_SUM_200_joint_breath.wav", "SF-QRC012_SUM_200_breath.wav", "SR-DAM012_SUM_200_alone_breath.wav", "SR-OTC035_SUM_200_alone_breath.wav")
# mpeaks <- c(20, 13, 35, 26, 18, 23, 20, 12, 19, 11, 9, 15, 12, 23, 26, 8, 10)
# 
# 
# peaks <- cbind(easy, mpeaks)
# colnames(peaks) <- c("file", "manPeaks")
# peaks <- as.data.frame(peaks)
# peaks$file <- substr(peaks$file, 1, 6)
# peaks$manPeaks <- as.numeric(peaks$manPeaks)

### end of manual peaks

### add automatically detected peaks
# 
# peaks$autPeaks <- as.numeric(NA)
# peaks$diffPeaks <- as.numeric(NA)
# 
# i= "H-Pferd_THORAX_alone_200.wav"

# toMatch <- c("HF", "LF")
# listWAV <- listWAV[grepl(paste(toMatch,collapse="|"), listWAV) & grepl("DKG", listWAV)]

for (i in listWAV){
  r <- readWave(paste0(folder, i))
  # i <- substr(i, 1, 6)
  
  ## Peaks
  
  breath <- as.numeric(r@left)
  # breath <- (breath - min(breath))/(max(breath) - min(breath)) # scale data from 0 to 1
  peak <- as.data.frame(findpeaks(breath, minpeakdistance=800))
  # nrow(peak)
  # plot(breath, type="l")
  # points(peak[,2], peak[,1], pch=2, col="red")
  
  ###
  # peaks$autPeaks[peaks$file == i] <- nrow(peak)
  # peaks$diffPeaks <- peaks$autPeaks - peaks$manPeaks
  # print(range(peaks$diffPeaks))
  
  ### text grid
  tg <- tg.createNewTextGrid(tMin=0, tMax=(length(breath)/r@samp.rate))
  tg <- tg.insertNewPointTier(tg, 1, "peaks")
  count <- 0
  for (n in 1:nrow(peak)){
    count <- count + 1
    tg <- tg.insertPoint(tg, "peaks", time=(peak[count,2]/r@samp.rate), "peak")
  }
  
  ## Valleys
  
  breath <- breath * -1
  peak <- as.data.frame(findpeaks(breath, minpeakdistance=800))

  ### text grid
  tg <- tg.insertNewPointTier(tg, 2, "valleys")
  count <- 0
  for (n in 1:nrow(peak)){
    count <- count + 1
    tg <- tg.insertPoint(tg, "valleys", time=(peak[count,2]/r@samp.rate), "valley")
  }
  # tg.write(tg, paste0(gsub(".wav", "", i), ".TextGrid"))
  tg.write(tg, paste0(i, ".TextGrid"))
}



# 
# file <- "BF-JPW003_SUM_200.wav"
# 
# r <- readWave(paste0(folder, file))
# summary(r) # get number of samples, duration (seconds), sampling rate, number of channels
# breath <- as.numeric(r@left)
# 
# 
# peaks <- findpeaks(breath, minpeakdistance=900)
# ### MINIMUM > multiply signal by -1? and then run function again, getting valleys
# plot(breath, type="l")
# points(peaks[,2], peaks[,1], pch=2, col="red")
# nrow(peaks)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 











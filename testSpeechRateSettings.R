library(tidyverse)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/2secsnippets/" # folder with all needed files

# listcsv <- list.files(folder, pattern="ENV.csv")
# 
# sr <- data.frame(matrix(ncol=2, nrow=0)) # dataframe for speech rate data
# colnames(sr) <- c("file", "speechRateAut")
# 
# for (i in listcsv){
#   env <- read.csv(paste0(folder,i)) # read CSV file with amplitude envelopes
#   i <- substr(i, 1, 17) # rename i
#   # interval$start <- int$start[int$file==i]*1000 # add start value to variable and multiply it: turn seconds into milliseconds
#   # interval$end <- int$end[int$file==i]*1000 # same for end value
#   # env <- env[interval$start <= env$time_ms & env$time_ms <= interval$end,] # keep amplitude envelopes only within part of the file containing the task we're instered in
#   peaks <- findpeaks(env$env) # find peaks
#   sr[nrow(sr)+1,] <- c(i, (nrow(peaks)/(nrow(env)/100))) # one row is 10 ms, so nrow = time. so numberOfPeaks/time = speech rate
# }

manual <- read.csv(paste0(folder, "Table.csv"), fileEncoding="UTF-8-BOM")
manual[,3] <- NULL
colnames(manual) <- c("file", "speechRateMan")
manual$speechRateMan <- manual$speechRateMan / 2

rn <- read.csv(paste0(folder, "ReduceNoise.csv"))
rn <- rn %>%
  mutate(name = gsub("__1_", "", name)) %>%
  select(c(1, 6)) %>%
  rename("file"="name", "AutRN"="speechrate.nsyll.dur.")

no <- read.csv(paste0(folder, "NoPre.csv"))
no <- no %>%
  mutate(name = gsub("__1_", "", name)) %>%
  select(c(1, 6)) %>%
  rename("file"="name", "AutNo"="speechrate.nsyll.dur.")

bp <- read.csv(paste0(folder, "BandPass.csv"))
bp <- bp %>%
  mutate(name = gsub("__1_", "", name)) %>%
  select(c(1, 6)) %>%
  rename("file"="name", "AutBP"="speechrate.nsyll.dur.")

rno <- merge(rn, no, by="file")
brno <- merge(rno, bp, by="file")


check <- merge(manual, brno, by="file")

check$NoDiff <- check$speechRateMan - check$AutNo
check$BPDiff <- check$speechRateMan - check$AutRN
check$RNDiff <- check$speechRateMan - check$AutBP

range(check$NoDiff)
sd(check$NoDiff)

range(check$BPDiff)
sd(check$BPDiff)

range(check$RNDiff)
sd(check$RNDiff)

png(paste0(folder, "SpeechRateDetectionCheck.png"), height=500, width=500)
par(mfrow=c(2,2))
hist(check$BPDiff, main="Band Pass filter", xlab="Manual minus automatic speech rate")
hist(check$RNDiff, main="Reduce Noise", xlab="Manual minus automatic speech rate")
hist(check$NoDiff, main="No preprocessing", xlab="Manual minus automatic speech rate")
dev.off()
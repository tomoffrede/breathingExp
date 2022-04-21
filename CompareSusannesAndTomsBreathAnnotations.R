### calculate difference in time points between Tom's and Susanne's annotations

library(rPraat)
library(tidyverse)

foldert <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Confederate/BreathingWithEntireCycle-Free/"
folders <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Confederate/BreathingWithEntireCycle-Free/Susanne/"

filess <- list.files(folders)
filest <- list.files(foldert, "TextGrid")
filest <- filest[substr(filest, 1, 6) %in% substr(filess, 1, 6)]

file <- as.data.frame(cbind(filess, filest))

diff <- data.frame(matrix(ncol=5, nrow=0))
names(diff) <- c("file", "pointS", "timeS", "pointT", "timeT")

for(i in 1:nrow(file)){
  s <- tg.read(paste0(folders, file$filess[i]), encoding = detectEncoding(paste0(folders, file$filess[i])))
  t <- tg.read(paste0(foldert, file$filest[i]), encoding = detectEncoding(paste0(foldert, file$filest[i])))
  filename <- gsub("_THORAX_200_corrected.TextGrid", "", file$filest[i])
  
  ds <- data.frame(matrix(ncol=3, nrow=0))
  names(ds) <- c("file", "pointS", "timeS")
  for(p in 1:tg.getNumberOfPoints(s, 1)){
    ds[nrow(ds)+1,] <- c(filename, paste0("peak", p), tg.getPointTime(s, 1, p))
  }
  for(v in 1:tg.getNumberOfPoints(s, 2)){
    ds[nrow(ds)+1,] <- c(filename, paste0("valley", v), tg.getPointTime(s, 2, v))
  }
  
  dt <- data.frame(matrix(ncol=2, nrow=0))
  names(dt) <- c("pointT", "timeT")
  for(p in 1:tg.getNumberOfPoints(t, 1)){
    dt[nrow(dt)+1,] <- c(paste0("peak", p), tg.getPointTime(t, 1, p))
  }
  for(v in 1:tg.getNumberOfPoints(t, 2)){
    dt[nrow(dt)+1,] <- c(paste0("valley", v), tg.getPointTime(t, 2, v))
  }
  
  d <- data.frame(cbind(ds, dt))
  diff <- rbind(diff, d)
}

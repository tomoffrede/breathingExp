### calculate difference in time points between Tom's and Susanne's annotations

library(rPraat)
library(ggplot2)

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
    ds[nrow(ds)+1,] <- c(filename, paste0("peak", p), as.numeric(tg.getPointTime(s, 1, p)))
  }
  for(v in 1:tg.getNumberOfPoints(s, 2)){
    ds[nrow(ds)+1,] <- c(filename, paste0("valley", v), as.numeric(tg.getPointTime(s, 2, v)))
  }
  
  dt <- data.frame(matrix(ncol=2, nrow=0))
  names(dt) <- c("pointT", "timeT")
  for(p in 1:tg.getNumberOfPoints(t, 1)){
    dt[nrow(dt)+1,] <- c(paste0("peak", p), as.numeric(tg.getPointTime(t, 1, p)))
  }
  for(v in 1:tg.getNumberOfPoints(t, 2)){
    dt[nrow(dt)+1,] <- c(paste0("valley", v), as.numeric(tg.getPointTime(t, 2, v)))
  }
  
  d <- data.frame(cbind(ds, dt))
  diff <- rbind(diff, d)
}

diff$timeS <- as.numeric(diff$timeS)
diff$timeT <- as.numeric(diff$timeT)

diff$timeDiff <- diff$timeS - diff$timeT

mean(diff$timeDiff)
sd(diff$timeDiff)

par(mfrow=c(2,2))
hist(diff$timeDiff, main="peaks+valleys; M = 0.007; SD = 0.056")
hist(diff$timeDiff[grepl("valley", diff$pointS)], main="valleys", xlim=c(-0.5, 0.3))
hist(diff$timeDiff[grepl("peak", diff$pointS)], main="peaks", xlim=c(-0.5, 0.3))


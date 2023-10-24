# Tom Offrede
# check distribution of f0 mean and median in data
# part of 2nd round of reviews

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folderFig <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/f0median_mean/"

load(paste0(folder, "DataReadSpeech.RData"))
load(paste0(folder, "DataReadSpeech-Median.RData"))

for(s in unique(frb$Speaker)){
  png(paste0(folderFig, s, ".png"))
  par(mfrow = c(2, 2))
  hist(frbMed$f0raw[frbMed$Speaker==s], main="Median")
  hist(frb$f0raw[frb$Speaker==s], main="Mean")
  plot(frbMed$f0raw[frbMed$Speaker==s], main="", ylab="")
  plot(frb$f0raw[frb$Speaker==s], main="", ylab="")
  dev.off()
}

# and all speakers together

png(paste0(folderFig, "all.png"), width=500, height=300)
par(mfrow = c(1, 2))
hist(frbMed$f0raw, main="Median", xlab="f0 median")
hist(frb$f0raw, main="Mean", xlab="f0 mean")
# plot(frbMed$f0raw, main="", ylab="")
# plot(frb$f0raw, main="", ylab="")
dev.off()

# calculate difference between average mean and average median in each file

diff <- data.frame(matrix(nrow=0, ncol=3))
names(diff) <- c("file", "mean", "median")

for(f in unique(frb$file[substr(frb$file, 2, 2)!="-"])){
  diff[nrow(diff)+1,] <- c(f,
                           as.numeric(abs(mean(frb$f0raw[frb$file==f], na.rm=TRUE) - mean(frbMed$f0raw[frbMed$file==f], na.rm=TRUE))),
                           as.numeric(abs(median(frb$f0raw[frb$file==f], na.rm=TRUE) - median(frbMed$f0raw[frbMed$file==f], na.rm=TRUE))))
}
diff$mean <- as.numeric(diff$mean)
diff$median <- as.numeric(diff$median)
hist(diff$mean)
hist(diff$median)

# Check the timing difference between original and downsampled breathing files

library(tuneR)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/PeaksValleys/testDownsampling/"

f <- list.files(folder)
og <- f[!grepl("200", f)]
ds <- f[grepl("200", f)]

o <- readWave(paste0(folder, og[[1]]))
s <- readWave(paste0(folder, ds[[1]]))
summary(o)
summary(s)

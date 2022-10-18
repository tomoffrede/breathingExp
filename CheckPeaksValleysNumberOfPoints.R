# check if peaks and valleys are fine
# there should be one more valley than peaks (an entire cycle is from valley to valley)

library(rPraat) # to work with TextGrids
library(tidyverse)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/" # folder with all needed files
`%!in%` <- Negate(`%in%`)
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/PeaksValleys/"

listBREATH <- list.files(folder2, pattern="SUM")
listBREATHr <- listBREATH[substr(listBREATH, 2, 2)=="R"]
listBREATHf <- listBREATH[substr(listBREATH, 2, 2)=="F"]

PV <- data.frame(matrix(ncol=3, nrow=0))
names(PV) <- c("file", "peaks", "valleys")
i <- listBREATHr[[49]]
for(i in listBREATHr){ # only read speech files
  breath <- tg.read(paste0(folder2, i), encoding = detectEncoding(paste0(folder2, i)))
  PV[nrow(PV)+1,] <- c(i, tg.getNumberOfPoints(breath, 1), tg.getNumberOfPoints(breath, 2))
}

PV$diff[PV$peaks > PV$valleys] <- "more PEAKS"
PV$diff[PV$peaks < PV$valleys] <- "more VALLEYS"
PV$diff[PV$peaks == PV$valleys] <- "same number"
table(PV$diff)

print("SAME") ; print(PV$file[PV$diff=="same number"])
print("MORE PEAKS") ; print(PV$file[PV$diff=="more PEAKS"])
# Tom Offrede
# Additional Preprocessing: get speech rate from Reading files

library(tidyverse)
library(pracma)
library(rPraat)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy} # taken from the plyr package: https://github.com/hadley/plyr/blob/34188a04f0e33c4115304cbcf40e5b1c7b85fedf/R/round-any.r#L28-L30
# see https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr/46489816#46489816

# read files (BR, HR, LR, SR) don't have the transcriptions: we can only have one speech rate value per file
# (however, we do have textgrids with silence annotations -- extracted automatically. so we could pretend that those are IPUs)

listTG <- list.files(folder, pattern="\\.TextGrid")
listTG <- listTG[!grepl("VUV", listTG)]
listCTGr <- listTG[substr(listTG, 2, 2) == "-" & grepl("Hirsch|Pferd|Schwalbe", listTG) & grepl("alone|joint", listTG)]
listPTGr0 <- listTG[substr(listTG, 2, 2)=="R"]
listPTGrb <- listPTGr0[substr(listPTGr0, 1, 1)=="B" & grepl("Hirsch|Pferd|Schwalbe", listPTGr0)]
listPTGrnb <- listPTGr0[grepl("joint|alone", listPTGr0)]
listPTGr <- c(listPTGrb, listPTGrnb)
listTGr <- c(listPTGr, listCTGr)

listCSV <- list.files(folder, ".csv")
listCCSVr <- listCSV[substr(listCSV, 2, 2) == "-" & grepl("Hirsch|Pferd|Schwalbe", listCSV) & grepl("alone|joint", listCSV)]
listPCSVr0 <- listCSV[substr(listCSV, 2, 2)=="R"]
listPCSVrb <- listPCSVr0[substr(listPCSVr0, 1, 1)=="B" & grepl("Hirsch|Pferd|Schwalbe", listPCSVr0)]
listPCSVrnb <- listPCSVr0[grepl("joint|alone", listPCSVr0)]
listPCSVr <- c(listPCSVrb, listPCSVrnb)
listCSVr <- c(listPCSVr, listCCSVr)

listR <- data.frame(cbind(listCSVr, listTGr)) %>%
  mutate(worked = ifelse(substr(listCSVr, 1, 6) == substr(listTGr, 1, 6), "worked!", "NO!!!!!!!!!!!"))
table(listR$worked)

#######

srr <- data.frame(matrix(nrow = 0, ncol=7))
names(srr) <- c("onset", "offset", "IPUDur", "IPU", "file", "articRateIPU", "Task")


for(i in 1:nrow(listR)){
  amp <- read.csv(paste0(folder, listR$listCSVr[i])) %>%
    mutate(env = (env - min(env)) / (max(env) - min(env)))
  tg <- tg.read(paste0(folder, listR$listTGr[i]), encoding=detectEncoding(paste0(folder, listR$listTGr[i])))

  # there are 3 BR files per participant so:
  if(grepl("BR", listR$listCSVr[i])){
    cond <- "ReadBaseline"
    if(grepl("Hirsch", listR$listCSVr[i])){
      file <- paste0(substr(listR$listCSVr[i], 1, 6), "-Hirsch")
    } else if(grepl("Pferd", listR$listCSVr[i])){
      file <- paste0(substr(listR$listCSVr[i], 1, 6), "-Pferd")
    } else if(grepl("Schwalbe", listR$listCSVr[i])){
      file <- paste0(substr(listR$listCSVr[i], 1, 6), "-Schwalbe")
    }
  } else {file <- substr(listR$listCSVr[i], 1, 6)}

  if(grepl("alone", listR$listCSVr[i])){
    cond <- "ReadAlone"
  }
  if(grepl("joint", listR$listCSVr[i])){cond <- "ReadJoint"}
  if(substr(file, 2, 2) == "-"){cond <- "ReadAlone"}

  ipu <- data.frame(matrix(ncol=3, nrow=0))
  names(ipu) <- c("onset", "offset", "IPUDur")
  for(n in 1:tg.getNumberOfIntervals(tg, 1)){
    if(tg.getLabel(tg, 1, n) == ""){
      ipu[nrow(ipu)+1,] <- c(as.numeric(tg.getIntervalStartTime(tg, 1, n)),
                             as.numeric(tg.getIntervalEndTime(tg, 1, n)),
                             as.numeric(tg.getIntervalDuration(tg, 1, n)))
    }
  }
  ipu <- ipu %>%
    mutate_at(c("onset", "offset", "IPUDur"), as.numeric) %>%
    mutate(IPU = 1:nrow(ipu),
           file = file,
           onsetRound = round_any(onset * 220500, 10, ceiling), # turn onset into the corresponding value considering sampling rate (as it's noted in `amp`), and then round it up
           offsetRound = round_any(offset * 220500, 10, floor), # for both these lines: not sure why I have to do sampling rate * 10, but it's what works
           articRateIPU = NA,
           Task = cond)

  for(r in 1:nrow(ipu)){
    ampTemp <- amp %>%
      filter(time_ms >= ipu$onsetRound[r] & time_ms <= ipu$offsetRound[r])
    p <- findpeaks(ampTemp$env, minpeakheight = 0.025, minpeakdistance = 22050/15) # minpeakdistance: sampling frequency divided by a very high potential speech rate
    if(is.numeric(nrow(p))){
      ipu$articRateIPU[r] <- as.numeric(nrow(p) / ipu$IPUDur[r])
    }
    plot(ampTemp$env, type="l", main=substr(listR$listCSVr[i], 1, 6))
    points(p[,2], p[,1], col="red")
  }
  ipu <- ipu %>%
    select(-c("onsetRound", "offsetRound"))
  srr <- rbind(srr, ipu)
}

save(srr, file=paste0(folder, "DataSpeechRate_Reading.RData"))


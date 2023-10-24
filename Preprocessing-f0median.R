### This script creates a dataset containing speakers' f0 mean and speech rate, and the difference between their features and that of a confederate along three different conditions
###
### The script was written for these files:
### a. Textgrids of free speech and read speech files.
###    In the free speech ones, the second interval of the second tier contains the section we are interested in.
###    In the read speech ones, the second interval of the first tier contain the "alone" section; the fourth interval, the "joint" (synchronous) section.
### b. CSV files created with the Praat script by de Jong, Pacilly & Heeren (2021), containing the speech rate in each file.
### c. TXT files created with Am?lie Rochet-Capellan's Praat script containing f0 information of WAV files (along with other acoustic information).
###
### This is how the script works:
### 1. Read the speech rate CSV files and create an object that will later be joined with the other acoustic information into a larger dataset.
### 2. Read the TXT files, remove the outliers (> 3 SDs) and annotate the mean of the f0raw of each file and mean of the f0med of each file.
### 3. Merge all datasets containing speech rate, f0 mean and median, and metadata.
###    Annotate condition, file order, difference between participants' and confederate's data and so on.
###    Calculate gender scores.
### 4. Save dataset as an R file.
###

library(rPraat) # to work with TextGrids
library(tidyverse)
library(pracma)
library(tuneR) # to read WAV (breathing) file

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/" # folder with all needed files
`%!in%` <- Negate(`%in%`)
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy} # taken from the plyr package: https://github.com/hadley/plyr/blob/34188a04f0e33c4115304cbcf40e5b1c7b85fedf/R/round-any.r#L28-L30
# see https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr/46489816#46489816

############# Speech Rate

# FROM DETECTION OF PEAKS IN AMPLITUDE ENVELOPES

### IPUs, PAUSES

################################################################################################################
############# start of articulation rate chunk (excluded from analysis)
################################################################################################################

# free speech files (BF, HF, LF, SF) have transcriptions annotations, so we can determine the speech rate of each IPU
# read files (BR, HR, LR, SR) don't have the transcriptions: we can only have one speech rate value per file
# (however, we do have textgrids with silence annotations -- extracted automatically. so we could pretend that those are IPUs)

# listTG <- list.files(folder, pattern=".TextGrid")
# listTG <- listTG[!grepl("VUV", listTG)]
# listCTGf <- listTG[grepl("Holidays|Hobbies|Home", listTG)]
# listCTGr <- listTG[substr(listTG, 2, 2) == "-" & grepl("Hirsch|Pferd|Schwalbe", listTG) & grepl("alone|joint", listTG)]
# listPTGf <- listTG[substr(listTG, 2, 2)=="F"]
# listPTGr0 <- listTG[substr(listTG, 2, 2)=="R"]
# listPTGrb <- listPTGr0[substr(listPTGr0, 1, 1)=="B" & grepl("Hirsch|Pferd|Schwalbe", listPTGr0)]
# listPTGrnb <- listPTGr0[grepl("joint|alone", listPTGr0)]
# listPTGr <- c(listPTGrb, listPTGrnb)
# listTGf <- c(listPTGf, listCTGf)
# listTGr <- c(listPTGr, listCTGr)
# 
# listCSV <- list.files(folder, ".csv")
# listCCSVf <- listCSV[grepl("Holidays|Hobbies|Home", listCSV)]
# listCCSVr <- listCSV[substr(listCSV, 2, 2) == "-" & grepl("Hirsch|Pferd|Schwalbe", listCSV) & grepl("alone|joint", listCSV)]
# listPCSVf <- listCSV[substr(listCSV, 2, 2)=="F"]
# listPCSVr0 <- listCSV[substr(listCSV, 2, 2)=="R"]
# listPCSVrb <- listPCSVr0[substr(listPCSVr0, 1, 1)=="B" & grepl("Hirsch|Pferd|Schwalbe", listPCSVr0)]
# listPCSVrnb <- listPCSVr0[grepl("joint|alone", listPCSVr0)]
# listPCSVr <- c(listPCSVrb, listPCSVrnb)
# listCSVf <- c(listPCSVf, listCCSVf)
# listCSVr <- c(listPCSVr, listCCSVr)
# 
# folderSIL <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Silences-FreeSpeech/"
# listSIL0 <- list.files(folderSIL) # need to make the confederate files be at the end of the list, so they match the other lists (CSV and TG)
# listSILp <- listSIL0[substr(listSIL0, 2, 2) != "-"]
# listSILc <- listSIL0[substr(listSIL0, 2, 2) == "-"]
# listSIL <- c(listSILp, listSILc)
# 
# listF0 <- data.frame(cbind(listCSVf, listTGf))
# listF <- data.frame(cbind(listF0, listSIL)) %>% 
#   mutate(worked1 = ifelse(substr(listCSVf, 1, 6) == substr(listTGf, 1, 6), "worked!", "NO!!!!!!!!!!!"),
#          worked2 = ifelse(substr(listCSVf, 1, 6) == substr(listSIL, 1, 6), "worked!", "NO!!!!!!!!!!!"))
# table(listF$worked1)
# table(listF$worked2)
# 
# listR <- data.frame(cbind(listCSVr, listTGr)) %>% 
#   mutate(worked = ifelse(substr(listCSVr, 1, 6) == substr(listTGr, 1, 6), "worked!", "NO!!!!!!!!!!!"))
# table(listR$worked)
# 
# srf <- data.frame(matrix(nrow = 0, ncol=6))
# names(srf) <- c("onset", "offset", "IPUDur", "IPU", "file", "speechRateIPU")
# 
# for(i in 1:nrow(listF)){
#   amp <- read.csv(paste0(folder, listF$listCSVf[i]))
#   tg <- tg.read(paste0(folder, listF$listTGf[i]), encoding=detectEncoding(paste0(folder, listF$listTGf[i])))
#   sil <- tg.read(paste0(folderSIL, listF$listSIL[i]), encoding=detectEncoding(paste0(folderSIL, listF$listSIL[i])))
#   
#   inter <- data.frame(matrix(ncol=5, nrow=0))
#   names(inter) <- c("label", "onset", "offset", "duration", "pauseDur")
#   for(n in 1:tg.getNumberOfIntervals(tg, 1)){
#     pauseDur <- 0
#     if(substr(listF$listCSVf[i], 2, 2) == "F"){ # if this file is a participant (and not confederate) file
#       if(tg.getIntervalStartTime(tg, 1, n) >= tg.getIntervalStartTime(tg, 2, as.numeric(tg.findLabels(tg, 2, "task"))) & tg.getIntervalEndTime(tg, 1, n) <= tg.getIntervalEndTime(tg, 2, as.numeric(tg.findLabels(tg, 2, "task")))){
#         for(s in 1:tg.getNumberOfIntervals(sil, 1)){
#           if(tg.getIntervalStartTime(sil, 1, s) >= tg.getIntervalStartTime(tg, 1, n) & tg.getIntervalEndTime(sil, 1, s) <= tg.getIntervalEndTime(tg, 1, n)){
#             if(tg.getLabel(sil, 1, s) == "xxx"){ # silent interval
#               pauseDur <- as.numeric(pauseDur + as.numeric(tg.getIntervalDuration(sil, 1, s)))
#             } else{pauseDur <- pauseDur}
#           }
#         }
#         inter[nrow(inter)+1,] <- c(tg.getLabel(tg, 1, n),
#                                    as.numeric(tg.getIntervalStartTime(tg, 1, n)),
#                                    as.numeric(tg.getIntervalEndTime(tg, 1, n)),
#                                    as.numeric(tg.getIntervalDuration(tg, 1, n)),
#                                    pauseDur) # transform into seconds?
#       }
#     } else if(substr(listF$listCSVf[i], 2, 2) == "-"){ # the confederate files are already cut to the right time, so I don't have to select the IPU timings
#       for(s in 1:tg.getNumberOfIntervals(sil, 1)){
#         if(tg.getIntervalStartTime(sil, 1, s) >= tg.getIntervalStartTime(tg, 1, n) & tg.getIntervalEndTime(sil, 1, s) <= tg.getIntervalEndTime(tg, 1, n)){
#           if(tg.getLabel(sil, 1, s) == "xxx"){ # silent interval
#             pauseDur <- as.numeric(pauseDur + as.numeric(tg.getIntervalDuration(sil, 1, s)))
#           }
#         }
#       }
#       inter[nrow(inter)+1,] <- c(tg.getLabel(tg, 1, n),
#                                  as.numeric(tg.getIntervalStartTime(tg, 1, n)),
#                                  as.numeric(tg.getIntervalEndTime(tg, 1, n)),
#                                  as.numeric(tg.getIntervalDuration(tg, 1, n)),
#                                  as.numeric(pauseDur))
#     }
#   }
#   inter$label[inter$label %in% c(" ", "  ", "   ", "    ", "\t")] <- ""
#   ipu <- inter %>% filter(label != "")
#   ipu <- ipu %>%
#     filter(label != "<usb>") %>%
#     rename("IPUDur"="duration")
#   ipu <- ipu %>% 
#     mutate_at(c("onset", "offset", "IPUDur", "pauseDur"), as.numeric) %>% 
#     mutate(IPU = 1:nrow(ipu),
#            file = substr(listF$listCSVf[i], 1, 6),
#            onsetRound = round_any(onset * 220500, 10, ceiling), # turn onset into the corresponding value considering sampling rate (as it's noted in `amp`), and then round it up
#            offsetRound = round_any(offset * 220500, 10, floor), # for both these lines: not sure why I have to do sampling rate * 10, but it's what works
#            speechRateIPU = NA,
#            durWithoutPauses = IPUDur - pauseDur)
#   
#   for(r in 1:nrow(ipu)){
#     ampTemp <- amp %>%
#       filter(time_ms >= ipu$onsetRound[r] & time_ms <= ipu$offsetRound[r]) %>% 
#       mutate(env = (env - min(env)) / (max(env) - min(env)))
#     p <- findpeaks(ampTemp$env, minpeakheight = 0.025, minpeakdistance = 22050/15) # minpeakdistance: sampling frequency divided by a very high potential speech rate
#     if(is.numeric(nrow(p))){
#       ipu$speechRateIPU[r] <- as.numeric(nrow(p) / ipu$IPUDur[r])
#       ipu$articRateIPU[r] <- as.numeric(nrow(p) / ipu$durWithoutPauses[r])
#     }
#     plot(ampTemp$env, type="l", main=substr(listF$listCSVf[i], 1, 6))
#     points(p[,2], p[,1], col="red", pch = 19)
#   }
#   ipu <- ipu %>% 
#     select(-c("label", "onsetRound", "offsetRound"))
#   srf <- rbind(srf, ipu)
# }
# 
# srf$speechRateIPU[srf$file=="HF-MHU"] <- NA # peak detection worked terribly for the file HF-MHU, so let's delete these speech rate values
# srf$articRateIPU[srf$file=="HF-MHU"] <- NA
# 
# 
# save(srf, file=paste0(folder, "srf.RData"))
# 
# # and now for reading files
# 
# srr <- data.frame(matrix(nrow = 0, ncol=7))
# names(srr) <- c("onset", "offset", "IPUDur", "IPU", "file", "articRateIPU", "Task")
# 
# 
# for(i in 1:nrow(listR)){
#   amp <- read.csv(paste0(folder, listR$listCSVr[i])) %>% 
#     mutate(env = (env - min(env)) / (max(env) - min(env)))
#   tg <- tg.read(paste0(folder, listR$listTGr[i]), encoding=detectEncoding(paste0(folder, listR$listTGr[i])))
#   
#   # there are 3 BR files per participant so:
#   if(grepl("BR", listR$listCSVr[i])){
#     cond <- "ReadBaseline"
#     if(grepl("Hirsch", listR$listCSVr[i])){
#       file <- paste0(substr(listR$listCSVr[i], 1, 6), "-Hirsch")
#     } else if(grepl("Pferd", listR$listCSVr[i])){
#       file <- paste0(substr(listR$listCSVr[i], 1, 6), "-Pferd")
#     } else if(grepl("Schwalbe", listR$listCSVr[i])){
#       file <- paste0(substr(listR$listCSVr[i], 1, 6), "-Schwalbe")
#     }
#   } else {file <- substr(listR$listCSVr[i], 1, 6)}
#   
#   if(grepl("alone", listR$listCSVr[i])){
#     cond <- "ReadAlone"
#   } else if(grepl("joint", listR$listCSVr[i])){cond <- "ReadJoint"}
#   if(substr(file, 2, 2) == "-"){cond <- "ReadAlone"}
#   
#   ipu <- data.frame(matrix(ncol=3, nrow=0))
#   names(ipu) <- c("onset", "offset", "IPUDur")
#   for(n in 1:tg.getNumberOfIntervals(tg, 1)){
#     if(tg.getLabel(tg, 1, n) == ""){
#       ipu[nrow(ipu)+1,] <- c(as.numeric(tg.getIntervalStartTime(tg, 1, n)),
#                              as.numeric(tg.getIntervalEndTime(tg, 1, n)),
#                              as.numeric(tg.getIntervalDuration(tg, 1, n)))
#     }
#   }
#   ipu <- ipu %>% 
#     mutate_at(c("onset", "offset", "IPUDur"), as.numeric) %>% 
#     mutate(IPU = 1:nrow(ipu),
#            file = file,
#            onsetRound = round_any(onset * 220500, 10, ceiling), # turn onset into the corresponding value considering sampling rate (as it's noted in `amp`), and then round it up
#            offsetRound = round_any(offset * 220500, 10, floor), # for both these lines: not sure why I have to do sampling rate * 10, but it's what works
#            articRateIPU = NA,
#            Task = cond)
#   
#   for(r in 1:nrow(ipu)){
#     ampTemp <- amp %>%
#       filter(time_ms >= ipu$onsetRound[r] & time_ms <= ipu$offsetRound[r])
#     p <- findpeaks(ampTemp$env, minpeakheight = 0.025, minpeakdistance = 22050/15) # minpeakdistance: sampling frequency divided by a very high potential speech rate
#     if(is.numeric(nrow(p))){
#       ipu$articRateIPU[r] <- as.numeric(nrow(p) / ipu$IPUDur[r])
#     }
#     plot(ampTemp$env, type="l", main=substr(listR$listCSVr[i], 1, 6))
#     points(p[,2], p[,1], col="red")
#   }
#   ipu <- ipu %>% 
#     select(-c("onsetRound", "offsetRound"))
#   srr <- rbind(srr, ipu)
# }
# 
# save(srr, file=paste0(folder, "srr.RData"))
# 
# # now we have: `srf`, containing speech rate data for free speech,
# # and `srr`, with speech rate data for read speech
# 
# load(paste0(folder, "srf.RData"))
# load(paste0(folder, "srr.RData"))

################################################################################################################
############# end of articulation rate chunk (excluded from analysis)
################################################################################################################


############# f0 median

# 2

listTXT <- list.files(folder, pattern=".txt")
confF <- listTXT[grepl("Home|Hobbies|Holidays", listTXT)]
listTXTf <- listTXT[substr(listTXT, 2, 2)=="F" | listTXT %in% confF]
confR <- listTXT[grepl("Hirsch|Schwalbe|Pferd", listTXT) & grepl("alone|joint", listTXT) & substr(listTXT, 2, 2) == "-"]
confRA <- c("H-Hirsch_alone.txt", "H-Pferd_alone.txt", "H-Schwalbe_alone.txt", "L-Hirsch_alone.txt", "L-Pferd_alone.txt", "L-Schwalbe_alone.txt", "S-Hirsch_alone.txt", "S-Pferd_alone.txt", "S-Schwalbe_alone.txt")
confRJ <- c("H-Hirsch_joint.txt", "H-Pferd_joint.txt", "H-Schwalbe_joint.txt", "L-Hirsch_joint.txt", "L-Pferd_joint.txt", "L-Schwalbe_joint.txt", "S-Hirsch_joint.txt", "S-Pferd_joint.txt", "S-Schwalbe_joint.txt")
f <- c("BF", "HF", "LF", "SF")
readB <- listTXT[substr(listTXT, 1, 2) == "BR"]
partR <- list.files(folder, pattern=".txt")
partR <- partR[substr(partR, 2, 2) != "-" & grepl("alone|joint", partR)]

listTG <- list.files(folder, pattern=".TextGrid")
listTG <- listTG[!grepl("_VUV", listTG) & !grepl("HSP", listTG)]
confFtg <- listTG[grepl("Home|Hobbies|Holidays", listTG)]
listTGf <- listTG[substr(listTG, 2, 2)=="F" | listTG %in% confFtg]
listTGBsil <- listTG[substr(listTG, 1, 2) == "BR"] # baseline reading
listTGBsil <- listTGBsil[grepl("Hirsch|Pferd|Schwalbe", listTGBsil)]
listTGRsil0 <- listTG[grepl("joint|alone", listTG)] # conditions joint and alone reading, participants
listTGRsil <- listTGRsil0[substr(listTGRsil0, 2, 2) != "-"]
listCRsil <- listTGRsil0[substr(listTGRsil0, 2, 2) == "-"]


listTxg <- as.data.frame(cbind(listTXTf, listTGf))
colnames(listTxg) <- c("txt", "tg")
listTxg$worked[substr(listTxg$txt, 1, 6)==substr(listTxg$tg, 1, 6)] <- "worked!"
listTxg$worked[substr(listTxg$txt, 1, 6)!=substr(listTxg$tg, 1, 6)] <- "NO!!!!!!!!!"
unique(listTxg$worked) # make sure all the TXT and Textgrid files are matching in each row

{listReadBase <- data.frame(cbind(readB, listTGBsil))
  names(listReadBase) <- c("txt", "sil")
  listReadBase$worked[substr(listReadBase$txt, 1, 20)==substr(listReadBase$sil, 1, 20)] <- "worked!"
  listReadBase$worked[substr(listReadBase$txt, 1, 20)!=substr(listReadBase$sil, 1, 20)] <- "NO!!!!!!!!!"}
table(listReadBase$worked)

{listReadC <- data.frame(cbind(confR, listCRsil))
  names(listReadC) <- c("txt", "sil")
  ifelse(substr(listReadC$txt, 1, 6) == substr(listReadC$sil, 1, 6),
         listReadC$worked <- "worked!",
         listReadC$worked <- "NO!!!!!!!!")}
table(listReadC$worked)

{listReadCond0 <- data.frame(cbind(partR, listTGRsil))
  names(listReadCond0) <- c("txt", "sil")
  ifelse(substr(listReadCond0$txt, 1, 6) == substr(listReadCond0$sil, 1, 6),
         listReadCond0$worked <- "worked!",
         listReadCond0$worked <- "NO!!!!!!!!")}
table(listReadCond0$worked)

listReadCond <- rbind(listReadCond0, listReadC)

ff <- data.frame(matrix(ncol=4, nrow=0))
colnames(ff) <- c("IPU", "f0raw", "file", "label")

for(i in 1:nrow(listTxg)){
  txt <- read.table(paste0(folder, listTxg$txt[i]), header=TRUE, na.strings = "--undefined--") %>%  # load file with f0 means
    mutate_at("f0med", as.numeric) %>% 
    mutate(f0z = (f0med - mean(f0med, na.rm=TRUE))/sd(f0med, na.rm=TRUE),
           f0med = ifelse(abs(f0z) > 2.5, NA, f0med))
  tg <- tg.read(paste0(folder, listTxg$tg[i]), encoding=detectEncoding(paste0(folder, listTxg$tg[i]))) # load textgrid with defined IPUs
  
  # create object with the index of each IPU and its onset and offset time
  IPUtimes <- data.frame(matrix(ncol=4, nrow=0))
  colnames(IPUtimes) <- c("IPU", "label", "start", "end")
  for(n in 1:tg.getNumberOfIntervals(tg, 1)){
    if(listTxg$txt[i] %!in% confF){
      if(tg.getIntervalStartTime(tg, 1, n) >= tg.getIntervalStartTime(tg, 2, as.numeric(tg.findLabels(tg, 2, "task"))) && tg.getIntervalEndTime(tg, 1, n) <= tg.getIntervalEndTime(tg, 2, as.numeric(tg.findLabels(tg, 2, "task")))){
        IPUtimes[nrow(IPUtimes)+1,] <- c(n,
                                         tg.getLabel(tg, 1, n),
                                         as.numeric(tg.getIntervalStartTime(tg, 1, n)),
                                         as.numeric(tg.getIntervalEndTime(tg, 1, n)))
      } 
    } else if(listTxg$txt[i] %in% confF){
      IPUtimes[nrow(IPUtimes)+1,] <- c(n,
                                       tg.getLabel(tg, 1, n),
                                       as.numeric(tg.getIntervalStartTime(tg, 1, n)),
                                       as.numeric(tg.getIntervalEndTime(tg, 1, n)))
    }
  }
  
  IPUtimes <- IPUtimes %>%
    filter(label != "") %>%
    filter(label != "<usb>")
  IPUtimes$IPU <- 1:nrow(IPUtimes)
  
  # get f0 for each IPU, i.e. the mean of all f0 values within the period of each IPU
  IPUtimes[, c("start", "end")] <- lapply(IPUtimes[, c("start", "end")], as.numeric)
  f0 <- data.frame(matrix(ncol=2, nrow=0))
  colnames(f0) <- c("IPU", "f0raw")
  for(p in 1:nrow(IPUtimes)){
    for(n in 1:nrow(txt)){
      if(txt$onset[n] >= IPUtimes$start[p] && txt$offset[n] <= IPUtimes$end[p]){
        f0[nrow(f0)+1,] <- c(IPUtimes$IPU[p], as.numeric(txt$f0med[n]))
      }
    }
  }
  f0 <- f0 %>%
    filter(!is.na(f0raw)) %>%
    mutate_at(c("IPU", "f0raw"), as.numeric)
  f0perIPU <- aggregate(f0$f0raw, list(f0$IPU), FUN=mean)
  colnames(f0perIPU) <- c("IPU", "f0raw")
  f0perIPU <- f0perIPU %>%
    mutate(file = substr(listTxg$txt[i], 1, 6))
  f <- merge(f0perIPU, IPUtimes %>% select(IPU, label), by="IPU")
  ff <- rbind(ff, f)
}

ff <- ff %>% select(file, IPU, f0raw, label)

# for(i in unique(ff$file)){
#   plot(ff$f0raw[ff$file==i])
#   readline("Continue")
# }

{fr <- data.frame(matrix(ncol=4, nrow=0))
  colnames(fr) <- c("IPU", "f0raw", "file", "Task")}

{# not working when trying to remove boundaries of adjacent intervals with same label: rows 5:7, 10, 12, 16, 20 (from listReadBase)
  
  # compare number of intervals of all files to see if that explains it?
  # {d <- data.frame(matrix(ncol=4, nrow=0))
  # names(d) <- c("file", "numberInt", "encoding", "worked")
  # for(i in 1:nrow(listReadBase)){
  #   sil <- tg.read(paste0(folder, listReadBase$sil[i]), encoding=detectEncoding(paste0(folder, listReadBase$sil[i])))
  #   if(i %in% c(5:7, 10, 12, 16, 20)){
  #     d[nrow(d)+1,] <- c(substr(listReadBase$sil[i], 1, 6), tg.getNumberOfIntervals(sil, 1), 1, "no")
  #   } else{
  #     d[nrow(d)+1,] <- c(substr(listReadBase$sil[i], 1, 6), tg.getNumberOfIntervals(sil, 1), 1, "yes")
  #   }
  # }
  # d <- d %>% mutate_at(c("file", "worked"), as.factor) %>% mutate_at("numberInt", as.numeric)
  # View(d)}
}

# i=5

for(i in 1:nrow(listReadBase)){
  txt <- read.table(paste0(folder, listReadBase$txt[i]), header=TRUE, na.strings = "--undefined--") %>% 
    mutate_at("f0med", as.numeric) %>% 
    mutate(f0z = (f0med - mean(f0med, na.rm=TRUE))/sd(f0med, na.rm=TRUE),
           f0med = ifelse(abs(f0z) > 2.5, NA, f0med))
  sil <- tg.read(paste0(folder, listReadBase$sil[i]), encoding=detectEncoding(paste0(folder, listReadBase$sil[i])))
  
  if(grepl("Hirsch", listReadBase$txt[i])){
    text <- "-Hirsch"
  } else if(grepl("Schwalbe", listReadBase$txt[i])){
    text <- "-Schwalbe"
  } else if(grepl("Pferd", listReadBase$txt[i])){
    text <- "-Pferd"
  }
  
  # get mean f0 for each non-silent period ("IPU")
  f0 <- data.frame(matrix(ncol=4, nrow=0))
  colnames(f0) <- c("IPU", "f0raw", "onset", "offset")
  for(p in 1:tg.getNumberOfIntervals(sil, 1)){
    for(n in 1:nrow(txt)){
      if(txt$onset[n] >= tg.getIntervalStartTime(sil, 1, p) && txt$offset[n] <= tg.getIntervalEndTime(sil, 1, p) && tg.getLabel(sil, 1, p) == ""){
        f0[nrow(f0)+1,] <- c(p,
                             as.numeric(txt$f0med[n]),
                             as.numeric(tg.getIntervalStartTime(sil, 1, p)),
                             as.numeric(tg.getIntervalEndTime(sil, 1, p)))
      }
    }
  }
  timings <- f0 %>% select(-f0raw)
  f0 <- f0 %>%
    filter(!is.na(f0raw)) %>%
    mutate_at(c("IPU", "f0raw", "onset", "offset"), as.numeric) %>% 
    group_by(IPU) %>% 
    summarize(f0raw = mean(f0raw)) %>%
    ungroup()
  f0 <- merge(timings, f0, by="IPU") %>%
    filter(!duplicated(IPU)) %>%
    mutate(IPU = 1:nrow(f0),
           file = paste0(substr(listReadBase$txt[i], 1, 6), text),
           Task = "ReadBaseline")
  
  fr <- rbind(fr, f0)
}

for(i in 1:nrow(listReadCond)){
  txt <- read.table(paste0(folder, listReadCond$txt[i]), header=TRUE, na.strings = "--undefined--") %>% 
    mutate_at("f0med", as.numeric) %>% 
    mutate(f0z = (f0med - mean(f0med, na.rm=TRUE))/sd(f0med, na.rm=TRUE),
           f0med = ifelse(abs(f0z) > 2.5, NA, f0med))
  sil <- tg.read(paste0(folder, listReadCond$sil[i]), encoding=detectEncoding(paste0(folder, listReadCond$sil[i])))
  
  if(grepl("alone", listReadCond$txt[i])){
    task <- "ReadAlone"
  } else{task <- "ReadJoint"}
  
  if(substr(listReadCond$txt[i], 2, 2) == "-"){ # the confederate only does ReadAlone
    task <- "ReadAlone"
  }
  
  # get mean f0 for each non-silent period ("IPU")
  f0 <- data.frame(matrix(ncol=4, nrow=0))
  colnames(f0) <- c("IPU", "f0raw", "onset", "offset")
  for(p in 1:tg.getNumberOfIntervals(sil, 1)){
    for(n in 1:nrow(txt)){
      if(txt$onset[n] >= tg.getIntervalStartTime(sil, 1, p) && txt$offset[n] <= tg.getIntervalEndTime(sil, 1, p) && tg.getLabel(sil, 1, p) == ""){
        f0[nrow(f0)+1,] <- c(p,
                             as.numeric(txt$f0med[n]),
                             as.numeric(tg.getIntervalStartTime(sil, 1, p)),
                             as.numeric(tg.getIntervalEndTime(sil, 1, p)))
      }
    }
  }
  timings <- f0 %>% select(-f0raw)
  f0 <- f0 %>%
    filter(!is.na(f0raw)) %>%
    mutate_at(c("IPU", "f0raw", "onset", "offset"), as.numeric) %>% 
    group_by(IPU) %>% 
    summarize(f0raw = mean(f0raw)) %>%
    ungroup()
  f0 <- merge(timings, f0, by="IPU") %>%
    filter(!duplicated(IPU)) %>%
    mutate(IPU = 1:nrow(f0),
           file = substr(listReadCond$txt[i], 1, 6),
           Task = task)
  
  fr <- rbind(fr, f0)
}

# fr <- fr %>%
#   mutate(Task = ifelse(substr(file, 1, 1) != "B", "ReadJoint", ifelse(grepl("Schwalbe", file), "ReadBaseline-Schwalbe", ifelse(grepl("Hirsch", file), "ReadBaseline-Hirsch", "ReadBaseline-Pferd"))))


# for (i in listTXTr){
#   txt <- read.table(paste0(folder, i), header=TRUE)
#   txt$f0med <- as.numeric(txt$f0med)
#   txt$zmean <- (txt$f0med - mean(txt$f0med))/sd(txt$f0med) # get z scores
#   txt <- txt[txt$zmean < 3,] # keep values below 3 SDs (there don't seem to be any low outliers, only high ones)
#   if (i %in% confRA){ # confederate's reading files, alone section
#     i <- paste0(substr(i, 1, 1), "A", substr(i, 3, 6))
#   } else if (i %in% readA){ # participants' reading files, alone section
#     i <- paste0(substr(i, 1, 2), "A", substr(i, 4, 6))
#   } else if (i %in% confRJ){ # confederate's reading files, joint section
#     i <- paste0(substr(i, 1, 1), "J", substr(i, 3, 6))
#   } else if (i %in% readJ){ # participants' reading files, joint section
#     i <- paste0(substr(i, 1, 2), "J", substr(i, 4, 6))
#   } else if(substr(i, 17, 21)=="PFERD"){
#     i <- paste0(substr(i, 1, 2), "P", substr(i, 4, 6))
#   } else if(substr(i, 17, 22)=="HIRSCH"){
#     i <- paste0(substr(i, 1, 2), "H", substr(i, 4, 6))
#   } else if(substr(i, 17, 24)=="SCHWALBE"){
#     i <- paste0(substr(i, 1, 2), "S", substr(i, 4, 6))
#   }
#   ff[nrow(ff)+1,] <- c(i, NA, mean(txt$f0med), mean(txt$f0med))
# }
# 
# ff$file <- as.factor(ff$file)
# ff$IPU <- as.factor(ff$IPU)
# ff$f0raw <- as.numeric(ff$f0raw)

##################################################################

#### BREATHING DATA

# number of textgrid files == number of wav files: [H,L,S][B, F]

folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/PeaksValleys/"

# Participants: 

# listBREATHall <- list.files(folder2, pattern="SUM")
# listWAVpf <- listBREATHall[grepl("wav", listBREATHall) & !grepl("TextGrid", listBREATHall) & substr(listBREATHall, 2, 2)=="F" & !grepl("breathL", listBREATHall)]
# listBREATH <- listBREATHall[grepl("TextGrid", listBREATHall)]
# listBREATHr <- listBREATH[substr(listBREATH, 2, 2)=="R"]
# listBREATHrj <- listBREATHr[grepl("joint|alone", listBREATHr) | substr(listBREATHr, 1, 2) == "BR"] # read joint AND baseline reading
# listBREATHf <- listBREATH[substr(listBREATH, 2, 2)=="F" & !grepl("breathL", listBREATH)]
# listBREATHl <- listBREATH[grepl("breathL", listBREATH)] # breathing during listening
# listBREATHb <- listBREATH[substr(listBREATH, 2, 2) == "B"] # breathing during baseline period (i.e. just watching the confedearate sitting/biking in silence)
# listBREATHlb <- c(listBREATHl, listBREATHb)
# listBREATHlb <- listBREATHlb[grepl("TextGrid", listBREATHlb)]
# listWAVlb <- listBREATHall[(grepl("breathL", listBREATHall) & !grepl("TextGrid", listBREATHall)) | (substr(listBREATHall, 2, 2) == "B" & !grepl("TextGrid", listBREATHall))]
# listWAVlb <- listWAVlb[substr(listWAVlb, 1, 6) %in% substr(listBREATHlb, 1, 6)]
# listBREATHlb <- listBREATHlb[substr(listBREATHlb, 1, 6) %in% substr(listWAVlb, 1, 6)]
# listWAVrj0 <- listBREATHall[grepl("joint|alone", listBREATHall) & !grepl("TextGrid", listBREATHall)]
# listWAVbr0 <- listBREATHall[grepl("wav", listBREATHall) & !grepl("TextGrid", listBREATHall) & substr(listBREATHall, 1, 2) == "BR"]
# listWAVbr <- listWAVbr0[grepl("Schwalbe", listWAVbr0) | grepl("Hirsch", listWAVbr0) | grepl("Pferd", listWAVbr0)]
# listWAVrj <- c(listWAVbr, listWAVrj0)
# listWAVrj <- listWAVrj[substr(listWAVrj, 1, 6) %in% substr(listBREATHrj, 1, 6)]
# 
# listTG <- list.files(folder, pattern=".TextGrid") # textgrids with transcriptions and IPU timing
# listTGf <- listTG[substr(listTG, 2, 2)=="F"]
# 
# listBREATHlbw <- data.frame(cbind(sort(listBREATHlb), sort(listWAVlb)))
# colnames(listBREATHlbw) <- c("breath", "wav")
# listBREATHlbw$workedwav[substr(listBREATHlbw$breath, 1, 6)==substr(listBREATHlbw$wav, 1, 6)] <- "worked!"
# listBREATHlbw$workedwav[substr(listBREATHlbw$breath, 1, 6)!=substr(listBREATHlbw$wav, 1, 6)] <- "NO!!!!!!!!!"
# table(listBREATHlbw$workedwav)
# 
# listTGf <- listTGf[substr(listTGf, 1, 6) %in% substr(listBREATHf, 1, 6)]
# listBREATHf <- listBREATHf[substr(listBREATHf, 1, 6) %in% substr(listTGf, 1, 6)]
# 
# listPBGf0 <- as.data.frame(cbind(listBREATHf, listTGf))
# listPBGf <- as.data.frame(cbind(listPBGf0, listWAVpf))
# colnames(listPBGf) <- c("breath", "tg", "wav")
# listPBGf$workedTGs[substr(listPBGf$breath, 1, 6)==substr(listPBGf$tg, 1, 6)] <- "worked!"
# listPBGf$workedTGs[substr(listPBGf$breath, 1, 6)!=substr(listPBGf$tg, 1, 6)] <- "NO!!!!!!!!!"
# listPBGf$workedwav[substr(listPBGf$breath, 1, 6)==substr(listPBGf$wav, 1, 6)] <- "worked!"
# listPBGf$workedwav[substr(listPBGf$breath, 1, 6)!=substr(listPBGf$wav, 1, 6)] <- "NO!!!!!!!!!"
# table(listPBGf$workedTGs) # make sure all the TXT and Textgrid files are matching in each row
# table(listPBGf$workedwav)
# 
# listREAD0 <- data.frame(cbind(listBREATHrj, listWAVrj))
# colnames(listREAD0) <- c("breath", "wav")
# listREAD0$worked[substr(listREAD0$breath, 1, 6)==substr(listREAD0$wav, 1, 6)] <- "worked!"
# listREAD0$worked[substr(listREAD0$breath, 1, 6)!=substr(listREAD0$wav, 1, 6)] <- "NO!!!!!!!!!"
# table(listREAD0$worked) # make sure all the TXT and Textgrid files are matching in each row
# 
# # confederate:
# 
# listCB <- list.files(folder2, pattern="THORAX") # we use the measurement from the thorax only for the confederate
# listCB <- listCB[grepl(".TextGrid", listCB)]
# listCBr <- listCB[grepl("joint|alone", listCB)]
# listCBf <- listCB[grepl("Holidays", listCB)|grepl("Hobbies", listCB)|grepl("Home", listCB)]
# 
# listCT <- list.files(folder, pattern="TextGrid")
# listCT <- listCT[!grepl("THORAX", listCT)]
# listCTr <- listCT[grepl("-Schwalbe", listCT)|grepl("-Hirsch", listCT)|grepl("-Pferd", listCT)]
# listCTf <- listCT[grepl("Holidays", listCT)|grepl("Hobbies", listCT)|grepl("Home", listCT)]
# 
# listCW <- list.files(folder2, pattern="THORAX")
# listCW <- listCW[!grepl("TextGrid", listCW)]
# listCWf <- listCW[grepl("Hobbies|Holidays|Home", listCW)]
# listCWr <- listCW[grepl("joint|alone", listCW)]
# 
# listCBGf0 <- as.data.frame(cbind(listCBf, listCTf))
# listCBGf <- as.data.frame(cbind(listCBGf0, listCWf))
# colnames(listCBGf) <- c("breath", "tg", "wav")
# listCBGf$workedTGs[substr(listCBGf$breath, 1, 6)==substr(listCBGf$tg, 1, 6)] <- "worked!"
# listCBGf$workedTGs[substr(listCBGf$breath, 1, 6)!=substr(listCBGf$tg, 1, 6)] <- "NO!!!!!!!!!"
# listCBGf$workedwav[substr(listCBGf$breath, 1, 6)==substr(listCBGf$wav, 1, 6)] <- "worked!"
# listCBGf$workedwav[substr(listCBGf$breath, 1, 6)!=substr(listCBGf$wav, 1, 6)] <- "NO!!!!!!!!!"
# table(listCBGf$workedTGs) # make sure all the TXT and Textgrid files are matching in each row
# table(listCBGf$workedwav)
# 
# listBGf <- rbind(listPBGf, listCBGf)
# listBREATHo <- c(listBREATHf, listBREATHl, listBREATHb, listCBf, listBREATHr, listCBr)
# 
# {listREADc <- data.frame(cbind(listCBr, listCWr))
#   names(listREADc) <- c("breath", "wav")
#   listREADc <- listREADc %>% 
#     mutate(worked = ifelse(substr(listREADc$breath, 1, 6) == substr(listREADc$wav, 1, 6), "worked!", "NO!!!!!"))}
# table(listREADc$worked)
# 
# listREAD <- rbind(listREAD0, listREADc)
# 
# ### GETTING DURATION OF BREATHING CYCLES AND BREATHING RATE
# 
# # first, check if number of valleys == number of peaks + 1 in each file
# 
# PV <- data.frame(matrix(ncol=3, nrow=0))
# names(PV) <- c("file", "peaks", "valleys")
# 
# IPUandCycles <- data.frame(matrix(nrow=0, ncol=3))
# names(IPUandCycles) <- c("IPU", "file", "breathCycleDur")
# 
# for(i in listBREATHo){
#   breath <- tg.read(paste0(folder2, i))
#   PV[nrow(PV)+1,] <- c(i, tg.getNumberOfPoints(breath, 1), tg.getNumberOfPoints(breath, 2))
# }
# PV$peaks <- as.numeric(PV$peaks)
# PV$valleys <- as.numeric(PV$valleys)
# pointsok <- PV$file[PV$peaks == (PV$valleys - 1) | PV$file == "SF-TKJ013_SUM_200_breathL_100_wav.TextGrid"]  # for some reason SF-TKJ_breathL is read as if having 76 peaks, but in reality it only has 75 (which you can confirm by annotating the times of all the peaks and seeing that length(unique(peaks))==75)
# PV$file[PV$file %!in% pointsok] # the output here should be zero
# 
# listBGf <- listBGf %>% filter(breath %in% pointsok) # make sure to get only the textgrids with one more valley than peaks (each cycle is from valley to valley)
# listBREATHlb <- listBREATHlb[listBREATHlb %in% pointsok]
# listBREATHo <- listBREATHo[listBREATHo %in% pointsok]

# pbr1 <- data.frame(matrix(ncol=13, nrow=0))
# colnames(pbr1) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "inhalAmp", "inhalDur", "numberIPUs")
# 
# durationsOK <- data.frame(matrix(ncol=2, nrow=0))
# names(durationsOK) <- c("file", "sameDurations")

# i=23

# PVcount <- data.frame(matrix(ncol=3, nrow=0))
# names(PVcount) <- c("file", "peaks", "valleys")

# for(i in 1:nrow(listBGf)){
#   act <- "speaking"
#   breath <- tg.read(paste0(folder2, listBGf$breath[i]))
#   tg <- tg.read(paste0(folder, listBGf$tg[i]), encoding=detectEncoding(paste0(folder, listBGf$tg[i])))
#   b <- (w <- readWave(paste0(folder2, listBGf$wav[i])))@left
#   b <- (b - min(b)) / (max(b) - min(b))
#   
#   PVcount[nrow(PVcount)+1,] <- c(i, tg.getNumberOfPoints(breath, 1), tg.getNumberOfPoints(breath, 2))
#   
#   ifelse(length(w@left)/w@samp.rate==tg.getTotalDuration(breath), # keep a file to make sure that all durations of wave and textgrid are the same
#          durationsOK[nrow(durationsOK)+1,] <- c(substr(listBGf$breath[i], 1, 6), "OK!"),
#          durationsOK[nrow(durationsOK)+1,] <- c(substr(listBGf$breath[i], 1, 6), "NO!!!!!!!"))
#   
#   # get onset and offset of each interval (IPU)
#   IPUtimes <- data.frame(matrix(ncol=4, nrow=0))
#   colnames(IPUtimes) <- c("IPU", "label", "start", "end")
#   for(n in 1:tg.getNumberOfIntervals(tg, 1)){
#     IPUtimes[nrow(IPUtimes)+1,] <- c(n,
#                                      tg.getLabel(tg, 1, n),
#                                      tg.getIntervalStartTime(tg, 1, n),
#                                      tg.getIntervalEndTime(tg, 1, n))
#   }
#   IPUtimes <- IPUtimes %>%
#     filter(label!="") #%>% # exclude silences
#   # filter(start >= tg.getIntervalStartTime(tg, 2, as.numeric(tg.findLabels(tg, 2, "task")))) # exclude everything before the "task" section ---- UPDATE: no need to do this because we'll only get the IPUs within the breathing cycles, and those are already correct
#   IPUtimes$IPU <- paste0("IPU", 1:nrow(IPUtimes))
#   
#   # get time of each point (peaks and valleys) and inhalation amplitude and duration for each cycle
#   PVtimes <- data.frame(matrix(ncol=12, nrow=0))
#   colnames(PVtimes) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "inhalAmp", "inhalDur")
#   for(t in 1:(tg.getNumberOfPoints(breath, 2)-1)){ # number of valleys minus one (the last valley is only counted as offset of its previous cycle)
#     if(listBGf$breath[i] %in% listBREATHf){ # for participants: to the time of each peak/valley, add the time between beginning of tg and the interval called "breathS"
#       PVtimes[nrow(PVtimes)+1,] <- c(substr(listBGf$breath[i], 1, 6),
#                                      act,
#                                      paste0("cycle", t), # number of breathing cycle
#                                      as.numeric(tg.getPointTime(breath, 2, t)) + as.numeric(tg.getIntervalStartTime(tg, 3, as.numeric(tg.findLabels(tg, 3, "breathS")))),
#                                      as.numeric(tg.getPointTime(breath, 1, t)) + as.numeric(tg.getIntervalStartTime(tg, 3, as.numeric(tg.findLabels(tg, 3, "breathS")))),
#                                      as.numeric(tg.getPointTime(breath, 2, t+1)) + as.numeric(tg.getIntervalStartTime(tg, 3, as.numeric(tg.findLabels(tg, 3, "breathS")))), # next valley
#                                      NA, NA, NA, NA,
#                                      as.numeric(b[tg.getPointTime(breath, 1, t)*w@samp.rate] - b[tg.getPointTime(breath, 2, t)*w@samp.rate]), # inhalation amplitude
#                                      NA)
#     } else if(listBGf$breath[i] %in% listCBf){ # confederate files: the audio files are already aligned to the breathing files, so no need to add anything to the times of the peaks and valleys
#       PVtimes[nrow(PVtimes)+1,] <- c(substr(listBGf$breath[i], 1, 6),
#                                      act,
#                                      paste0("cycle", t), # number of breathing cycle
#                                      as.numeric(tg.getPointTime(breath, 2, t)),
#                                      as.numeric(tg.getPointTime(breath, 1, t)),
#                                      as.numeric(tg.getPointTime(breath, 2, t+1)), # next valley
#                                      NA, NA, NA, NA,
#                                      as.numeric(b[tg.getPointTime(breath, 1, t)*w@samp.rate] - b[tg.getPointTime(breath, 2, t)*w@samp.rate]), # inhalation amplitude
#                                      NA)
#     }
#     
#   }
#   PVtimes[, c("onset", "peak", "offset")] <- lapply(PVtimes[, c("onset", "peak", "offset")], as.numeric)
#   PVtimes$cycleOK[PVtimes$onset < PVtimes$peak & PVtimes$peak < PVtimes$offset] <- "OK!"
#   PVtimes$cycleOK[PVtimes$cycleOK=="NA"] <- "Not OK!"
#   table(PVtimes$cycleOK) # make sure that onset < peak < offset (i.e., the cycles make sense)
#   
#   PVtimes$cycleDur <- PVtimes$offset - PVtimes$onset # duration of each cycle
#   PVtimes$numberBreathCycles <- nrow(PVtimes)
#   PVtimes$breathCycleDurMean <- mean(PVtimes$cycleDur)
#   PVtimes$breathRate <- (nrow(PVtimes) / ((PVtimes$offset[nrow(PVtimes)] - PVtimes$onset[1]) / 60)) # breathing rate = number of cycles / time from first to last valley divided by 60 (to turn into minutes)
#   PVtimes$inhalDur <- PVtimes$peak - PVtimes$onset
#   
#   # get number of IPUs within each cycle
#   ic <- data.frame(matrix(ncol=2, nrow=0))
#   names(ic) <- c("cycle", "IPU")
#   PVtimes <- PVtimes %>%
#     mutate_at(c("onset", "offset"), as.numeric) %>%
#     mutate_at("breathCycle", as.factor)
#   IPUtimes <- IPUtimes %>%
#     mutate_at("IPU", as.factor) %>%
#     mutate_at(c("start", "end"), as.numeric)
#   
#   for(t in 1:nrow(PVtimes)){
#     for(n in 1:nrow(IPUtimes)){
#       if(IPUtimes$start[n] >= PVtimes$onset[t] && IPUtimes$start[n] <= PVtimes$offset[t]){ # start of IPU before the OFFSET of the cycle: because they were annotated manually and sometimes the END of the IPU is marked as AFTER the offset of the cycle
#         ic[nrow(ic)+1,] <- c(PVtimes$breathCycle[t], IPUtimes$IPU[n])
#       }
#     }
#   }
#   ic$IPU <- paste0("IPU", 1:nrow(ic)) # not sure why I wrote this here... Aren't the IPU names already right?
#   k <- data.frame(table(ic$cycle)) # get number of IPUs per cycle
#   
#   for(l in 1:nrow(PVtimes)){
#     for(j in 1:nrow(k)){
#       if(PVtimes$breathCycle[l] == k$Var1[j]){
#         PVtimes$numberIPUs[l] <- k$Freq[j]
#       }
#     }
#   }
#   
#   PVtimes <- PVtimes %>% select(-c("cycleOK"))
#   
#   pbr1 <- rbind(pbr1, PVtimes)
#   
#   # save the length of breath cycle in which each IPU is inserted -- join this with speech dataset
#   ic <- ic %>%
#     mutate(file = substr(listBGf$breath[i], 1, 6)) %>%
#     rename(breathCycle = cycle)
#   tojoin <- PVtimes %>% select(breathCycle, cycleDur)
#   ic <- merge(ic, tojoin, by="breathCycle")
#   ic <- ic %>%
#     rename(breathCycleDur = cycleDur) %>%
#     select(-"breathCycle")
#   
#   IPUandCycles <- rbind(IPUandCycles, ic)
# }

# table(durationsOK$sameDurations)
# 
# IPUandCycles$IPU <- gsub("IPU", "", IPUandCycles$IPU)
# 
# pbr2 <- data.frame(matrix(ncol=12, nrow=0))
# colnames(pbr2) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "inhalAmp", "inhalDur")
# 
# PVcount <- data.frame(matrix(ncol=3, nrow=0))
# names(PVcount) <- c("file", "peaks", "valleys")

# i=4

# for(i in 1:nrow(listBREATHlbw)){ # list with the listening part of the free spech files and with the "watching" files (where the participants just watched Carry with everyone in silence)
#   if(listBREATHlbw$breath[i] %in% listBREATHb){
#     act <- "watching"
#   } else if(listBREATHlbw$breath[i] %in% listBREATHl){
#     act <- "listening"
#   }
#   
#   breath <- tg.read(paste0(folder2, listBREATHlbw$breath[i]))
#   b <- (w <- readWave(paste0(folder2, listBREATHlbw$wav[i])))@left
#   b <- (b - min(b)) / (max(b) - min(b))
#   
#   PVcount[nrow(PVcount)+1,] <- c(i, tg.getNumberOfPoints(breath, 1), tg.getNumberOfPoints(breath, 2))
#   
#   # get time of each point (peaks and valleys)
#   PVtimes <- data.frame(matrix(ncol=12, nrow=0))
#   colnames(PVtimes) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "inhalAmp", "inhalDur")
#   for(t in 1:(tg.getNumberOfPoints(breath, 2)-1)){ # number of valleys minus one (the last valley is only counted as offset of its previous cycle)
#     PVtimes[nrow(PVtimes)+1,] <- c(substr(listBREATHlbw$breath[i], 1, 6),
#                                    act,
#                                    paste0("cycle", t), # number of breathing cycle
#                                    as.numeric(tg.getPointTime(breath, 2, t)),
#                                    as.numeric(tg.getPointTime(breath, 1, t)),
#                                    as.numeric(tg.getPointTime(breath, 2, t+1)), # next valley
#                                    NA, NA, NA, NA,
#                                    as.numeric(b[tg.getPointTime(breath, 1, t)*w@samp.rate] - b[tg.getPointTime(breath, 2, t)*w@samp.rate]), # inhalation amplitude
#                                    NA)
#   }
#   PVtimes[, c("onset", "peak", "offset")] <- lapply(PVtimes[, c("onset", "peak", "offset")], as.numeric)
#   PVtimes$cycleOK[PVtimes$onset < PVtimes$peak & PVtimes$peak < PVtimes$offset] <- "OK!"
#   PVtimes$cycleOK[PVtimes$cycleOK=="NA"] <- "Not OK!"
#   table(PVtimes$cycleOK) # make sure that onset < peak < offset (i.e., the cycles make sense)
#   
#   PVtimes$cycleDur <- PVtimes$offset - PVtimes$onset # duration of each cycle
#   PVtimes$numberBreathCycles <- nrow(PVtimes)
#   PVtimes$breathCycleDurMean <- mean(PVtimes$cycleDur)
#   PVtimes$breathRate <- (nrow(PVtimes) / ((PVtimes$offset[nrow(PVtimes)] - PVtimes$onset[1]) / 60)) # breathing rate = number of cycles / time from first to last valley divided by 60 (to turn into minutes)
#   PVtimes$inhalDur <- PVtimes$peak - PVtimes$onset
#   
#   PVtimes <- PVtimes %>% select(-c("cycleOK"))
#   
#   pbr2 <- rbind(pbr2, PVtimes)
# }
# 
# pbr2$numberIPUs <- NA
# 
# 
# br0 <- rbind(pbr1, pbr2)

# save(br, file=paste0(folder, "BreathingData.RData"))

### Now do the same for reading files
# 
# pbr3 <- data.frame(matrix(ncol=12, nrow=0))
# colnames(pbr3) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "inhalAmp", "inhalDur")
# 
# PVcount <- data.frame(matrix(ncol=3, nrow=0))
# names(PVcount) <- c("file", "peaks", "valleys")
# 
# # for(i in 1:nrow(listREAD)){
# #   if(grepl("_Hirsch", listREAD$breath[i])){
# #     act <- "ReadBaseline-Hirsch"
# #   } else if(grepl("_Pferd", listREAD$breath[i])){
# #     act <- "ReadBaseline-Pferd"
# #   } else if(grepl("_Schwalbe", listREAD$breath[i])){
# #     act <- "ReadBaseline-Schwalbe"
# #   } else if(grepl("joint", listREAD$breath[i])){
# #     act <- "ReadJoint"
# #   } else if(grepl("alone", listREAD$breath[i])){
# #     act <- "ReadAlone"
# #   }
# #   if(substr(listREAD$breath[i], 2, 2) == "-"){
# #     act <- "ReadAlone"
# #   }
# #   
# #   breath <- tg.read(paste0(folder2, listREAD$breath[i]))
# #   b <- (w <- readWave(paste0(folder2, listREAD$wav[i])))@left
# #   b <- (b - min(b)) / (max(b) - min(b))
# #   
# #   PVcount[nrow(PVcount)+1,] <- c(i, tg.getNumberOfPoints(breath, 1), tg.getNumberOfPoints(breath, 2))
# #   
# #   # get time of each point (peaks and valleys)
# #   PVtimes <- data.frame(matrix(ncol=12, nrow=0))
# #   colnames(PVtimes) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "inhalAmp", "inhalDur")
# #   for(t in 1:(tg.getNumberOfPoints(breath, 2)-1)){ # number of valleys minus one (the last valley is only counted as offset of its previous cycle)
# #     PVtimes[nrow(PVtimes)+1,] <- c(substr(listREAD$breath[i], 1, 6),
# #                                    act,
# #                                    paste0("cycle", t), # number of breathing cycle
# #                                    as.numeric(tg.getPointTime(breath, 2, t)),
# #                                    as.numeric(tg.getPointTime(breath, 1, t)),
# #                                    as.numeric(tg.getPointTime(breath, 2, t+1)), # next valley
# #                                    NA, NA, NA, NA,
# #                                    as.numeric(b[tg.getPointTime(breath, 1, t)*w@samp.rate] - b[tg.getPointTime(breath, 2, t)*w@samp.rate]), # inhalation amplitude
# #                                    NA)
# #   }
# #   PVtimes[, c("onset", "peak", "offset")] <- lapply(PVtimes[, c("onset", "peak", "offset")], as.numeric)
# #   PVtimes$cycleOK[PVtimes$onset < PVtimes$peak & PVtimes$peak < PVtimes$offset] <- "OK!"
# #   PVtimes$cycleOK[PVtimes$cycleOK=="NA"] <- "Not OK!"
# #   table(PVtimes$cycleOK) # make sure that onset < peak < offset (i.e., the cycles make sense)
# #   
# #   PVtimes$cycleDur <- PVtimes$offset - PVtimes$onset # duration of each cycle
# #   PVtimes$numberBreathCycles <- nrow(PVtimes)
# #   PVtimes$breathCycleDurMean <- mean(PVtimes$cycleDur)
# #   PVtimes$breathRate <- (nrow(PVtimes) / ((PVtimes$offset[nrow(PVtimes)] - PVtimes$onset[1]) / 60)) # breathing rate = number of cycles / time from first to last valley divided by 60 (to turn into minutes)
# #   PVtimes$inhalDur <- PVtimes$peak - PVtimes$onset
# #   
# #   PVtimes <- PVtimes %>% select(-c("cycleOK"))
# #   
# #   pbr3 <- rbind(pbr3, PVtimes)
# # }
# 
# pbr3$numberIPUs <- NA
# 
# br <- rbind(br0, pbr3)
# 
# # br <- br %>%
# #   mutate(file = ifelse(grepl("Schwalbe", act), paste0(file, "-Schwalbe"),
# #                        ifelse(grepl("Hirsch", act), paste0(file, "-Hirsch"),
# #                               ifelse(grepl("Pferd", act), paste0(file, "-Pferd"),
# #                                      file))),
# #          act = ifelse(grepl("Baseline", act), "ReadBaseline", act))

####### check the inahaltions that are negative in amplitude or duration (this is caused by mistakes in the textgrid)
# v <- pbr3 %>%
#   filter(inhalAmp <= 0 | inhalDur <= 0) %>%
#   # include the name of the text in "file", so you can easily see which files specifically are wrong
#   mutate(file = ifelse(grepl("Schwalbe", act), paste0(file, "-Schwalbe"), ifelse(grepl("Hirsch", act), paste0(file, "-Hirsch"), paste0(file, "-Pferd"))))
# 
# unique(v$file)
####### done

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

# 3

ff$IPU <- as.factor(ff$IPU)
# srf$IPU <- as.factor(srf$IPU)

# fs0 <- full_join(ff, srf, by=c("file", "IPU"), all=TRUE)

# fs <- full_join(ff, IPUandCycles, by=c("file", "IPU"), all=TRUE)
fs <- ff

{# # I wanted to include the number of "IPUs" per breath cycle, but the code below takes _really_ long, and I don't think it works right.
  # # Now that I think of it, maybe this metric doesn't even make sense.
  # fr$breathCycle <- NA
  # brToJoin <- br %>% filter(grepl("Read", act))
  # brToJoin$breathCycle <- as.character(brToJoin$breathCycle)
  # for(r in 1:nrow(fr)){
  #   for(b in 1:nrow(brToJoin)){
  #     if(fr$file == brToJoin$file && fr$onset[r] >= brToJoin$onset[b] && fr$offset[r] <= brToJoin$offset[b]){
  #       fr$breathCycle[r] <- brToJoin$breathCycle[b]
  #     }
  #   }
  # }
}

frs <- fr %>% select(-IPU)
# srr <- srr %>% select(-IPU)

# frs <- full_join(fr, srr, by=c("file", "onset", "offset"), all=TRUE)

frs <- frs %>% 
  group_by(file) %>% 
  arrange(onset) %>% 
  mutate(IPU = 1:n()) %>% 
  ungroup()

# frb <- merge(frs, br %>% select(file, breathRate), by="file") %>% 
  # distinct() # for some reason this merge() is duplicating a bunch of rows, so here we delete the duplicate rows

frb <- frs

# frb <- frb %>% 
#   mutate(Task = ifelse(grepl("Baseline", Task), "ReadBaseline", Task))

conffiles <- c("irs", "obb", "oli", "ome", "fer", "chw")

d <- list(fs, frb)

for(i in 1:length(d)){
  d[[i]]$Speaker <- substr(d[[i]]$file, 4, 6)
  d[[i]]$Speaker[d[[i]]$Speaker %in% conffiles] <- "Confederate"
  
  d[[i]]$Condition <- substr(d[[i]]$file, 1, 1)
  d[[i]]$Condition[d[[i]]$Condition=="B"] <- "Baseline"
  d[[i]]$Condition[d[[i]]$Condition=="H"] <- "Heavy"
  d[[i]]$Condition[d[[i]]$Condition=="L"] <- "Light"
  d[[i]]$Condition[d[[i]]$Condition=="S"] <- "Sitting"
  d[[i]]$Condition <- as.factor(d[[i]]$Condition)
  
  d[[i]]$Task[substr(d[[i]]$file, 2, 2) == "F"] <- "Free"
  # d[[i]]$Task[substr(d[[i]]$file, 2, 3) == "RA"] <- "ReadAlone"
  # d[[i]]$Task[substr(d[[i]]$file, 2, 3) == "RJ"] <- "ReadJoint"
  # d[[i]]$Task[substr(d[[i]]$file, 1, 2) == "BR"] <- "ReadAlone" # baseline reading is also alone
  d[[i]]$Task[d[[i]]$Speaker == "Confederate" & substr(d[[i]]$file, 2, 2) == "A"] <- "ReadAlone"
  d[[i]]$Task[d[[i]]$Speaker == "Confederate" & substr(d[[i]]$file, 2, 2) == "J"] <- "ReadJoint"
  # d[[i]]$Task[d[[i]]$Speaker == "Confederate" & grepl("Hirs|Schw|Pfer", d[[i]]$file)] <- "ReadJoint"
  d[[i]]$Task[d[[i]]$Speaker == "Confederate" & grepl("Hobb|Holi|Home", d[[i]]$file)] <- "Free"
  # d[[i]]$Task[d[[i]]$act == "ReadJoint"] <- "ReadJoint"
  # d[[i]]$Task[d[[i]]$act == "ReadAlone"] <- "ReadAlone"
  # d[[i]]$Task[d[[i]]$act == "ReadBaseline"] <- "ReadBaseline"
  d[[i]]$Task <- as.factor(d[[i]]$Task)
  
}

fs <- d[[1]]
# br <- d[[2]]
frb <- d[[2]]

meta <- read.csv("C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/metadata.csv", fileEncoding="UTF-8-BOM")
names(meta)[names(meta) == "Participant"] <- "Speaker"

fsm <- merge(fs, meta, by="Speaker", all=TRUE)
# brm <- merge(br, meta, by="Speaker", all=TRUE)
frm <- merge(frb, meta, by="Speaker", all=TRUE)

dat <- list(fsm, frm)

for(i in 1:length(dat)){ # since we have one dataset with breathing info and one with speech info, do all the same naming of conditions etc for each
  dat[[i]]$Order[dat[[i]]$Condition=="Baseline"] <- 0
  
  dat[[i]]$Order[dat[[i]]$List==1 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="ReadAlone"] <- 1
  dat[[i]]$Order[dat[[i]]$List==1 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="ReadJoint"] <- 2
  dat[[i]]$Order[dat[[i]]$List==1 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="Free"] <- 3
  dat[[i]]$Order[dat[[i]]$List==1 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="Free"] <- 4
  dat[[i]]$Order[dat[[i]]$List==1 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="ReadAlone"] <- 5
  dat[[i]]$Order[dat[[i]]$List==1 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="ReadJoint"] <- 6
  dat[[i]]$Order[dat[[i]]$List==1 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="Free"] <- 7
  dat[[i]]$Order[dat[[i]]$List==1 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="ReadAlone"] <- 8
  dat[[i]]$Order[dat[[i]]$List==1 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="ReadJoint"] <- 9
  
  dat[[i]]$Order[dat[[i]]$List==2 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="ReadAlone"] <- 1
  dat[[i]]$Order[dat[[i]]$List==2 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="ReadJoint"] <- 2
  dat[[i]]$Order[dat[[i]]$List==2 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="Free"] <- 3
  dat[[i]]$Order[dat[[i]]$List==2 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="Free"] <- 4
  dat[[i]]$Order[dat[[i]]$List==2 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="ReadAlone"] <- 5
  dat[[i]]$Order[dat[[i]]$List==2 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="ReadJoint"] <- 6
  dat[[i]]$Order[dat[[i]]$List==2 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="Free"] <- 7
  dat[[i]]$Order[dat[[i]]$List==2 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="ReadAlone"] <- 8
  dat[[i]]$Order[dat[[i]]$List==2 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="ReadJoint"] <- 9
  
  dat[[i]]$Order[dat[[i]]$List==3 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="ReadAlone"] <- 1
  dat[[i]]$Order[dat[[i]]$List==3 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="ReadJoint"] <- 2
  dat[[i]]$Order[dat[[i]]$List==3 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="Free"] <- 3
  dat[[i]]$Order[dat[[i]]$List==3 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="ReadAlone"] <- 4
  dat[[i]]$Order[dat[[i]]$List==3 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="ReadJoint"] <- 5
  dat[[i]]$Order[dat[[i]]$List==3 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="Free"] <- 6
  dat[[i]]$Order[dat[[i]]$List==3 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="ReadAlone"] <- 7
  dat[[i]]$Order[dat[[i]]$List==3 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="ReadJoint"] <- 8
  dat[[i]]$Order[dat[[i]]$List==3 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="Free"] <- 9
  
  dat[[i]]$Order[dat[[i]]$List==4 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="ReadAlone"] <- 1
  dat[[i]]$Order[dat[[i]]$List==4 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="ReadJoint"] <- 2
  dat[[i]]$Order[dat[[i]]$List==4 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="Free"] <- 3
  dat[[i]]$Order[dat[[i]]$List==4 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="Free"] <- 4
  dat[[i]]$Order[dat[[i]]$List==4 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="ReadAlone"] <- 5
  dat[[i]]$Order[dat[[i]]$List==4 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="ReadJoint"] <- 6
  dat[[i]]$Order[dat[[i]]$List==4 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="ReadAlone"] <- 7
  dat[[i]]$Order[dat[[i]]$List==4 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="ReadJoint"] <- 8
  dat[[i]]$Order[dat[[i]]$List==4 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="Free"] <- 9
  
  dat[[i]]$Order[dat[[i]]$List==5 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="ReadAlone"] <- 1
  dat[[i]]$Order[dat[[i]]$List==5 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="ReadJoint"] <- 2
  dat[[i]]$Order[dat[[i]]$List==5 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="Free"] <- 3
  dat[[i]]$Order[dat[[i]]$List==5 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="Free"] <- 4
  dat[[i]]$Order[dat[[i]]$List==5 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="ReadAlone"] <- 5
  dat[[i]]$Order[dat[[i]]$List==5 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="ReadJoint"] <- 6
  dat[[i]]$Order[dat[[i]]$List==5 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="ReadAlone"] <- 7
  dat[[i]]$Order[dat[[i]]$List==5 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="ReadJoint"] <- 8
  dat[[i]]$Order[dat[[i]]$List==5 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="Free"] <- 9
  
  dat[[i]]$Order[dat[[i]]$List==6 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="ReadAlone"] <- 1
  dat[[i]]$Order[dat[[i]]$List==6 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="ReadJoint"] <- 2
  dat[[i]]$Order[dat[[i]]$List==6 & dat[[i]]$Condition=="Heavy" & dat[[i]]$Task=="Free"] <- 3
  dat[[i]]$Order[dat[[i]]$List==6 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="ReadAlone"] <- 4
  dat[[i]]$Order[dat[[i]]$List==6 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="ReadJoint"] <- 5
  dat[[i]]$Order[dat[[i]]$List==6 & dat[[i]]$Condition=="Light" & dat[[i]]$Task=="Free"] <- 6
  dat[[i]]$Order[dat[[i]]$List==6 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="Free"] <- 7
  dat[[i]]$Order[dat[[i]]$List==6 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="ReadAlone"] <- 8
  dat[[i]]$Order[dat[[i]]$List==6 & dat[[i]]$Condition=="Sitting" & dat[[i]]$Task=="ReadJoint"] <- 9
  
  read <- c("ReadAlone", "ReadJoint")
  
  dat[[i]]$Topic[dat[[i]]$Condition == "Baseline" & dat[[i]]$Task == "Free"] <- "Food"
  
  dat[[i]]$Topic[dat[[i]]$List == 1 & dat[[i]]$Condition == "Sitting" & dat[[i]]$Task %in% read] <- "Hirsch"
  dat[[i]]$Topic[dat[[i]]$List == 1 & dat[[i]]$Condition == "Sitting" & dat[[i]]$Task == "Free"] <- "Holidays"
  dat[[i]]$Topic[dat[[i]]$List == 1 & dat[[i]]$Condition == "Light" & dat[[i]]$Task %in% read] <- "Schwalbe"
  dat[[i]]$Topic[dat[[i]]$List == 1 & dat[[i]]$Condition == "Light" & dat[[i]]$Task == "Free"] <- "Hobbies"
  dat[[i]]$Topic[dat[[i]]$List == 1 & dat[[i]]$Condition == "Heavy" & dat[[i]]$Task %in% read] <- "Pferd"
  dat[[i]]$Topic[dat[[i]]$List == 1 & dat[[i]]$Condition == "Heavy" & dat[[i]]$Task == "Free"] <- "Home"
  
  dat[[i]]$Topic[dat[[i]]$List == 2 & dat[[i]]$Condition == "Heavy" & dat[[i]]$Task %in% read] <- "Schwalbe"
  dat[[i]]$Topic[dat[[i]]$List == 2 & dat[[i]]$Condition == "Heavy" & dat[[i]]$Task == "Free"] <- "Home"
  dat[[i]]$Topic[dat[[i]]$List == 2 & dat[[i]]$Condition == "Sitting" & dat[[i]]$Task %in% read] <- "Pferd"
  dat[[i]]$Topic[dat[[i]]$List == 2 & dat[[i]]$Condition == "Sitting" & dat[[i]]$Task == "Free"] <- "Holidays"
  dat[[i]]$Topic[dat[[i]]$List == 2 & dat[[i]]$Condition == "Light" & dat[[i]]$Task %in% read] <- "Hirsch"
  dat[[i]]$Topic[dat[[i]]$List == 2 & dat[[i]]$Condition == "Light" & dat[[i]]$Task == "Free"] <- "Hobbies"
  
  dat[[i]]$Topic[dat[[i]]$List == 3 & dat[[i]]$Condition == "Light" & dat[[i]]$Task %in% read] <- "Pferd"
  dat[[i]]$Topic[dat[[i]]$List == 3 & dat[[i]]$Condition == "Light" & dat[[i]]$Task == "Free"] <- "Hobbies"
  dat[[i]]$Topic[dat[[i]]$List == 3 & dat[[i]]$Condition == "Heavy" & dat[[i]]$Task %in% read] <- "Hirsch"
  dat[[i]]$Topic[dat[[i]]$List == 3 & dat[[i]]$Condition == "Heavy" & dat[[i]]$Task == "Free"] <- "Home"
  dat[[i]]$Topic[dat[[i]]$List == 3 & dat[[i]]$Condition == "Sitting" & dat[[i]]$Task %in% read] <- "Schwalbe"
  dat[[i]]$Topic[dat[[i]]$List == 3 & dat[[i]]$Condition == "Sitting" & dat[[i]]$Task == "Free"] <- "Holidays"
  
  dat[[i]]$Topic[dat[[i]]$List == 4 & dat[[i]]$Condition == "Sitting" & dat[[i]]$Task %in% read] <- "Hirsch"
  dat[[i]]$Topic[dat[[i]]$List == 4 & dat[[i]]$Condition == "Sitting" & dat[[i]]$Task == "Free"] <- "Home"
  dat[[i]]$Topic[dat[[i]]$List == 4 & dat[[i]]$Condition == "Heavy" & dat[[i]]$Task %in% read] <- "Schwalbe"
  dat[[i]]$Topic[dat[[i]]$List == 4 & dat[[i]]$Condition == "Heavy" & dat[[i]]$Task == "Free"] <- "Hobbies"
  dat[[i]]$Topic[dat[[i]]$List == 4 & dat[[i]]$Condition == "Light" & dat[[i]]$Task %in% read] <- "Pferd"
  dat[[i]]$Topic[dat[[i]]$List == 4 & dat[[i]]$Condition == "Light" & dat[[i]]$Task == "Free"] <- "Holidays"
  
  dat[[i]]$Topic[dat[[i]]$List == 5 & dat[[i]]$Condition == "Light" & dat[[i]]$Task %in% read] <- "Schwalbe"
  dat[[i]]$Topic[dat[[i]]$List == 5 & dat[[i]]$Condition == "Light" & dat[[i]]$Task == "Free"] <- "Hobbies"
  dat[[i]]$Topic[dat[[i]]$List == 5 & dat[[i]]$Condition == "Sitting" & dat[[i]]$Task %in% read] <- "Hirsch"
  dat[[i]]$Topic[dat[[i]]$List == 5 & dat[[i]]$Condition == "Sitting" & dat[[i]]$Task == "Free"] <- "Home"
  dat[[i]]$Topic[dat[[i]]$List == 5 & dat[[i]]$Condition == "Heavy" & dat[[i]]$Task %in% read] <- "Pferd"
  dat[[i]]$Topic[dat[[i]]$List == 5 & dat[[i]]$Condition == "Heavy" & dat[[i]]$Task == "Free"] <- "Holidays"
  
  dat[[i]]$Topic[dat[[i]]$List == 6 & dat[[i]]$Condition == "Heavy" & dat[[i]]$Task %in% read] <- "Pferd"
  dat[[i]]$Topic[dat[[i]]$List == 6 & dat[[i]]$Condition == "Heavy" & dat[[i]]$Task == "Free"] <- "Home"
  dat[[i]]$Topic[dat[[i]]$List == 6 & dat[[i]]$Condition == "Light" & dat[[i]]$Task %in% read] <- "Hirsch"
  dat[[i]]$Topic[dat[[i]]$List == 6 & dat[[i]]$Condition == "Light" & dat[[i]]$Task == "Free"] <- "Holidays"
  dat[[i]]$Topic[dat[[i]]$List == 6 & dat[[i]]$Condition == "Sitting" & dat[[i]]$Task %in% read] <- "Schwalbe"
  dat[[i]]$Topic[dat[[i]]$List == 6 & dat[[i]]$Condition == "Sitting" & dat[[i]]$Task == "Free"] <- "Hobbies"
  
  dat[[i]]$Topic[substr(dat[[i]]$file, 1, 3) == "BRP"] <- "Pferd"
  dat[[i]]$Topic[substr(dat[[i]]$file, 1, 3) == "BRH"] <- "Hirsch"
  dat[[i]]$Topic[substr(dat[[i]]$file, 1, 3) == "BRS"] <- "Schwalbe"
  
  dat[[i]]$Topic[dat[[i]]$Speaker == "Confederate" & substr(dat[[i]]$file, 3, 6) == "Hobb"] <- "Hobbies"
  dat[[i]]$Topic[dat[[i]]$Speaker == "Confederate" & substr(dat[[i]]$file, 3, 6) == "Schw"] <- "Schwalbe"
  dat[[i]]$Topic[dat[[i]]$Speaker == "Confederate" & substr(dat[[i]]$file, 3, 6) == "Pfer"] <- "Pferd"
  dat[[i]]$Topic[dat[[i]]$Speaker == "Confederate" & substr(dat[[i]]$file, 3, 6) == "Home"] <- "Home"
  dat[[i]]$Topic[dat[[i]]$Speaker == "Confederate" & substr(dat[[i]]$file, 3, 6) == "Hirs"] <- "Hirsch"
  dat[[i]]$Topic[dat[[i]]$Speaker == "Confederate" & substr(dat[[i]]$file, 3, 6) == "Holi"] <- "Holidays"
  
  dat[[i]] <- dat[[i]] %>% 
    mutate(Topic = ifelse(grepl("Schwalbe", file), "Schwalbe",
                          ifelse(grepl("Pferd", file), "Pferd",
                                 ifelse(grepl("Hirsch", file), "Hirsch",
                                        Topic))))
  
  ### Calculate BMI
  
  dat[[i]]$BMI <- dat[[i]]$Weight/((dat[[i]]$Height/100)^2) # dividing height by 100 because it has to be in meters
  
  ### Calculate gender scores
  
  # inverted items in GEPAQ: F2, M4:
  inv <- c("GEPAQ.F2", "GEPAQ.M4")
  dat[[i]][, inv] <- (max(dat[[i]][,inv]) + 1) - dat[[i]][, inv]
  
  # participant CBE didn't answer GEPAQ.M5, so it's NA. I'll get the mean of the other GEPAQ.M answers of this participant and use that as GEPAQ.M5
  dat[[i]]$GEPAQ.M5[dat[[i]]$Participant=="CBE"] <- (dat[[i]]$GEPAQ.M1[dat[[i]]$Participant=="CBE"] + dat[[i]]$GEPAQ.M2[dat[[i]]$Participant=="CBE"] + dat[[i]]$GEPAQ.M3[dat[[i]]$Participant=="CBE"] + dat[[i]]$GEPAQ.M4[dat[[i]]$Participant=="CBE"] + dat[[i]]$GEPAQ.M6[dat[[i]]$Participant=="CBE"] + dat[[i]]$GEPAQ.M7[dat[[i]]$Participant=="CBE"] + dat[[i]]$GEPAQ.M8[dat[[i]]$Participant=="CBE"]) / 7
  
  # mean of all answers to each subscale
  dat[[i]]$GEPAQ.F <- (dat[[i]]$GEPAQ.F1 + dat[[i]]$GEPAQ.F2 + dat[[i]]$GEPAQ.F3 + dat[[i]]$GEPAQ.F4 + dat[[i]]$GEPAQ.F5 + dat[[i]]$GEPAQ.F6 + dat[[i]]$GEPAQ.F7 + dat[[i]]$GEPAQ.F8) / 8
  dat[[i]]$GEPAQ.M <- (dat[[i]]$GEPAQ.M1 + dat[[i]]$GEPAQ.M2 + dat[[i]]$GEPAQ.M3 + dat[[i]]$GEPAQ.M4 + dat[[i]]$GEPAQ.M5 + dat[[i]]$GEPAQ.M6 + dat[[i]]$GEPAQ.M7 + dat[[i]]$GEPAQ.M8) / 8
  dat[[i]]$TMF.F <- (dat[[i]]$TMF.F1 + dat[[i]]$TMF.F2 + dat[[i]]$TMF.F3 + dat[[i]]$TMF.F4 + dat[[i]]$TMF.F5 + dat[[i]]$TMF.F6) / 6
  dat[[i]]$TMF.M <- (dat[[i]]$TMF.M1 + dat[[i]]$TMF.M2 + dat[[i]]$TMF.M3 + dat[[i]]$TMF.M4 + dat[[i]]$TMF.M5 + dat[[i]]$TMF.M6) / 6
  
  dat[[i]] <- dat[[i]] %>% select(-c(GEPAQ.M1:TMF.F6)) # get rid of the individual items
  
  # ##### Save dataset without calculating differences:
  dat[[i]]$Order <- as.integer(dat[[i]]$Order)
  dat[[i]]$List <- as.factor(dat[[i]]$List)
  dat[[i]]$Age <- as.numeric(dat[[i]]$Age)
  dat[[i]]$Height <- as.numeric(dat[[i]]$Height)
  dat[[i]]$Gender <- as.factor(dat[[i]]$Gender)
  dat[[i]]$Topic <- as.factor(dat[[i]]$Topic)
  dat[[i]]$Role[dat[[i]]$Speaker != "Confederate"] <- "Participant"
  dat[[i]]$Role[dat[[i]]$Speaker == "Confederate"] <- "Confederate"
  dat[[i]]$Role <- as.factor(dat[[i]]$Role)
}

fsm <- dat[[1]] %>% 
  mutate_at(c("Task", "Condition", "Speaker"), as.factor) %>% 
  mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy")),
         Cond2 = ifelse(Condition == "Baseline", "Baseline", "Interaction")) %>% 
  distinct()
# brm <- dat[[2]] %>% 
#   mutate_at(c("Task", "act", "Condition", "Speaker"), as.factor) %>% 
#   mutate_at(c("inhalDur", "inhalAmp"), as.numeric) %>% 
#   mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy")),
#          across(act, factor, levels=c("watching","listening", "speaking")),
#          Cond2 = ifelse(Condition == "Baseline", "Baseline", "Interaction")) %>% 
#   distinct()
frb <- dat[[2]] %>% 
  mutate_at(c("Task", "Condition", "Speaker"), as.factor) %>% 
  mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy")),
         Cond2 = ifelse(Condition == "Baseline", "Baseline", "Interaction")) %>%
  distinct()

# fsm <- merge(fsm, brm %>% select(c(file, breathCycleDurMean, breathRate, inhalDur, inhalAmp)) %>% filter(!duplicated(file)) %>% filter(substr(file, 2, 2) != "B"), by="file")

# # in SF-WJH f0IPUmean is NA, so fix that:
# unique(fsm$f0IPUmean[fsm$file=="SF-WJH"])[!is.na(unique(fsm$f0IPUmean[fsm$file=="SF-WJH"]))==TRUE]
# fsm$f0IPUmean[fsm$file=="SF-WJH"] <- unique(fsm$f0IPUmean[fsm$file=="SF-WJH"])[!is.na(unique(fsm$f0IPUmean[fsm$file=="SF-WJH"]))==TRUE]

frbMed <- frb
fsmMed <- fsm

save(fsmMed, file=paste0(folder, "DataSpeech-Median.RData"))
# save(brm, file=paste0(folder, "DataBreathing.RData"))
save(frbMed, file=paste0(folder, "DataReadSpeech-Median.RData"))

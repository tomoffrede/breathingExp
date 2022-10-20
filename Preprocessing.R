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
### 2. Read the TXT files, remove the outliers (> 3 SDs) and annotate the mean of the f0raw of each file and mean of the f0median of each file.
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

# free speech files (BF, HF, LF, SF) have transcriptions annotations, so we can determine the speech rate of each IPU
# read files (BR, HR, LR, SR) don't have the transcriptions: we can only have one speech rate value per file
# (however, we do have textgrids with silence annotations -- extracted automatically. so we could pretend that those are IPUs)

listTG <- list.files(folder, pattern=".TextGrid")
listTG <- listTG[!grepl("VUV", listTG)]
listCTGf <- listTG[grepl("Holidays|Hobbies|Home", listTG)]
listCTGr <- listTG[substr(listTG, 2, 2) == "-" & grepl("Hirsch|Pferd|Schwalbe", listTG) & grepl("alone|joint", listTG)]
listPTGf <- listTG[substr(listTG, 2, 2)=="F"]
listPTGr0 <- listTG[substr(listTG, 2, 2)=="R"]
listPTGrb <- listPTGr0[substr(listPTGr0, 1, 1)=="B" & grepl("Hirsch|Pferd|Schwalbe", listPTGr0)]
listPTGrnb <- listPTGr0[grepl("joint", listPTGr0)]
listPTGr <- c(listPTGrb, listPTGrnb)
listTGf <- c(listPTGf, listCTGf)
listTGr <- c(listPTGr, listCTGr)

listCSV <- list.files(folder, ".csv")
listCCSVf <- listCSV[grepl("Holidays|Hobbies|Home", listCSV)]
listCCSVr <- listCSV[substr(listCSV, 2, 2) == "-" & grepl("Hirsch|Pferd|Schwalbe", listCSV) & grepl("alone|joint", listCSV)]
listPCSVf <- listCSV[substr(listCSV, 2, 2)=="F"]
listPCSVr0 <- listCSV[substr(listCSV, 2, 2)=="R"]
listPCSVrb <- listPCSVr0[substr(listPCSVr0, 1, 1)=="B" & grepl("Hirsch|Pferd|Schwalbe", listPCSVr0)]
listPCSVrnb <- listPCSVr0[grepl("joint", listPCSVr0)]
listPCSVr <- c(listPCSVrb, listPCSVrnb)
listCSVf <- c(listPCSVf, listCCSVf)
listCSVr <- c(listPCSVr, listCCSVr)

listF <- data.frame(cbind(listCSVf, listTGf)) %>% 
  mutate(worked = ifelse(substr(listCSVf, 1, 6) == substr(listTGf, 1, 6), "worked!", "NO!!!!!!!!!!!"))
table(listF$worked)
  
listR <- data.frame(cbind(listCSVr, listTGr)) %>% 
  mutate(worked = ifelse(substr(listCSVr, 1, 6) == substr(listTGr, 1, 6), "worked!", "NO!!!!!!!!!!!"))
table(listR$worked)

for(i in listF){
  amp <- read.csv(paste0(folder, listF$listCSVf[i]))
  t <- tg.read(paste0(folder, listF$listTGf[i]), encoding=detectEncoding(paste0(folder, listF$listTGf[i])))
  
  # start <- round_any(as.numeric(tg.getIntervalStartTime(t, 2, as.numeric(tg.findLabels(t, 2, "task")))), 10, ceiling) * 1000 # in ms
  # end <- round_any(as.numeric(tg.getIntervalEndTime(t, 2, as.numeric(tg.findLabels(t, 2, "task")))), 10, floor) * 1000
  # dur <- (end-start)/1000 #turn to seconds
  # durNoPauses <- dur - pauseDur$dur[substr(pauseDur$file, 1, 6) == substr(listF$csv[i], 1, 6)]
  
  # amp <- amp[amp$time_ms >= start & amp$time_ms <= end,]
  
  amp$env <- (amp$env - min(amp$env)) / (max(amp$env) - min(amp$env))
  p <- findpeaks(amp$env, minpeakheight = 0.025, minpeakdistance = 22050/15) # minpeakdistance: sampling frequency divided by a very high potential speech rate
  
  ampr[nrow(ampr)+1,] <- c(substr(listF$csv[i], 1, 6), # file name
                           as.numeric(nrow(p)/(dur)), # speech rate (number of peaks divided by entire duration)
                           as.numeric(nrow(p)/(durNoPauses))) # articulation rate: number of peaks divided by (duration minus duration of pauses)
}




ld <- data.frame(matrix(ncol=8, nrow=0))
names(ld) <- c("IPUDur", "IPU", "file", "syll", "speechRateSyll", "durSpeech", "pause", "pauseDurManual")

for(i in listTGf){
  tg <- tg.read(paste0(folder, i), encoding=detectEncoding(paste0(folder, i)))
  inter <- data.frame(matrix(ncol=2, nrow=0))
  names(inter) <- c("label", "duration")
  for(n in 1:tg.getNumberOfIntervals(tg, 1)){
    if(i %in% listPTG){
    if(tg.getIntervalStartTime(tg, 1, n) >= tg.getIntervalStartTime(tg, 2, as.numeric(tg.findLabels(tg, 2, "task"))) & tg.getIntervalEndTime(tg, 1, n) <= tg.getIntervalEndTime(tg, 2, as.numeric(tg.findLabels(tg, 2, "task")))){
      inter[nrow(inter)+1,] <- c(tg.getLabel(tg, 1, n), as.numeric(tg.getIntervalDuration(tg, 1, n)))
      }
    } else if(i %in% listCTG){ # the confederate files are already cut to the right time, so I don't have to select the IPU timings
      inter[nrow(inter)+1,] <- c(tg.getLabel(tg, 1, n), as.numeric(tg.getIntervalDuration(tg, 1, n)))
    }
  }
  inter$label[inter$label %in% c(" ", "  ", "   ", "    ", "\t")] <- ""
  ipu <- inter %>% filter(label != "")
  ipu <- ipu %>%
    filter(label != "<usb>") %>%
    rename("IPUDur"="duration")
  ipu$IPU <- 1:nrow(ipu)
  ipu$file <- substr(i, 1, 6)
  ipu$syll <- (hyphen(ipu$label, hyph.pattern="de")@hyphen)$syll
  ipu$speechRateIPU <- ipu$syll / as.numeric(ipu$IPUDur)
  if(i %in% listPTG){
    ipu$durSpeech = as.numeric(tg.getIntervalDuration(tg, 2, as.numeric(tg.findLabels(tg, 2, "task"))))
  } else if(i %in% listCTG){
    ipu$durSpeech = as.numeric(tg.getIntervalEndTime(tg, 1, as.numeric(tg.findLabels(tg, 1, ipu$label[nrow(ipu)]))[[length(tg.findLabels(tg, 1, ipu$label[nrow(ipu)]))]])) - as.numeric(tg.getIntervalStartTime(tg, 1, as.numeric(tg.findLabels(tg, 1, ipu$label[1]))[[1]]))
  }
  
  ipu$label <- NULL
  paus <- inter %>%
    filter(label == "") %>%
    rename("pause"="label",
           "pauseDurManual" = "duration") %>%
    mutate(pause = 1:nrow(inter %>% filter(label == "")))
  if(i %in% listPTG){
    paus[nrow(paus)+1,] <- NA # because for the participants' files there will always be one less pause than IPUs, and we need the same number of rows
  } else if(i %in% listCTG){
    paus <- paus[-1,] # in the participants' files, the first interval is speech, not a pause, so let's delete this "pause" from the confederate's file
    paus[nrow(paus),] <- NA # turn this the same as the participants' files
  }
  
  ip <- cbind(ipu, paus)
  ld <- rbind(ld, ip)
}

syllables <- aggregate(ld$syll, list(ld$file), FUN=sum)
names(syllables) <- c("file", "syll")
ld$pauseDurManual <- as.numeric(ld$pauseDurManual)
ld1 <- ld[!is.na(ld$pauseDurManual),]
pauseDur <- aggregate(ld1$pauseDurManual, list(ld1$file), FUN=sum)
names(pauseDur) <- c("file", "durPauses")
c <- ld[, c(2,3,5,6)]
c1 <- merge(c, pauseDur, by="file")
sr <- merge(syllables, c1, by="file")
sr$speechRate <- sr$syll/sr$durSpeech
sr$articRate <- sr$syll/(sr$durSpeech - sr$durPauses)
sr$syll <- NULL

sr[, c("file", "IPU")] <- lapply(sr[, c("file", "IPU")], as.factor)

# NOTE: speechRateIPU is the speech rate calculated individually for each IPU. each IPU has a different speechRateIPU.
# articRate is the speech rate calculated over the entire speaking time minus duration of pauses. each file has only one articRate value.

############# f0 mean, median

# 2

listTXT <- list.files(folder, pattern=".txt")
confF <- listTXT[grepl("Home", listTXT)|grepl("Hobbies", listTXT)|grepl("Holidays", listTXT)]
listTXTf <- listTXT[substr(listTXT, 2, 2)=="F" | listTXT %in% confF]
confRA <- c("H-Hirsch_alone.txt", "H-Pferd_alone.txt", "H-Schwalbe_alone.txt", "L-Hirsch_alone.txt", "L-Pferd_alone.txt", "L-Schwalbe_alone.txt", "S-Hirsch_alone.txt", "S-Pferd_alone.txt", "S-Schwalbe_alone.txt")
confRJ <- c("H-Hirsch_joint.txt", "H-Pferd_joint.txt", "H-Schwalbe_joint.txt", "L-Hirsch_joint.txt", "L-Pferd_joint.txt", "L-Schwalbe_joint.txt", "S-Hirsch_joint.txt", "S-Pferd_joint.txt", "S-Schwalbe_joint.txt")
f <- c("BF", "HF", "LF", "SF")
readB <- listTXT[substr(listTXT, 1, 2) == "BR"]
readJ <- list.files(folder, pattern="joint.txt")
readJ <- readJ[substr(readJ, 2, 2) != "-"]
readA <- list.files(folder, pattern="alone.txt")
readA <- readA[substr(readA, 2, 2) != "-"]

listTG <- list.files(folder, pattern=".TextGrid")
listTG <- listTG[!grepl("_VUV", listTG) & !grepl("HSP", listTG)]
confFtg <- listTG[grepl("Home", listTG)|grepl("Hobbies", listTG)|grepl("Holidays", listTG)]
listTGf <- listTG[substr(listTG, 2, 2)=="F" | listTG %in% confFtg]
listTGBsil <- listTG[substr(listTG, 1, 2) == "BR"] # baseline reading
listTGRsil0 <- listTG[grepl("joint", listTG)] # conditions joint reading, participants
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

{listReadC <- data.frame(cbind(confRJ, listCRsil))
names(listReadC) <- c("txt", "sil")
ifelse(substr(listReadC$txt, 1, 6) == substr(listReadC$sil, 1, 6),
       listReadC$worked <- "worked!",
       listReadC$worked <- "NO!!!!!!!!")}
table(listReadC$worked)

{listReadCond0 <- data.frame(cbind(readJ, listTGRsil))
names(listReadCond0) <- c("txt", "sil")
ifelse(substr(listReadCond0$txt, 1, 6) == substr(listReadCond0$sil, 1, 6),
       listReadCond0$worked <- "worked!",
       listReadCond0$worked <- "NO!!!!!!!!")}
table(listReadCond0$worked)

listReadCond <- rbind(listReadCond0, listReadC)

ff <- data.frame(matrix(ncol=4, nrow=0))
colnames(ff) <- c("IPU", "f0raw", "file", "label")

for(i in 1:nrow(listTxg)){
  txt <- read.table(paste0(folder, listTxg$txt[i]), header=TRUE, na.strings = "--undefined--") # load file with f0 means
  txt$f0mean <- as.numeric(txt$f0mean)
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
        f0[nrow(f0)+1,] <- c(IPUtimes$IPU[p], as.numeric(txt$f0mean[n]))
      }
    }
  }
  f0 <- f0 %>%
    filter(!is.na(f0raw)) %>%
    mutate_at(c("IPU", "f0raw"), as.numeric)
  f0perIPU <- aggregate(f0$f0raw, list(f0$IPU), FUN=mean)
  colnames(f0perIPU) <- c("IPU", "f0raw")
  f0perIPU <- f0perIPU %>%
    mutate(file = substr(listTxg$txt[i], 1, 6),
           f0z = (f0raw - mean(f0raw))/sd(f0raw)) %>%
    mutate(f0raw = ifelse(abs(f0z) > 2, NA, f0raw)) %>% # I don't want to use the IPUs with outlier f0raw, but I don't want to completely delete those IPUs from the dataset because they'll still be joined with the speech rate information. So just turn them into NA here.
    mutate(f0IPUmean = mean(f0raw, na.rm=TRUE)) %>% 
    select(-f0z)
  f <- merge(f0perIPU, IPUtimes %>% select(IPU, label), by="IPU")
  ff <- rbind(ff, f)
}

ff <- ff %>% select(file, IPU, f0raw, f0IPUmean, label)

# for(i in unique(ff$file)){
#   plot(ff$f0raw[ff$file==i])
#   readline("Continue")
# }

{fr <- data.frame(matrix(ncol=3, nrow=0))
colnames(fr) <- c("IPU", "f0raw", "file")}
 
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
  txt <- read.table(paste0(folder, listReadBase$txt[i]), header=TRUE, na.strings = "--undefined--")
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
                             as.numeric(txt$f0mean[n]),
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
           f0z = (f0raw - mean(f0raw))/sd(f0raw)) %>%
    filter(abs(f0z) < 2) %>% 
    select(-f0z)
  
  fr <- rbind(fr, f0)
}

for(i in 1:nrow(listReadCond)){
  txt <- read.table(paste0(folder, listReadCond$txt[i]), header=TRUE, na.strings = "--undefined--")
  sil <- tg.read(paste0(folder, listReadCond$sil[i]), encoding=detectEncoding(paste0(folder, listReadCond$sil[i])))
  
  # get mean f0 for each non-silent period ("IPU")
  f0 <- data.frame(matrix(ncol=4, nrow=0))
  colnames(f0) <- c("IPU", "f0raw", "onset", "offset")
    for(p in 1:tg.getNumberOfIntervals(sil, 1)){
      for(n in 1:nrow(txt)){
        if(txt$onset[n] >= tg.getIntervalStartTime(sil, 1, p) && txt$offset[n] <= tg.getIntervalEndTime(sil, 1, p) && tg.getLabel(sil, 1, p) == ""){
          f0[nrow(f0)+1,] <- c(p,
                               as.numeric(txt$f0mean[n]),
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
             f0z = (f0raw - mean(f0raw))/sd(f0raw)) %>%
      filter(abs(f0z) < 2) %>% 
      select(-f0z)
  
  fr <- rbind(fr, f0)
}

fr <- fr %>% mutate(Task = ifelse(substr(file, 1, 1) != "B", "ReadJoint", ifelse(grepl("Schwalbe", file), "ReadBaseline-Schwalbe", ifelse(grepl("Hirsch", file), "ReadBaseline-Hirsch", "ReadBaseline-Pferd"))))

 
# for (i in listTXTr){
#   txt <- read.table(paste0(folder, i), header=TRUE)
#   txt$f0mean <- as.numeric(txt$f0mean)
#   txt$zmean <- (txt$f0mean - mean(txt$f0mean))/sd(txt$f0mean) # get z scores
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
#   ff[nrow(ff)+1,] <- c(i, NA, mean(txt$f0mean), mean(txt$f0mean))
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

listBREATHall <- list.files(folder2, pattern="SUM")
listWAVpf <- listBREATHall[grepl("wav", listBREATHall) & !grepl("TextGrid", listBREATHall) & substr(listBREATHall, 2, 2)=="F" & !grepl("breathL", listBREATHall)]
listBREATH <- listBREATHall[grepl("TextGrid", listBREATHall)]
listBREATHr <- listBREATH[substr(listBREATH, 2, 2)=="R"]
listBREATHrj <- listBREATHr[grepl("joint", listBREATHr) | substr(listBREATHr, 1, 2) == "BR"] # read joint AND baseline reading
listBREATHf <- listBREATH[substr(listBREATH, 2, 2)=="F" & !grepl("breathL", listBREATH)]
listBREATHl <- listBREATH[grepl("breathL", listBREATH)] # breathing during listening
listBREATHb <- listBREATH[substr(listBREATH, 2, 2) == "B"] # breathing during baseline period (i.e. just watching the confedearate sitting/biking in silence)
listBREATHlb <- c(listBREATHl, listBREATHb)
listBREATHlb <- listBREATHlb[grepl("TextGrid", listBREATHlb)]
listWAVlb <- listBREATHall[(grepl("breathL", listBREATHall) & !grepl("TextGrid", listBREATHall)) | (substr(listBREATHall, 2, 2) == "B" & !grepl("TextGrid", listBREATHall))]
listWAVlb <- listWAVlb[substr(listWAVlb, 1, 6) %in% substr(listBREATHlb, 1, 6)]
listBREATHlb <- listBREATHlb[substr(listBREATHlb, 1, 6) %in% substr(listWAVlb, 1, 6)]
listWAVrj0 <- listBREATHall[grepl("joint", listBREATHall) & !grepl("TextGrid", listBREATHall)]
listWAVbr0 <- listBREATHall[grepl("wav", listBREATHall) & !grepl("TextGrid", listBREATHall) & substr(listBREATHall, 1, 2) == "BR"]
listWAVbr <- listWAVbr0[grepl("Schwalbe", listWAVbr0) | grepl("Hirsch", listWAVbr0) | grepl("Pferd", listWAVbr0)]
listWAVrj <- c(listWAVbr, listWAVrj0)
listWAVrj <- listWAVrj[substr(listWAVrj, 1, 6) %in% substr(listBREATHrj, 1, 6)]

listTG <- list.files(folder, pattern=".TextGrid") # textgrids with transcriptions and IPU timing
listTGf <- listTG[substr(listTG, 2, 2)=="F"]

listBREATHlbw <- data.frame(cbind(sort(listBREATHlb), sort(listWAVlb)))
colnames(listBREATHlbw) <- c("breath", "wav")
listBREATHlbw$workedwav[substr(listBREATHlbw$breath, 1, 6)==substr(listBREATHlbw$wav, 1, 6)] <- "worked!"
listBREATHlbw$workedwav[substr(listBREATHlbw$breath, 1, 6)!=substr(listBREATHlbw$wav, 1, 6)] <- "NO!!!!!!!!!"
table(listBREATHlbw$workedwav)

listTGf <- listTGf[substr(listTGf, 1, 6) %in% substr(listBREATHf, 1, 6)]
listBREATHf <- listBREATHf[substr(listBREATHf, 1, 6) %in% substr(listTGf, 1, 6)]

listPBGf0 <- as.data.frame(cbind(listBREATHf, listTGf))
listPBGf <- as.data.frame(cbind(listPBGf0, listWAVpf))
colnames(listPBGf) <- c("breath", "tg", "wav")
listPBGf$workedTGs[substr(listPBGf$breath, 1, 6)==substr(listPBGf$tg, 1, 6)] <- "worked!"
listPBGf$workedTGs[substr(listPBGf$breath, 1, 6)!=substr(listPBGf$tg, 1, 6)] <- "NO!!!!!!!!!"
listPBGf$workedwav[substr(listPBGf$breath, 1, 6)==substr(listPBGf$wav, 1, 6)] <- "worked!"
listPBGf$workedwav[substr(listPBGf$breath, 1, 6)!=substr(listPBGf$wav, 1, 6)] <- "NO!!!!!!!!!"
table(listPBGf$workedTGs) # make sure all the TXT and Textgrid files are matching in each row
table(listPBGf$workedwav)

listREAD0 <- data.frame(cbind(listBREATHrj, listWAVrj))
colnames(listREAD0) <- c("breath", "wav")
listREAD0$worked[substr(listREAD0$breath, 1, 6)==substr(listREAD0$wav, 1, 6)] <- "worked!"
listREAD0$worked[substr(listREAD0$breath, 1, 6)!=substr(listREAD0$wav, 1, 6)] <- "NO!!!!!!!!!"
table(listREAD0$worked) # make sure all the TXT and Textgrid files are matching in each row

# confederate:

listCB <- list.files(folder2, pattern="THORAX") # we use the measurement from the thorax only for the confederate
listCB <- listCB[grepl(".TextGrid", listCB)]
listCBr <- listCB[grepl("joint|alone", listCB)]
listCBf <- listCB[grepl("Holidays", listCB)|grepl("Hobbies", listCB)|grepl("Home", listCB)]

listCT <- list.files(folder, pattern="TextGrid")
listCT <- listCT[!grepl("THORAX", listCT)]
listCTr <- listCT[grepl("-Schwalbe", listCT)|grepl("-Hirsch", listCT)|grepl("-Pferd", listCT)]
listCTf <- listCT[grepl("Holidays", listCT)|grepl("Hobbies", listCT)|grepl("Home", listCT)]

listCW <- list.files(folder2, pattern="THORAX")
listCW <- listCW[!grepl("TextGrid", listCW)]
listCWf <- listCW[grepl("Hobbies|Holidays|Home", listCW)]
listCWr <- listCW[grepl("joint|alone", listCW)]

listCBGf0 <- as.data.frame(cbind(listCBf, listCTf))
listCBGf <- as.data.frame(cbind(listCBGf0, listCWf))
colnames(listCBGf) <- c("breath", "tg", "wav")
listCBGf$workedTGs[substr(listCBGf$breath, 1, 6)==substr(listCBGf$tg, 1, 6)] <- "worked!"
listCBGf$workedTGs[substr(listCBGf$breath, 1, 6)!=substr(listCBGf$tg, 1, 6)] <- "NO!!!!!!!!!"
listCBGf$workedwav[substr(listCBGf$breath, 1, 6)==substr(listCBGf$wav, 1, 6)] <- "worked!"
listCBGf$workedwav[substr(listCBGf$breath, 1, 6)!=substr(listCBGf$wav, 1, 6)] <- "NO!!!!!!!!!"
table(listCBGf$workedTGs) # make sure all the TXT and Textgrid files are matching in each row
table(listCBGf$workedwav)

listBGf <- rbind(listPBGf, listCBGf)
listBREATHo <- c(listBREATHf, listBREATHl, listBREATHb, listCBf, listBREATHr, listCBr)

{listREADc <- data.frame(cbind(listCBr, listCWr))
names(listREADc) <- c("breath", "wav")
listREADc <- listREADc %>% 
  mutate(worked = ifelse(substr(listREADc$breath, 1, 6) == substr(listREADc$wav, 1, 6), "worked!", "NO!!!!!"))}
table(listREADc$worked)

listREAD <- rbind(listREAD0, listREADc)

### GETTING DURATION OF BREATHING CYCLES AND BREATHING RATE

# first, check if number of valleys == number of peaks + 1 in each file

PV <- data.frame(matrix(ncol=3, nrow=0))
names(PV) <- c("file", "peaks", "valleys")

IPUandCycles <- data.frame(matrix(nrow=0, ncol=3))
names(IPUandCycles) <- c("IPU", "file", "breathCycleDur")

for(i in listBREATHo){
  breath <- tg.read(paste0(folder2, i))
  PV[nrow(PV)+1,] <- c(i, tg.getNumberOfPoints(breath, 1), tg.getNumberOfPoints(breath, 2))
}
PV$peaks <- as.numeric(PV$peaks)
PV$valleys <- as.numeric(PV$valleys)
pointsok <- PV$file[PV$peaks == (PV$valleys - 1) | PV$file == "SF-TKJ013_SUM_200_breathL_100_wav.TextGrid"]  # for some reason SF-TKJ_breathL is read as if having 76 peaks, but in reality it only has 75 (which you can confirm by annotating the times of all the peaks and seeing that length(unique(peaks))==75)
PV$file[PV$file %!in% pointsok] # the output here should be zero

listBGf <- listBGf %>% filter(breath %in% pointsok) # make sure to get only the textgrids with one more valley than peaks (each cycle is from valley to valley)
listBREATHlb <- listBREATHlb[listBREATHlb %in% pointsok]
listBREATHo <- listBREATHo[listBREATHo %in% pointsok]

pbr1 <- data.frame(matrix(ncol=13, nrow=0))
colnames(pbr1) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "inhalAmp", "inhalDur", "numberIPUs")

durationsOK <- data.frame(matrix(ncol=2, nrow=0))
names(durationsOK) <- c("file", "sameDurations")

# i=23

PVcount <- data.frame(matrix(ncol=3, nrow=0))
names(PVcount) <- c("file", "peaks", "valleys")

for(i in 1:nrow(listBGf)){
  act <- "speaking"
  breath <- tg.read(paste0(folder2, listBGf$breath[i]))
  tg <- tg.read(paste0(folder, listBGf$tg[i]), encoding=detectEncoding(paste0(folder, listBGf$tg[i])))
  b <- (w <- readWave(paste0(folder2, listBGf$wav[i])))@left
  b <- (b - min(b)) / (max(b) - min(b))
  
  PVcount[nrow(PVcount)+1,] <- c(i, tg.getNumberOfPoints(breath, 1), tg.getNumberOfPoints(breath, 2))
  
  ifelse(length(w@left)/w@samp.rate==tg.getTotalDuration(breath), # keep a file to make sure that all durations of wave and textgrid are the same
         durationsOK[nrow(durationsOK)+1,] <- c(substr(listBGf$breath[i], 1, 6), "OK!"),
         durationsOK[nrow(durationsOK)+1,] <- c(substr(listBGf$breath[i], 1, 6), "NO!!!!!!!"))

  # get onset and offset of each interval (IPU)
  IPUtimes <- data.frame(matrix(ncol=4, nrow=0))
  colnames(IPUtimes) <- c("IPU", "label", "start", "end")
  for(n in 1:tg.getNumberOfIntervals(tg, 1)){
    IPUtimes[nrow(IPUtimes)+1,] <- c(n,
                                     tg.getLabel(tg, 1, n),
                                     tg.getIntervalStartTime(tg, 1, n),
                                     tg.getIntervalEndTime(tg, 1, n))
  }
  IPUtimes <- IPUtimes %>%
    filter(label!="") #%>% # exclude silences
    # filter(start >= tg.getIntervalStartTime(tg, 2, as.numeric(tg.findLabels(tg, 2, "task")))) # exclude everything before the "task" section ---- UPDATE: no need to do this because we'll only get the IPUs within the breathing cycles, and those are already correct
  IPUtimes$IPU <- paste0("IPU", 1:nrow(IPUtimes))

  # get time of each point (peaks and valleys) and inhalation amplitude and duration for each cycle
  PVtimes <- data.frame(matrix(ncol=12, nrow=0))
  colnames(PVtimes) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "inhalAmp", "inhalDur")
  for(t in 1:(tg.getNumberOfPoints(breath, 2)-1)){ # number of valleys minus one (the last valley is only counted as offset of its previous cycle)
    if(listBGf$breath[i] %in% listBREATHf){ # for participants: to the time of each peak/valley, add the time between beginning of tg and the interval called "breathS"
      PVtimes[nrow(PVtimes)+1,] <- c(substr(listBGf$breath[i], 1, 6),
                                     act,
                                     paste0("cycle", t), # number of breathing cycle
                                     as.numeric(tg.getPointTime(breath, 2, t)) + as.numeric(tg.getIntervalStartTime(tg, 3, as.numeric(tg.findLabels(tg, 3, "breathS")))),
                                     as.numeric(tg.getPointTime(breath, 1, t)) + as.numeric(tg.getIntervalStartTime(tg, 3, as.numeric(tg.findLabels(tg, 3, "breathS")))),
                                     as.numeric(tg.getPointTime(breath, 2, t+1)) + as.numeric(tg.getIntervalStartTime(tg, 3, as.numeric(tg.findLabels(tg, 3, "breathS")))), # next valley
                                     NA, NA, NA, NA,
                                     as.numeric(b[tg.getPointTime(breath, 1, t)*w@samp.rate] - b[tg.getPointTime(breath, 2, t)*w@samp.rate]), # inhalation amplitude
                                     NA)
    } else if(listBGf$breath[i] %in% listCBf){ # confederate files: the audio files are already aligned to the breathing files, so no need to add anything to the times of the peaks and valleys
      PVtimes[nrow(PVtimes)+1,] <- c(substr(listBGf$breath[i], 1, 6),
                                     act,
                                     paste0("cycle", t), # number of breathing cycle
                                     as.numeric(tg.getPointTime(breath, 2, t)),
                                     as.numeric(tg.getPointTime(breath, 1, t)),
                                     as.numeric(tg.getPointTime(breath, 2, t+1)), # next valley
                                     NA, NA, NA, NA,
                                     as.numeric(b[tg.getPointTime(breath, 1, t)*w@samp.rate] - b[tg.getPointTime(breath, 2, t)*w@samp.rate]), # inhalation amplitude
                                     NA)
    }
    
  }
  PVtimes[, c("onset", "peak", "offset")] <- lapply(PVtimes[, c("onset", "peak", "offset")], as.numeric)
  PVtimes$cycleOK[PVtimes$onset < PVtimes$peak & PVtimes$peak < PVtimes$offset] <- "OK!"
  PVtimes$cycleOK[PVtimes$cycleOK=="NA"] <- "Not OK!"
  table(PVtimes$cycleOK) # make sure that onset < peak < offset (i.e., the cycles make sense)
  
  PVtimes$cycleDur <- PVtimes$offset - PVtimes$onset # duration of each cycle
  PVtimes$numberBreathCycles <- nrow(PVtimes)
  PVtimes$breathCycleDurMean <- mean(PVtimes$cycleDur)
  PVtimes$breathRate <- (nrow(PVtimes) / ((PVtimes$offset[nrow(PVtimes)] - PVtimes$onset[1]) / 60)) # breathing rate = number of cycles / time from first to last valley divided by 60 (to turn into minutes)
  PVtimes$inhalDur <- PVtimes$peak - PVtimes$onset
  
  # get number of IPUs within each cycle
  ic <- data.frame(matrix(ncol=2, nrow=0))
  names(ic) <- c("cycle", "IPU")
  PVtimes <- PVtimes %>%
    mutate_at(c("onset", "offset"), as.numeric) %>%
    mutate_at("breathCycle", as.factor)
  IPUtimes <- IPUtimes %>%
    mutate_at("IPU", as.factor) %>%
    mutate_at(c("start", "end"), as.numeric)

  for(t in 1:nrow(PVtimes)){
    for(n in 1:nrow(IPUtimes)){
      if(IPUtimes$start[n] >= PVtimes$onset[t] && IPUtimes$start[n] <= PVtimes$offset[t]){ # start of IPU before the OFFSET of the cycle: because they were annotated manually and sometimes the END of the IPU is marked as AFTER the offset of the cycle
        ic[nrow(ic)+1,] <- c(PVtimes$breathCycle[t], IPUtimes$IPU[n])
      }
    }
  }
  ic$IPU <- paste0("IPU", 1:nrow(ic)) # not sure why I wrote this here... Aren't the IPU names already right?
  k <- data.frame(table(ic$cycle)) # get number of IPUs per cycle
  
  for(l in 1:nrow(PVtimes)){
    for(j in 1:nrow(k)){
      if(PVtimes$breathCycle[l] == k$Var1[j]){
        PVtimes$numberIPUs[l] <- k$Freq[j]
      }
    }
  }
  
  PVtimes <- PVtimes %>% select(-c("cycleOK"))
  
  pbr1 <- rbind(pbr1, PVtimes)
  
  # save the length of breath cycle in which each IPU is inserted -- join this with speech dataset
  ic <- ic %>%
    mutate(file = substr(listBGf$breath[i], 1, 6)) %>%
    rename(breathCycle = cycle)
  tojoin <- PVtimes %>% select(breathCycle, cycleDur)
  ic <- merge(ic, tojoin, by="breathCycle")
  ic <- ic %>%
    rename(breathCycleDur = cycleDur) %>%
    select(-"breathCycle")
  
  IPUandCycles <- rbind(IPUandCycles, ic)
}

{
# PVcount$diff[PVcount$peaks > PVcount$valleys] <- "more PEAKS"
# PVcount$diff[PVcount$peaks < PVcount$valleys] <- "more VALLEYS"
# PVcount$diff[PVcount$peaks == PVcount$valleys] <- "same number"
# table(PVcount$diff)
# print("SAME") ; print(PVcount$file[PVcount$diff=="same number"])
# print("MORE PEAKS") ; print(PVcount$file[PVcount$diff=="more PEAKS"])
# PVcount <- PVcount %>%
#   mutate(ok = ifelse(valleys == peaks + 1, "ok", "no!!!!!"))
# table(PVcount$ok, )
}

table(durationsOK$sameDurations)

IPUandCycles$IPU <- gsub("IPU", "", IPUandCycles$IPU)

pbr2 <- data.frame(matrix(ncol=12, nrow=0))
colnames(pbr2) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "inhalAmp", "inhalDur")

PVcount <- data.frame(matrix(ncol=3, nrow=0))
names(PVcount) <- c("file", "peaks", "valleys")

# i=4

for(i in 1:nrow(listBREATHlbw)){ # list with the listening part of the free spech files and with the "watching" files (where the participants just watched Carry with everyone in silence)
  if(listBREATHlbw$breath[i] %in% listBREATHb){
    act <- "watching"
  } else if(listBREATHlbw$breath[i] %in% listBREATHl){
    act <- "listening"
  }
  
  breath <- tg.read(paste0(folder2, listBREATHlbw$breath[i]))
  b <- (w <- readWave(paste0(folder2, listBREATHlbw$wav[i])))@left
  b <- (b - min(b)) / (max(b) - min(b))
  
  PVcount[nrow(PVcount)+1,] <- c(i, tg.getNumberOfPoints(breath, 1), tg.getNumberOfPoints(breath, 2))
  
  # get time of each point (peaks and valleys)
  PVtimes <- data.frame(matrix(ncol=12, nrow=0))
  colnames(PVtimes) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "inhalAmp", "inhalDur")
  for(t in 1:(tg.getNumberOfPoints(breath, 2)-1)){ # number of valleys minus one (the last valley is only counted as offset of its previous cycle)
    PVtimes[nrow(PVtimes)+1,] <- c(substr(listBREATHlbw$breath[i], 1, 6),
                                   act,
                                   paste0("cycle", t), # number of breathing cycle
                                   as.numeric(tg.getPointTime(breath, 2, t)),
                                   as.numeric(tg.getPointTime(breath, 1, t)),
                                   as.numeric(tg.getPointTime(breath, 2, t+1)), # next valley
                                   NA, NA, NA, NA,
                                   as.numeric(b[tg.getPointTime(breath, 1, t)*w@samp.rate] - b[tg.getPointTime(breath, 2, t)*w@samp.rate]), # inhalation amplitude
                                   NA)
  }
  PVtimes[, c("onset", "peak", "offset")] <- lapply(PVtimes[, c("onset", "peak", "offset")], as.numeric)
  PVtimes$cycleOK[PVtimes$onset < PVtimes$peak & PVtimes$peak < PVtimes$offset] <- "OK!"
  PVtimes$cycleOK[PVtimes$cycleOK=="NA"] <- "Not OK!"
  table(PVtimes$cycleOK) # make sure that onset < peak < offset (i.e., the cycles make sense)
  
  PVtimes$cycleDur <- PVtimes$offset - PVtimes$onset # duration of each cycle
  PVtimes$numberBreathCycles <- nrow(PVtimes)
  PVtimes$breathCycleDurMean <- mean(PVtimes$cycleDur)
  PVtimes$breathRate <- (nrow(PVtimes) / ((PVtimes$offset[nrow(PVtimes)] - PVtimes$onset[1]) / 60)) # breathing rate = number of cycles / time from first to last valley divided by 60 (to turn into minutes)
  PVtimes$inhalDur <- PVtimes$peak - PVtimes$onset
  
  PVtimes <- PVtimes %>% select(-c("cycleOK"))
  
  pbr2 <- rbind(pbr2, PVtimes)
}

pbr2$numberIPUs <- NA

{
# PVcount$diff[PVcount$peaks > PVcount$valleys] <- "more PEAKS"
# PVcount$diff[PVcount$peaks < PVcount$valleys] <- "more VALLEYS"
# PVcount$diff[PVcount$peaks == PVcount$valleys] <- "same number"
# table(PVcount$diff)
# print("SAME") ; print(PVcount$file[PVcount$diff=="same number"])
# print("MORE PEAKS") ; print(PVcount$file[PVcount$diff=="more PEAKS"])
# PVcount <- PVcount %>%
#   mutate(ok = ifelse(valleys == peaks + 1, "ok", "no!!!!!"))
# table(PVcount$ok)
}

br0 <- rbind(pbr1, pbr2)

# save(br, file=paste0(folder, "BreathingData.RData"))

### Now do the same for reading files

pbr3 <- data.frame(matrix(ncol=12, nrow=0))
colnames(pbr3) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "inhalAmp", "inhalDur")

PVcount <- data.frame(matrix(ncol=3, nrow=0))
names(PVcount) <- c("file", "peaks", "valleys")

for(i in 1:nrow(listREAD)){
  if(grepl("_Hirsch", listREAD$breath[i])){
    act <- "ReadBaseline-Hirsch"
  } else if(grepl("_Pferd", listREAD$breath[i])){
    act <- "ReadBaseline-Pferd"
  } else if(grepl("_Schwalbe", listREAD$breath[i])){
    act <- "ReadBaseline-Schwalbe"
  } else if(grepl("joint", listREAD$breath[i])){
    act <- "ReadJoint"
  } else if(grepl("alone", listREAD$breath[i])){
    act <- "ReadAlone"
  }
  
  breath <- tg.read(paste0(folder2, listREAD$breath[i]))
  b <- (w <- readWave(paste0(folder2, listREAD$wav[i])))@left
  b <- (b - min(b)) / (max(b) - min(b))
  
  PVcount[nrow(PVcount)+1,] <- c(i, tg.getNumberOfPoints(breath, 1), tg.getNumberOfPoints(breath, 2))
  
  # get time of each point (peaks and valleys)
  PVtimes <- data.frame(matrix(ncol=12, nrow=0))
  colnames(PVtimes) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "inhalAmp", "inhalDur")
  for(t in 1:(tg.getNumberOfPoints(breath, 2)-1)){ # number of valleys minus one (the last valley is only counted as offset of its previous cycle)
    PVtimes[nrow(PVtimes)+1,] <- c(substr(listREAD$breath[i], 1, 6),
                                   act,
                                   paste0("cycle", t), # number of breathing cycle
                                   as.numeric(tg.getPointTime(breath, 2, t)),
                                   as.numeric(tg.getPointTime(breath, 1, t)),
                                   as.numeric(tg.getPointTime(breath, 2, t+1)), # next valley
                                   NA, NA, NA, NA,
                                   as.numeric(b[tg.getPointTime(breath, 1, t)*w@samp.rate] - b[tg.getPointTime(breath, 2, t)*w@samp.rate]), # inhalation amplitude
                                   NA)
  }
  PVtimes[, c("onset", "peak", "offset")] <- lapply(PVtimes[, c("onset", "peak", "offset")], as.numeric)
  PVtimes$cycleOK[PVtimes$onset < PVtimes$peak & PVtimes$peak < PVtimes$offset] <- "OK!"
  PVtimes$cycleOK[PVtimes$cycleOK=="NA"] <- "Not OK!"
  table(PVtimes$cycleOK) # make sure that onset < peak < offset (i.e., the cycles make sense)
  
  PVtimes$cycleDur <- PVtimes$offset - PVtimes$onset # duration of each cycle
  PVtimes$numberBreathCycles <- nrow(PVtimes)
  PVtimes$breathCycleDurMean <- mean(PVtimes$cycleDur)
  PVtimes$breathRate <- (nrow(PVtimes) / ((PVtimes$offset[nrow(PVtimes)] - PVtimes$onset[1]) / 60)) # breathing rate = number of cycles / time from first to last valley divided by 60 (to turn into minutes)
  PVtimes$inhalDur <- PVtimes$peak - PVtimes$onset
  
  PVtimes <- PVtimes %>% select(-c("cycleOK"))
  
  pbr3 <- rbind(pbr3, PVtimes)
}

pbr3$numberIPUs <- NA

{
# PVcount$diff[PVcount$peaks > PVcount$valleys] <- "more PEAKS"
# PVcount$diff[PVcount$peaks < PVcount$valleys] <- "more VALLEYS"
# PVcount$diff[PVcount$peaks == PVcount$valleys] <- "same number"
# table(PVcount$diff)
# 
# print("SAME") ; print(PVcount$file[PVcount$diff=="same number"])
# print("MORE PEAKS") ; print(PVcount$file[PVcount$diff=="more PEAKS"])
# PVcount <- PVcount %>%
#   mutate(ok = ifelse(valleys == peaks + 1, "ok", "no!!!!!"))
# table(PVcount$ok)
}

br <- rbind(br0, pbr3)

br <- br %>%
  mutate(file = ifelse(grepl("Schwalbe", act), paste0(file, "-Schwalbe"),
                       ifelse(grepl("Hirsch", act), paste0(file, "-Hirsch"),
                              ifelse(grepl("Pferd", act), paste0(file, "-Pferd"),
                                     file))),
         act = ifelse(grepl("Baseline", act), "ReadBaseline", act))

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

fs0 <- full_join(ff, sr, by=c("file", "IPU"), all=TRUE)

fs <- full_join(fs0, IPUandCycles, by=c("file", "IPU"), all=TRUE)

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

frb <- merge(fr, br %>% select(file, breathRate), by="file")

frb <- frb %>% 
  mutate(Task = ifelse(grepl("Baseline", Task), "ReadBaseline", Task))

conffiles <- c("irs", "obb", "oli", "ome", "fer", "chw")

d <- list(fs, br, frb)

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
  d[[i]]$Task[d[[i]]$act == "ReadJoint"] <- "ReadJoint"
  d[[i]]$Task[d[[i]]$act == "ReadAlone"] <- "ReadAlone"
  d[[i]]$Task[d[[i]]$act == "ReadBaseline"] <- "ReadBaseline"
  d[[i]]$Task <- as.factor(d[[i]]$Task)

}

fs <- d[[1]]
br <- d[[2]]
frb <- d[[3]]

meta <- read.csv("C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/metadata.csv", fileEncoding="UTF-8-BOM")
names(meta)[names(meta) == "Participant"] <- "Speaker"

fsm <- merge(fs, meta, by="Speaker", all=TRUE)
brm <- merge(br, meta, by="Speaker", all=TRUE)
frm <- merge(frb, meta, by="Speaker", all=TRUE)

dat <- list(fsm, brm, frm)

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

fsm <- dat[[1]]
brm <- dat[[2]]
frb <- dat[[3]]

# fixing a mistake I found (why doesn't this work????)
# frb$Task <- as.factor(frb$Task)
# frb$Task[grepl("Hirs|Schw|Pfer", frb$file)] <- "Read"

# end of fix

brm <- brm %>% mutate_at(c("inhalDur", "inhalAmp"), as.numeric)

fsm <- merge(fsm, brm %>% select(c(file, breathCycleDurMean, breathRate, inhalDur, inhalAmp)) %>% filter(!duplicated(file)) %>% filter(substr(file, 2, 2) != "B"), by="file")

# for(i in 1:nrow(fsm)){
#   if(is.na(fsm$breathCycleDur[i])){
#     fsm$breathCycleDur[i] <- fsm$breathCycleDur[as.numeric(fsm$IPU[i])-1 & fsm$file[i]]
#   }
# } # didn't work

# in SF-WJH f0IPUmean is NA, so fix that:
unique(fsm$f0IPUmean[fsm$file=="SF-WJH"])[!is.na(unique(fsm$f0IPUmean[fsm$file=="SF-WJH"]))==TRUE]
fsm$f0IPUmean[fsm$file=="SF-WJH"] <- unique(fsm$f0IPUmean[fsm$file=="SF-WJH"])[!is.na(unique(fsm$f0IPUmean[fsm$file=="SF-WJH"]))==TRUE]

save(fsm, file=paste0(folder, "DataSpeech.RData"))
save(brm, file=paste0(folder, "DataBreathing.RData"))
save(frb, file=paste0(folder, "DataReadSpeech.RData"))

# # for Susanne
# brm <- brm %>% filter(Speaker=="Confederate")
# write.csv(brm, file=paste0(folder, "ConfederateBreathingData.csv"))
###

# #####

### Calculate difference between participants' and confederate's speech features (I'm sure there would be a more elegant solution)

# transform the dataset so it contains the confederate information for each row of interest of each participant

# here, it doesn't make sense anymore to have information per IPU (we don't want to subtract the value of each IPU, just of the entire stretch of speech)

# load(paste0(folder, "DataSpeech.RData"))

dat <- fsm %>%
  filter(!duplicated(file)) %>%
  select(-c(IPU, f0raw, label, speechRateIPU))

conf <- dat %>%
  filter(Speaker == "Confederate") %>%
  select(c("f0IPUmean", "articRate", "Condition", "Task", "Topic", "breathCycleDurMean", "breathRate"))
colnames(conf) <- sub("^","C", colnames(conf))
dat2 <-dat[dat$Speaker!="Confederate",]
dat3 <- dat2[rep(seq_len(nrow(dat2)), each = nrow(conf)),]
rownames(dat3) <- 1:nrow(dat3)
conf2 <- do.call("rbind", replicate((nrow(dat3)/nrow(conf)), conf, simplify=FALSE)) # this doesn't take long for datasets that aren't so long, but here it's taking a while. I wonder if there's a simpler solution.

dat4 <- cbind(dat3, conf2)

dat4 <- dat4[(dat4$Condition == dat4$CCondition & dat4$Topic == dat4$CTopic) | (dat4$Condition == "Baseline"),]
dat4[dat4$Condition=="Baseline", colnames(conf)] <- NA
dat4 <- dat4[!duplicated(dat4),] # no duplicates of the same row

dat4$CTask <- as.factor(dat4$CTask)
dat4$CTopic <- as.factor(dat4$CTopic)

# calculate differences

dat4$f0rawDiff <- dat4$f0IPUmean - dat4$Cf0IPUmean
dat4$articRateDiff <- dat4$articRate - dat4$CarticRate
dat4$breathCycleDurDiff <- dat4$breathCycleDurMean - dat4$CbreathCycleDurMean
dat4$breathRateDiff <- dat4$breathRate - dat4$CbreathRate

# dat4[dat4$Condition=="Baseline", colnames(grepl("Diff", colnames(dat4)))] <- NA # turn the "Diff" values into NA for the baseline, because they don't make sense there

# gender differences

dat4$GenderDiff.TMFF <- dat4$ConfGenderF - dat4$TMF.F # difference between participants' gender identity and their perception of conf's gender expression
dat4$GenderDiff.TMFM <- dat4$ConfGenderM - dat4$TMF.M

# Still missing from dataset:
# How well the participants synchronized their read speech

# 4
dat <- dat4

save(dat, file=paste0(folder, "DataWithDifferences.RData"))


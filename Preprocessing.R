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
### 2. Read the TXT files, remove the outliers (> 3 SDs) and annotate the mean of the f0mean of each file and mean of the f0median of each file.
### 3. Merge all datasets containing speech rate, f0 mean and median, and metadata.
###    Annotate condition, file order, difference between participants' and confederate's data and so on.
###    Calculate gender scores.
### 4. Save dataset as an R file.
###

library(rPraat) # to work with TextGrids
library(tidyverse)
library(sylly.de) # for counting syllables

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/" # folder with all needed files
`%!in%` <- Negate(`%in%`)

############# Speech Rate

# FROM NUMBER OF SYLLABLES DIVIDED BY TIME OF MANUALLY ANNOTATED IPUs

### IPUs, PAUSES

listTG <- list.files(folder, pattern=".TextGrid")
listCTG <- listTG[grepl("Holidays", listTG) | grepl("Hobbies", listTG) | grepl("Home", listTG)]
listPTG <- listTG[substr(listTG, 2, 2)=="F"]
listTGf <- c(listPTG, listCTG)

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

############# f0 mean, median

# 2

listTXT <- list.files(folder, pattern=".txt")
confF <- listTXT[grepl("Home", listTXT)|grepl("Hobbies", listTXT)|grepl("Holidays", listTXT)]
listTXTf <- listTXT[substr(listTXT, 2, 2)=="F" | listTXT %in% confF]
confRA <- c("H-Hirsch_alone.txt", "H-Pferd_alone.txt", "H-Schwalbe_alone.txt", "L-Hirsch_alone.txt", "L-Pferd_alone.txt", "L-Schwalbe_alone.txt", "S-Hirsch_alone.txt", "S-Pferd_alone.txt", "S-Schwalbe_alone.txt")
confRJ <- c("H-Hirsch_joint.txt", "H-Pferd_joint.txt", "H-Schwalbe_joint.txt", "L-Hirsch_joint.txt", "L-Pferd_joint.txt", "L-Schwalbe_joint.txt", "S-Hirsch_joint.txt", "S-Pferd_joint.txt", "S-Schwalbe_joint.txt")
f <- c("BF", "HF", "LF", "SF")
readJ <- list.files(folder, pattern="joint.txt")
readJ <- readJ[substr(readJ, 2, 2) != "-"]
readA <- list.files(folder, pattern="alone.txt")
readA <- readA[substr(readA, 2, 2) != "-"]

listTG <- list.files(folder, pattern=".TextGrid")
listTG <- listTG[!grepl("_VUV", listTG)]
confFtg <- listTG[grepl("Home", listTG)|grepl("Hobbies", listTG)|grepl("Holidays", listTG)]
listTGf <- listTG[substr(listTG, 2, 2)=="F" | listTG %in% confFtg]

listTxg <- as.data.frame(cbind(listTXTf, listTGf))
colnames(listTxg) <- c("txt", "tg")
listTxg$worked[substr(listTxg$txt, 1, 6)==substr(listTxg$tg, 1, 6)] <- "worked!"
listTxg$worked[substr(listTxg$txt, 1, 6)!=substr(listTxg$tg, 1, 6)] <- "NO!!!!!!!!!"
unique(listTxg$worked) # make sure all the TXT and Textgrid files are matching in each row

ff <- data.frame(matrix(ncol=4, nrow=0))
colnames(ff) <- c("IPU", "f0mean", "file", "label")

for(i in 1:nrow(listTxg)){
  txt <- read.table(paste0(folder, listTxg$txt[i]), header=TRUE, na.strings = "--undefined--") # load file with f0 means
  txt$f0mean <- as.numeric(txt$f0mean)
  tg <- tg.read(paste0(folder, listTxg$tg[i]), encoding=detectEncoding(paste0(folder, listTxg$tg[i]))) # load textgrid with defined IPUs
  
  # create object ("intervals") with the index of each IPU and its onset and offset time
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
  colnames(f0) <- c("IPU", "f0mean")
  for(p in 1:nrow(IPUtimes)){
    for(n in 1:nrow(txt)){
      if(txt$onset[n] >= IPUtimes$start[p] && txt$offset[n] <= IPUtimes$end[p]){
        f0[nrow(f0)+1,] <- c(IPUtimes$IPU[p], as.numeric(txt$f0mean[n]))
      }
    }
  }
  f0 <- f0 %>%
    filter(!is.na(f0mean)) %>%
    mutate_at(c("IPU", "f0mean"), as.numeric)
  f0perIPU <- aggregate(f0$f0mean, list(f0$IPU), FUN=mean)
  colnames(f0perIPU) <- c("IPU", "f0mean")
  f0perIPU <- f0perIPU %>%
    mutate(IPU = 1:nrow(f0perIPU)) %>%
    mutate(file = substr(listTxg$txt[i], 1, 6)) %>%
    mutate(f0z = (f0mean - mean(f0mean))/sd(f0mean)) %>%
    mutate(f0mean = ifelse(abs(f0z) > 2, NA, f0mean)) %>% # I don't want to use the IPUs with outlier f0mean, but I don't want to completely delete those IPUs from the dataset because they'll still be joined with the speech rate information. So just turn them into NA here.
    mutate(f0IPUmean = mean(f0mean, na.rm=TRUE))
  f <- merge(f0perIPU, IPUtimes %>% select("IPU", "label"), by="IPU")
  ff <- rbind(ff, f)
}

ff <- ff %>% select(file, IPU, f0mean, f0IPUmean, label)

ff <- ff %>% mutate(f0z = (f0mean - mean(f0mean, na.rm=TRUE))/sd(f0mean, na.rm=TRUE))
nrow(ff %>% filter(abs(f0z) > 2))/nrow(ff) # percentage of IPUs with f0 outside of 2 SD

ff <- ff %>% mutate(f0mean = ifelse(abs(f0z) > 2, NA, f0mean)) # keep f0 below 2 SD?

# for(i in unique(ff$file)){
#   plot(ff$f0mean[ff$file==i])
#   readline("Continue")
# }

# listTXTr <- listTXT[listTXT %!in% listTXTf]
# 
# # for reading files: no IPU separation:
# 
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
# ff$f0mean <- as.numeric(ff$f0mean)

##################################################################

#### BREATHING DATA

# number of textgrid files == number of wav files: [H,L,S][B, F]

folder2 <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/PeaksValleys/"

# participants: 

listBREATH <- list.files(folder2, pattern="SUM")
listBREATH <- listBREATH[grepl("TextGrid", listBREATH)]
listBREATHr <- listBREATH[substr(listBREATH, 2, 2)=="R"]
listBREATHf <- listBREATH[substr(listBREATH, 2, 2)=="F" & !grepl("breathL", listBREATH)]
listBREATHl <- listBREATH[grepl("breathL", listBREATH)] # breathing during listening
listBREATHb <- listBREATH[substr(listBREATH, 2, 2) == "B"] # breathing during baseline period (i.e. just watching the confedearate sitting/biking in silence)
listBREATHlb <- c(listBREATHl, listBREATHb)

listTG <- list.files(folder, pattern=".TextGrid")
listTGf <- listTG[substr(listTG, 2, 2)=="F"]

listTGf <- listTGf[substr(listTGf, 1, 6) %in% substr(listBREATHf, 1, 6)]
listBREATHf <- listBREATHf[substr(listBREATHf, 1, 6) %in% substr(listTGf, 1, 6)]

listPBGf <- as.data.frame(cbind(listBREATHf, listTGf))
colnames(listPBGf) <- c("breath", "tg")
listPBGf$worked[substr(listPBGf$breath, 1, 6)==substr(listPBGf$tg, 1, 6)] <- "worked!"
listPBGf$worked[substr(listPBGf$breath, 1, 6)!=substr(listPBGf$tg, 1, 6)] <- "NO!!!!!!!!!"
table(listPBGf$worked) # make sure all the TXT and Textgrid files are matching in each row

# confederate:

listCB <- list.files(folder2, pattern="THORAX") # we use the measurement from the thorax only for the confederate
listCB <- listCB[grepl(".TextGrid", listCB)]
listCBr <- listCB[grepl("alone", listCB)|grepl("joint", listCB)]
listCBf <- listCB[grepl("Holidays", listCB)|grepl("Hobbies", listCB)|grepl("Home", listCB)]

listCT <- list.files(folder, pattern="TextGrid")
listCT <- listCT[!grepl("THORAX", listCT)]
listCTr <- listCT[grepl("Schwalbe", listCT)|grepl("Hirsch", listCT)|grepl("Pferd", listCT)]
listCTf <- listCT[grepl("Holidays", listCT)|grepl("Hobbies", listCT)|grepl("Home", listCT)]
listCBGf <- as.data.frame(cbind(listCBf, listCTf))
colnames(listCBGf) <- c("breath", "tg")
listCBGf$worked[substr(listCBGf$breath, 1, 6)==substr(listCBGf$tg, 1, 6)] <- "worked!"
listCBGf$worked[substr(listCBGf$breath, 1, 6)!=substr(listCBGf$tg, 1, 6)] <- "NO!!!!!!!!!"
table(listCBGf$worked) # make sure all the TXT and Textgrid files are matching in each row

listBGf <- rbind(listPBGf, listCBGf)
listBREATHo <- c(listBREATHf, listBREATHl, listBREATHb, listCBf)

### GETTING DURATION OF BREATHING CYCLES AND BREATHING RATE

# first, check if number of valleys = number of peaks + 1 in each file

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

pbr1 <- data.frame(matrix(ncol=8, nrow=0))
colnames(pbr1) <- c("file", "act", "breathCycle", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "numberIPUs")

# i=88

for(i in 1:nrow(listBGf)){
  act <- "speaking"
  breath <- tg.read(paste0(folder2, listBGf$breath[i]))
  tg <- tg.read(paste0(folder, listBGf$tg[i]), encoding=detectEncoding(paste0(folder, listBGf$tg[i])))

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

  # get time of each point (peaks and valleys)
  PVtimes <- data.frame(matrix(ncol=10, nrow=0))
  colnames(PVtimes) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate")
  for(t in 1:(tg.getNumberOfPoints(breath, 2)-1)){ # number of valleys minus one (the last valley is only counted as offset of its previous cycle)
    if(listBGf$breath[i] %in% listBREATHf){ # for participants: to the time of each peak/valley, add the time between beginning of tg and the interval called "breathS"
      PVtimes[nrow(PVtimes)+1,] <- c(substr(listBGf$breath[i], 1, 6),
                                     act,
                                     paste0("cycle", t), # number of breathing cycle
                                     as.numeric(tg.getPointTime(breath, 2, t)) + as.numeric(tg.getIntervalStartTime(tg, 3, as.numeric(tg.findLabels(tg, 3, "breathS")))),
                                     as.numeric(tg.getPointTime(breath, 1, t)) + as.numeric(tg.getIntervalStartTime(tg, 3, as.numeric(tg.findLabels(tg, 3, "breathS")))),
                                     as.numeric(tg.getPointTime(breath, 2, t+1)) + as.numeric(tg.getIntervalStartTime(tg, 3, as.numeric(tg.findLabels(tg, 3, "breathS")))), # next valley
                                     NA, NA, NA, NA)
    } else if(listBGf$breath[i] %in% listCBf){ # confederate files: the audio files are already aligned to the breathing files, so no need to add anything to the times of the peaks and valleys
      PVtimes[nrow(PVtimes)+1,] <- c(substr(listBGf$breath[i], 1, 6),
                                     act,
                                     paste0("cycle", t), # number of breathing cycle
                                     as.numeric(tg.getPointTime(breath, 2, t)),
                                     as.numeric(tg.getPointTime(breath, 1, t)),
                                     as.numeric(tg.getPointTime(breath, 2, t+1)), # next valley
                                     NA, NA, NA, NA)
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
  ic$IPU <- paste0("IPU", 1:nrow(ic))
  k <- data.frame(table(ic$cycle)) # get number of IPUs per cycle
  
  for(l in 1:nrow(PVtimes)){
    for(j in 1:nrow(k)){
      if(PVtimes$breathCycle[l] == k$Var1[j]){
        PVtimes$numberIPUs[l] <- k$Freq[j]
      }
    }
  }
  
  PVtimes <- PVtimes %>% select(-c("onset", "peak", "offset", "cycleOK"))
  
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

IPUandCycles$IPU <- gsub("IPU", "", IPUandCycles$IPU)

pbr2 <- data.frame(matrix(ncol=8, nrow=0))
colnames(pbr2) <- c("file", "act", "breathCycle", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "numberIPUs")

# i="HF-ATN007_SUM_200_breathL_wav.TextGrid"

for(i in listBREATHlb){ # list with the listening part of the free spech files and with the "watching" files (where the participants just watched Carry with everyone in silence)
  if(i %in% listBREATHb){
    act <- "watching"
  } else if(i %in% listBREATHl){
    act <- "listening"
  }
  
  breath <- tg.read(paste0(folder2, i))
  
  # get time of each point (peaks and valleys)
  PVtimes <- data.frame(matrix(ncol=11, nrow=0))
  colnames(PVtimes) <- c("file", "act", "breathCycle", "onset", "peak", "offset", "cycleDur", "numberBreathCycles", "breathCycleDurMean", "breathRate", "numberIPUs")
  for(t in 1:(tg.getNumberOfPoints(breath, 2)-1)){ # number of valleys minus one (the last valley is only counted as offset of its previous cycle)
    PVtimes[nrow(PVtimes)+1,] <- c(substr(i, 1, 6),
                                   act,
                                   paste0("cycle", t), # number of breathing cycle
                                   as.numeric(tg.getPointTime(breath, 2, t)),
                                   as.numeric(tg.getPointTime(breath, 1, t)),
                                   as.numeric(tg.getPointTime(breath, 2, t+1)), # next valley
                                   NA, NA, NA, NA, NA)
  }
  PVtimes[, c("onset", "peak", "offset")] <- lapply(PVtimes[, c("onset", "peak", "offset")], as.numeric)
  PVtimes$cycleOK[PVtimes$onset < PVtimes$peak & PVtimes$peak < PVtimes$offset] <- "OK!"
  PVtimes$cycleOK[PVtimes$cycleOK=="NA"] <- "Not OK!"
  table(PVtimes$cycleOK) # make sure that onset < peak < offset (i.e., the cycles make sense)
  
  PVtimes$cycleDur <- PVtimes$offset - PVtimes$onset # duration of each cycle
  PVtimes$numberBreathCycles <- nrow(PVtimes)
  PVtimes$breathCycleDurMean <- mean(PVtimes$cycleDur)
  PVtimes$breathRate <- (nrow(PVtimes) / ((PVtimes$offset[nrow(PVtimes)] - PVtimes$onset[1]) / 60)) # breathing rate = number of cycles / time from first to last valley divided by 60 (to turn into minutes)
  
  PVtimes <- PVtimes %>% select(-c("onset", "peak", "offset", "cycleOK"))
  
  pbr2 <- rbind(pbr2, PVtimes)
}

br <- rbind(pbr1, pbr2)

# save(br, file=paste0(folder, "BreathingData.RData"))

################

# for(i in listBREATH){
#   tg <- tg.read(paste0(folder2, i))
#   if(i %in% listBREATHr){
#     if(grepl("alone", i)){
#       i <- paste0(substr(i, 1, 2), "A", substr(i, 4, 6))
#     } else if(grepl("joint", i)){
#       i <- paste0(substr(i, 1, 2), "J", substr(i, 4, 6))
#     } else if(grepl("PFERD", i)){
#       i <- paste0(substr(i, 1, 2), "P", substr(i, 4, 6))
#     } else if(grepl("SCHWALBE", i)){
#       i <- paste0(substr(i, 1, 2), "S", substr(i, 4, 6))
#     } else if(grepl("HIRSCH", i)){
#       i <- paste0(substr(i, 1, 2), "H", substr(i, 4, 6))
#     }
#   }
#   pbr[nrow(pbr)+1,] <- c(i, tg.getNumberOfPoints(tg, 1), tg.getNumberOfPoints(tg, 2))
# }

#######################################################################################################################
#######################################################################################################################

# for(i in listCB){
#   tg <- tg.read(paste0(folder2, i))
#   cbr[nrow(cbr)+1,] <- c(i, tg.getNumberOfPoints(tg, 1), tg.getNumberOfPoints(tg, 2))
# }
# 
# cbr <- cbr %>%
#   mutate(file = gsub("_200.TextGrid", "", file)) %>%
#   mutate(file = gsub("_THORAX", "", file))
# 
# cbr$fileOG <- cbr$file
# cbr$file <- paste0(substr(cbr$file, 1, 6))
# cbr$file[grepl("alone", cbr$fileOG)] <- paste0(substr(cbr$fileOG[grepl("alone", cbr$fileOG)], 1, 1), "A", substr(cbr$fileOG[grepl("alone", cbr$fileOG)], 3, 6))
# cbr$file[grepl("joint", cbr$fileOG)] <- paste0(substr(cbr$fileOG[grepl("joint", cbr$fileOG)], 1, 1), "J", substr(cbr$fileOG[grepl("joint", cbr$fileOG)], 3, 6))
# cbr$fileOG <- NULL

##################################################################

# 3


ff$IPU <- as.factor(ff$IPU)

fs <- full_join(ff, sr, by=c("file", "IPU"), all=TRUE)

conffiles <- c("irs", "obb", "oli", "ome", "fer", "chw")

d <- list(fs, br)

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
  d[[i]]$Task[substr(d[[i]]$file, 2, 3) == "RA"] <- "ReadAlone"
  d[[i]]$Task[substr(d[[i]]$file, 2, 3) == "RJ"] <- "ReadJoint"
  d[[i]]$Task[substr(d[[i]]$file, 1, 2) == "BR"] <- "ReadAlone" # baseline reading is also alone
  d[[i]]$Task[d[[i]]$Speaker == "Confederate" & substr(d[[i]]$file, 2, 2) == "A"] <- "ReadAlone"
  d[[i]]$Task[d[[i]]$Speaker == "Confederate" & substr(d[[i]]$file, 2, 2) == "J"] <- "ReadJoint"
  d[[i]]$Task[d[[i]]$Speaker == "Confederate" & substr(d[[i]]$file, 2, 2) == "-"] <- "Free"
  d[[i]]$Task <- as.factor(d[[i]]$Task)
}

fs <- d[[1]]
br <- d[[2]]

meta <- read.csv("C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/metadata.csv", fileEncoding="UTF-8-BOM")
names(meta)[names(meta) == "Participant"] <- "Speaker"

fsm <- merge(fs, meta, by="Speaker", all=TRUE)
brm <- merge(br, meta, by="Speaker", all=TRUE)

dat <- list(fsm, brm)

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

fsm <- merge(fsm, brm %>% select(c(file, breathCycleDurMean, breathRate)) %>% filter(!duplicated(file)) %>% filter(substr(file, 2, 2) != "B"), by="file")
fsm <- full_join(fsm, IPUandCycles, by=c("file", "IPU"), all=TRUE)
for(i in 1:nrow(fsm)){
  if(is.na(fsm$breathCycleDur[i])){
    fsm$breathCycleDur[i] <- fsm$breathCycleDur[as.numeric(fsm$IPU[i])-1 & fsm$file[i]]
  }
}

save(fsm, file=paste0(folder, "DataSpeech.RData"))
save(brm, file=paste0(folder, "DataBreathing.RData"))

# #####

### Calculate difference between participants' and confederate's speech features (I'm sure there would be a more elegant solution)

# transform the dataset so it contains the confederate information for each row of interest of each participant

# here, it doesn't make sense anymore to have information per IPU (we don't want to subtract the value of each IPU, just of the entire stretch of speech)

# load(paste0(folder, "DataSpeech.RData"))

dat <- fsm %>%
  filter(!duplicated(file)) %>%
  select(-c(IPU, f0mean, label, f0z, speechRateIPU))

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

dat4$f0meanDiff <- dat4$f0IPUmean - dat4$Cf0IPUmean
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


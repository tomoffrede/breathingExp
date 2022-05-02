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

############# Speech Rate

####################################### FROM PRAAT SCRIPT

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/" # folder with all needed files
`%!in%` <- Negate(`%in%`)

# I made a mistake when extracting the speech rate and articulation rate of the files, so there are three files to join now

sr0 <- read.csv(paste0(folder, "additionalBandPass.csv"))

sr0 <- sr0 %>%
  select(c(1, 4:7)) %>%
  set_names(c("name", "durationSpeech", "phonTime", "speechRate", "articRate")) %>%
  mutate(name = gsub("_task", "", name)) %>%
  mutate(file = substr(sr0$name, 1, 6)) %>%
  filter(substr(file, 2, 2) != "-") # these are the confederate's files. we'll use the confederate's values that were calculated in the other file below (sr2)

sr0$file[substr(sr0$name, 17, 21)=="PFERD"] <- paste0(substr(sr0$name[substr(sr0$name, 17, 21)=="PFERD"], 1, 2), "P", substr(sr0$name[substr(sr0$name, 17, 21)=="PFERD"], 4, 6))
sr0$file[substr(sr0$name, 17, 22)=="HIRSCH"] <- paste0(substr(sr0$name[substr(sr0$name, 17, 22)=="HIRSCH"], 1, 2), "H", substr(sr0$name[substr(sr0$name, 17, 22)=="HIRSCH"], 4, 6))
sr0$file[substr(sr0$name, 17, 24)=="SCHWALBE"] <- paste0(substr(sr0$name[substr(sr0$name, 17, 24)=="SCHWALBE"], 1, 2), "S", substr(sr0$name[substr(sr0$name, 17, 24)=="SCHWALBE"], 4, 6))

corrected <- sr0$file

sr0 <- sr0 %>% select(2:6)

sr1 <- read.csv(paste0(folder, "tableBandPass.csv")) # using the speech rates calculated after band passing the files; there are two other options (see speech rate Praat script)

sr1 <- sr1 %>%
  select(c(1, 4:7)) %>%
  set_names(c("name", "durationSpeech", "phonTime", "speechRate", "articRate")) %>%
  mutate(file = substr(name, 1, 6)) %>%
  filter(substr(name, 1, 2)!="BR") %>%
  filter(file %!in% corrected) %>%
  filter(substr(file, 2, 2) !="-") # exclude the confederate's files

sr1$file[substr(sr1$name, 16, 21)=="_alone"] <- paste0(substr(sr1$name[substr(sr1$name, 16, 21)=="_alone"], 1, 2), "A", substr(sr1$name[substr(sr1$name, 16, 21)=="_alone"], 4, 6))
sr1$file[substr(sr1$name, 16, 21)=="_joint"] <- paste0(substr(sr1$name[substr(sr1$name, 16, 21)=="_joint"], 1, 2), "J", substr(sr1$name[substr(sr1$name, 16, 21)=="_joint"], 4, 6))

sr1 <- sr1 %>% select(2:6)

sr2 <- read.csv(paste0(folder, "confederateBandPass.csv"))

sr2 <- sr2 %>%
  select(c(1, 4:7)) %>%
  set_names(c("name", "durationSpeech", "phonTime", "speechRate", "articRate")) %>%
  mutate(file = substr(name, 1, 6))

confRA <- c("H-Hirsch_alone", "H-Pferd_alone", "H-Schwalbe_alone", "L-Hirsch_alone", "L-Pferd_alone", "L-Schwalbe_alone", "S-Hirsch_alone", "S-Pferd_alone", "S-Schwalbe_alone")
confRJ <- c("H-Hirsch_joint", "H-Pferd_joint", "H-Schwalbe_joint", "L-Hirsch_joint", "L-Pferd_joint", "L-Schwalbe_joint", "S-Hirsch_joint", "S-Pferd_joint", "S-Schwalbe_joint")

sr2$file[sr2$name %in% confRA] <- paste0(substr(sr2$name[sr2$name %in% confRA], 1, 1), "A", substr(sr2$name[sr2$name %in% confRA], 3, 6))
sr2$file[sr2$name %in% confRJ] <- paste0(substr(sr2$name[sr2$name %in% confRJ], 1, 1), "J", substr(sr2$name[sr2$name %in% confRJ], 3, 6))

sr2 <- sr2 %>% select(2:6)

sr3 <- rbind(sr0, sr1)
sr <- rbind(sr3, sr2)
sr$pauseDur <- sr$duration - sr$phonTime

####################################### FROM TRANSCRIPTION

### IPUs, PAUSES

ld <- data.frame(matrix(ncol=8, nrow=0))
names(ld) <- c("IPUDur", "IPU", "file", "syll", "speechRateSyll", "durationSpeechManual", "pause", "pauseDurManual")

# we can only get this info for free speech, so:

for(i in listTGf){
  tg <- tg.read(paste0(folder, i), encoding=detectEncoding(paste0(folder, i)))
  inter <- data.frame(matrix(ncol=2, nrow=0))
  names(inter) <- c("label", "duration")
  for(n in 1:tg.getNumberOfIntervals(tg, 1)){
    if(tg.getIntervalStartTime(tg, 1, n) >= tg.getIntervalStartTime(tg, 2, 2) & tg.getIntervalEndTime(tg, 1, n) <= tg.getIntervalEndTime(tg, 2, 2)){
      inter[nrow(inter)+1,] <- c(tg.getLabel(tg, 1, n), as.numeric(tg.getIntervalDuration(tg, 1, n)))
    }
  }
  inter <- inter %>% filter(label != "<usb>")
  inter$label[inter$label %in% c(" ", "  ", "   ", "    ")] <- ""
  ipu <- inter %>%
    filter(label != "") %>%
    mutate(IPU = 1:nrow(inter %>% filter(label != "")),
           file = substr(i, 1, 6)) %>%
    rename("IPUDur"="duration")
  ipu$syll <- (hyphen(ipu$label, hyph.pattern="de")@hyphen)$syll
  ipu$speechRateSyll <- ipu$syll / as.numeric(ipu$IPUDur)
  ipu$durationSpeechManual = as.numeric(tg.getIntervalEndTime(tg, 1, tg.getNumberOfIntervals(tg, 1))) - as.numeric(tg.getIntervalStartTime(tg, 1, 1))
  ipu$label <- NULL
  paus <- inter %>%
    filter(label == "") %>%
    rename("pause"="label",
           "pauseDurManual" = "duration") %>%
    mutate(pause = 1:nrow(inter %>% filter(label == "")))
  paus[nrow(paus)+1,] <- NA # because there will always be one less pause than IPUs, and we need the same number of rows
  ip <- cbind(ipu, paus)
  ld <- rbind(ld, ip)
}

syllables <- aggregate(ld$syll, list(ld$file), FUN=sum)
names(syllables) <- c("file", "syll")
c <- ld[!duplicated(ld$file), c(3,6)]
SRS <- merge(syllables, c, by="file")
SRS$SRsyll <- SRS$syll/SRS$durationSpeechManual

com <- merge(sr, SRS, by="file")
com <- com[, c(1, 4, 9)]
com$diff <- com$speechRate - com$SRsyll
hist(com$diff)

# now we have the duration of each IPU and each pause in free speech (manually annotated)

##################################################################


####################################### FROM AMPLITUDE ENVELOPE

folderp <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Confederate/AudioAlignedToBreathing/"

csv <- list.files(folderp, "csv")
t <- list.files(folderp, "TextGrid")
lf <- as.data.frame(cbind(csv, t))

i=1

# make this loop work (something not working in the middle, haven't gone through all of it yet, i'm fucking tired.)

for(i in 1:nrow(lf)){
  c <- read.csv(paste0(folderp, lf$csv[i]))
  t <- tg.read(paste0(folderp, lf$t[i]), encoding=detectEncoding(paste0(folderp, lf$t[i])))
  inter <- data.frame(matrix(ncol=4, nrow=0))
  names(inter) <- c("label", "onset", "offset", "duration")
  for(n in 1:tg.getNumberOfIntervals(t, 1)){
    inter[nrow(inter)+1,] <- c(tg.getLabel(t, 1, n),
                               as.numeric(tg.getIntervalStartTime(t, 1, n)),
                               as.numeric(tg.getIntervalEndTime(t, 1, n)),
                               as.numeric(tg.getIntervalDuration(t, 1, n)))
  }
  # inter <- inter %>% filter(label != "<usb>")
  inter$label[inter$label %in% c(" ", "  ", "   ", "    ")] <- ""
  ipu <- inter %>%
    filter(label != "") %>%
    mutate(IPU = 1:nrow(inter %>% filter(label != "")),
           file = substr(i, 1, 6)) %>%
    rename("IPUDur"="duration")
  ipu$syll <- (hyphen(ipu$label, hyph.pattern="de")@hyphen)$syll
  ipu$speechRateSyll <- ipu$syll / as.numeric(ipu$IPUDur)
  ipu$durationSpeechManual = as.numeric(tg.getIntervalEndTime(tg, 1, tg.getNumberOfIntervals(tg, 1))) - as.numeric(tg.getIntervalStartTime(tg, 1, 1))
  ipu$label <- NULL
  paus <- inter %>%
    filter(label == "") %>%
    rename("pause"="label",
           "pauseDurManual" = "duration") %>%
    mutate(pause = 1:nrow(inter %>% filter(label == "")))
  paus[nrow(paus)+1,] <- NA # because there will always be one less pause than IPUs, and we need the same number of rows
  ip <- cbind(ipu, paus)
  ld <- rbind(ld, ip)
}

for(i in listTGf){
  tg <- tg.read(paste0(folder, i), encoding=detectEncoding(paste0(folder, i)))
  inter <- data.frame(matrix(ncol=2, nrow=0))
  names(inter) <- c("label", "duration")
  for(n in 1:tg.getNumberOfIntervals(tg, 1)){
    if(tg.getIntervalStartTime(tg, 1, n) >= tg.getIntervalStartTime(tg, 2, 2) & tg.getIntervalEndTime(tg, 1, n) <= tg.getIntervalEndTime(tg, 2, 2)){
      inter[nrow(inter)+1,] <- c(tg.getLabel(tg, 1, n), as.numeric(tg.getIntervalDuration(tg, 1, n)))
    }
  }
  inter <- inter %>% filter(label != "<usb>")
  inter$label[inter$label %in% c(" ", "  ", "   ", "    ")] <- ""
  ipu <- inter %>%
    filter(label != "") %>%
    mutate(IPU = 1:nrow(inter %>% filter(label != "")),
           file = substr(i, 1, 6)) %>%
    rename("IPUDur"="duration")
  ipu$syll <- (hyphen(ipu$label, hyph.pattern="de")@hyphen)$syll
  ipu$speechRateSyll <- ipu$syll / as.numeric(ipu$IPUDur)
  ipu$durationSpeechManual = as.numeric(tg.getIntervalEndTime(tg, 1, tg.getNumberOfIntervals(tg, 1))) - as.numeric(tg.getIntervalStartTime(tg, 1, 1))
  ipu$label <- NULL
  paus <- inter %>%
    filter(label == "") %>%
    rename("pause"="label",
           "pauseDurManual" = "duration") %>%
    mutate(pause = 1:nrow(inter %>% filter(label == "")))
  paus[nrow(paus)+1,] <- NA # because there will always be one less pause than IPUs, and we need the same number of rows
  ip <- cbind(ipu, paus)
  ld <- rbind(ld, ip)
}

syllables <- aggregate(ld$syll, list(ld$file), FUN=sum)
names(syllables) <- c("file", "syll")
c <- ld[!duplicated(ld$file), c(3,6)]
SRS <- merge(syllables, c, by="file")
SRS$SRsyll <- SRS$syll/SRS$durationSpeechManual

com <- merge(sr, SRS, by="file")
com <- com[, c(1, 4, 9)]
com$diff <- com$speechRate - com$SRsyll
hist(com$diff)

############# f0 mean, median

# 2

listTXT <- list.files(folder, pattern=".txt")
confF <- c("H-Hobbies.txt", "H-Holidays.txt", "H-Home.txt", "L-Hobbies.txt", "L-Holidays.txt", "L-Home.txt", "S-Hobbies.txt", "S-Holidays.txt", "S-Home.txt")
listTXTf <- listTXT[substr(listTXT, 2, 2)=="F" | listTXT %in% confF]
confRA <- c("H-Hirsch_alone.txt", "H-Pferd_alone.txt", "H-Schwalbe_alone.txt", "L-Hirsch_alone.txt", "L-Pferd_alone.txt", "L-Schwalbe_alone.txt", "S-Hirsch_alone.txt", "S-Pferd_alone.txt", "S-Schwalbe_alone.txt")
confRJ <- c("H-Hirsch_joint.txt", "H-Pferd_joint.txt", "H-Schwalbe_joint.txt", "L-Hirsch_joint.txt", "L-Pferd_joint.txt", "L-Schwalbe_joint.txt", "S-Hirsch_joint.txt", "S-Pferd_joint.txt", "S-Schwalbe_joint.txt")
f <- c("BF", "HF", "LF", "SF")
readJ <- list.files(folder, pattern="joint.txt")
readJ <- readJ[substr(readJ, 2, 2) != "-"]
readA <- list.files(folder, pattern="alone.txt")
readA <- readA[substr(readA, 2, 2) != "-"]

listTG <- list.files(folder, pattern=".TextGrid")
confFtg <- c("H-Hobbies.TextGrid", "H-Holidays.TextGrid", "H-Home.TextGrid", "L-Hobbies.TextGrid", "L-Holidays.TextGrid", "L-Home.TextGrid", "S-Hobbies.TextGrid", "S-Holidays.TextGrid", "S-Home.TextGrid")
listTGf <- listTG[substr(listTG, 2, 2)=="F" | listTG %in% confFtg]

listTxg <- as.data.frame(cbind(listTXTf, listTGf))
colnames(listTxg) <- c("txt", "tg")
listTxg$worked[substr(listTxg$txt, 1, 6)==substr(listTxg$tg, 1, 6)] <- "worked!"
listTxg$worked[substr(listTxg$txt, 1, 6)!=substr(listTxg$tg, 1, 6)] <- "NO!!!!!!!!!"
unique(listTxg$worked) # make sure all the TXT and Textgrid files are matching in each row

ff <- data.frame(matrix(ncol=4, nrow=0))
colnames(ff) <- c("IPU", "f0mean", "file")

# for free speech: f0 mean per IPU

for(i in 1:nrow(listTxg)){
  txt <- read.table(paste0(folder, listTxg$txt[i]), header=TRUE) # load file with f0 means
  txt$f0mean <- as.numeric(txt$f0mean)
  tg <- tg.read(paste0(folder, listTxg$tg[i]), encoding=detectEncoding(paste0(folder, listTxg$tg[i]))) # load textgrid with defined IPUs
  
  # create object ("intervals") with the index of each IPU and its onset and offset time
  IPUtimes <- data.frame(matrix(ncol=3, nrow=0))
  colnames(IPUtimes) <- c("IPU", "start", "end")
  labels <- data.frame(matrix(ncol=2, nrow=0))
  colnames(labels) <- c("IPU", "label")
  for(n in 1:tg.getNumberOfIntervals(tg, 1)){
    labels[nrow(labels)+1,] <- c(n, tg.getLabel(tg, 1, n))
  }
  labels <- labels %>% filter(label!="")
  for(n in labels$IPU){
    IPUtimes[nrow(IPUtimes)+1,] <- c(n, tg.getIntervalStartTime(tg, 1, as.numeric(n)), tg.getIntervalEndTime(tg, 1, as.numeric(n)))
  }
  
  # get f0 for each IPU, i.e. the mean of all f0 values within the period of each IPU
  f0 <- data.frame(matrix(ncol=2, nrow=0))
  colnames(f0) <- c("IPU", "f0mean")
  for(p in 1:nrow(IPUtimes)){
    for(n in 1:nrow(txt)){
      if(txt$onset[n] >= IPUtimes$start[p] & txt$offset[n] <= IPUtimes$end[p]){
        f0[nrow(f0)+1,] <- c(IPUtimes$IPU[p], txt$f0mean[n])
      }
    }
  }
  f0$IPU <- as.numeric(f0$IPU)
  f0$f0mean <- as.numeric(f0$f0mean)
  f0perIPU <- aggregate(f0$f0mean, list(f0$IPU), FUN=mean)
  colnames(f0perIPU) <- c("IPU", "f0mean")
  f0perIPU$IPU <- 1:nrow(f0perIPU)
  f0perIPU$file <- substr(listTxg$txt[i], 1, 6)
  ff <- rbind(ff, f0perIPU)
}

ff <- ff %>% select(file, IPU, f0mean)

fmeans <- aggregate(ff$f0mean, list(ff$file), FUN=mean)
names(fmeans) <- c("file", "f0IPUmean")
ff <- merge(ff, fmeans, by="file")

listTXTr <- listTXT[listTXT %!in% listTXTf]

# for reading files: no IPU separation:

for (i in listTXTr){
  txt <- read.table(paste0(folder, i), header=TRUE)
  txt$f0mean <- as.numeric(txt$f0mean)
  txt$zmean <- (txt$f0mean - mean(txt$f0mean))/sd(txt$f0mean) # get z scores
  txt <- txt[txt$zmean < 3,] # keep values below 3 SDs (there don't seem to be any low outliers, only high ones)
  if (i %in% confRA){ # confederate's reading files, alone section
    i <- paste0(substr(i, 1, 1), "A", substr(i, 3, 6))
  } else if (i %in% readA){ # participants' reading files, alone section
    i <- paste0(substr(i, 1, 2), "A", substr(i, 4, 6))
  } else if (i %in% confRJ){ # confederate's reading files, joint section
    i <- paste0(substr(i, 1, 1), "J", substr(i, 3, 6))
  } else if (i %in% readJ){ # participants' reading files, joint section
    i <- paste0(substr(i, 1, 2), "J", substr(i, 4, 6))
  } else if(substr(i, 17, 21)=="PFERD"){
    i <- paste0(substr(i, 1, 2), "P", substr(i, 4, 6))
  } else if(substr(i, 17, 22)=="HIRSCH"){
    i <- paste0(substr(i, 1, 2), "H", substr(i, 4, 6))
  } else if(substr(i, 17, 24)=="SCHWALBE"){
    i <- paste0(substr(i, 1, 2), "S", substr(i, 4, 6))
  }
  ff[nrow(ff)+1,] <- c(i, NA, mean(txt$f0mean), mean(txt$f0mean))
}

ff$file <- as.factor(ff$file)
ff$IPU <- as.factor(ff$IPU)
ff$f0mean <- as.numeric(ff$f0mean)

##################################################################



#### BREATHING DATA

# number of textgrid files == number of wav files: [H,L,S][B, F]; 

folder2 <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/PeaksValleys/"

listBREATH <- list.files(folder2, pattern="SUM")
listBREATHr <- listBREATH[substr(listBREATH, 2, 2)=="R"]
listBREATHf <- listBREATH[substr(listBREATH, 2, 2)=="F" & grepl("TextGrid", listBREATH) & !grepl("breathL", listBREATH)]
listBREATHlb <- listBREATH[grepl("breathL", listBREATH) | substr(listBREATH, 2, 2) == "B"] # breathing during listening and during baseline periods (i.e. just watching the confedearate sitting/biking in silence)

listTG <- list.files(folder, pattern=".TextGrid")
listTGf <- listTG[substr(listTG, 2, 2)=="F"]

listTGf <- listTGf[substr(listTGf, 1, 6) %in% substr(listBREATHf, 1, 6)]

listBGf <- as.data.frame(cbind(listBREATHf, listTGf))
colnames(listBGf) <- c("breath", "tg")
listBGf$worked[substr(listBGf$breath, 1, 6)==substr(listBGf$tg, 1, 6)] <- "worked!"
listBGf$worked[substr(listBGf$breath, 1, 6)!=substr(listBGf$tg, 1, 6)] <- "NO!!!!!!!!!"
table(listBGf$worked) # make sure all the TXT and Textgrid files are matching in each row

pbr <- data.frame(matrix(ncol=3, nrow=0))
colnames(pbr) <- c("file", "peaksSpeech", "valleysSpeech")


############################

# listT <- list.files(folder2, "TextGrid")
# listT <- listT[substr(listT, 2, 2) =="F"]
# listW <- list.files(folder2, "wav")
# listW <- listW[!grepl("TextGrid", listW)]
# listW <- listW[substr(listW, 2, 2) =="F"]

############################


### GETTING DURATION OF BREATHING CYCLES AND NUMBER OF IPUs PER CYCLE

# first, check if number of valleys = number of peaks + 1 in each file

PV <- data.frame(matrix(ncol=3, nrow=0))
names(PV) <- c("file", "peaks", "valleys")

for(i in 1:nrow(listBGf)){
  breath <- tg.read(paste0(folder2, listBGf$breath[i]))
  PV[nrow(PV)+1,] <- c(listBGf[i, 1], tg.getNumberOfPoints(breath, 1), tg.getNumberOfPoints(breath, 2))
}
PV$peaks <- as.numeric(PV$peaks)
PV$valleys <- as.numeric(PV$valleys)
pointsok <- PV$file[PV$peaks == (PV$valleys - 1)]
PV$file[PV$file %!in% pointsok] # the output here should be zero

listBGf <- listBGf %>% filter(breath %in% pointsok) # make sure to get only the textgrids with one more valley than peaks (each cycle is from valley to valley)

# i=1

for(i in 1:nrow(listBGf)){
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
    filter(label!="") %>% # exclude silences
    filter(start >= tg.getIntervalStartTime(tg, 2, 2)) # exclude everything before the "task" section
  IPUtimes$IPU <- 1:nrow(IPUtimes)

  # get time of each point (peaks and valleys)
  PVtimes <- data.frame(matrix(ncol=4, nrow=0))
  colnames(PVtimes) <- c("cycle", "onset", "peak", "offset")
  for(t in 1:(tg.getNumberOfPoints(breath, 2)-1)){ # number of valleys minus one (the last valley is only counted as offset of its previous cycle)
    PVtimes[nrow(PVtimes)+1,] <- c(t, # number of breathing cycle
                                   (tg.getPointTime(breath, 2, t) + tg.getIntervalStartTime(tg, 3, 2)), # saving the time of each point plus the start time of the first IPU. this is because the breathing textgrid starts at time 0, and the first IPU of the transcript textgrid starts at time x
                                   (tg.getPointTime(breath, 1, t) + tg.getIntervalStartTime(tg, 3, 2)),
                                   (tg.getPointTime(breath, 2, t+1) + tg.getIntervalStartTime(tg, 3, 2))) # next valley
  }
  PVtimes[, c("onset", "peak", "offset")] <- lapply(PVtimes[, c("onset", "peak", "offset")], as.numeric)
  PVtimes$cycleDur <- PVtimes$offset - PVtimes$onset # duration of each cycle
  PVtimes$cycleOK[PVtimes$onset < PVtimes$peak & PVtimes$peak < PVtimes$offset] <- "OK!"
  PVtimes$cycleOK[PVtimes$cycleOK=="NA"] <- "Not OK!"
  unique(PVtimes$cycleOK) # make sure that onset < peak < offset (i.e., the cycles make sense)

  # get number of IPUs within each cycle
  ic <- data.frame(matrix(ncol=2, nrow=0))
  names(ic) <- c("cycle", "IPU")
  for(t in 1:nrow(PVtimes)){
    for(n in 1:nrow(IPUtimes)){
      if(IPUtimes$start[n] >= PVtimes$onset[t] & IPUtimes$start[n] <= PVtimes$offset[t]){
        ic[nrow(ic)+1,] <- c(PVtimes$cycle[t], IPUtimes$IPU[n])
      }
    }
  }
}


################

for(i in listBREATH){
  tg <- tg.read(paste0(folder2, i))
  if(i %in% listBREATHr){
    if(grepl("alone", i)){
      i <- paste0(substr(i, 1, 2), "A", substr(i, 4, 6))
    } else if(grepl("joint", i)){
      i <- paste0(substr(i, 1, 2), "J", substr(i, 4, 6))
    } else if(grepl("PFERD", i)){
      i <- paste0(substr(i, 1, 2), "P", substr(i, 4, 6))
    } else if(grepl("SCHWALBE", i)){
      i <- paste0(substr(i, 1, 2), "S", substr(i, 4, 6))
    } else if(grepl("HIRSCH", i)){
      i <- paste0(substr(i, 1, 2), "H", substr(i, 4, 6))
    }
  } else if (i %in% listBREATHf){
    i <- paste0(substr(i, 1, 6))
  }
  pbr[nrow(pbr)+1,] <- c(i, tg.getNumberOfPoints(tg, 1), tg.getNumberOfPoints(tg, 2))
}

#### confederate's breathing data

folder2 <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/PeaksValleys/"
folderCorrectedTransc <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/Confederate/CorrectedTextGridsWithTranscripts/"

listCB <- list.files(folder2, pattern="THORAX") # we use the measurement from the thorax only for the confederate
listCB <- listCB[grepl(".TextGrid", listCB)]
listCBr <- listCB[grepl("alone", listCB)|grepl("joint", listCB)]
listCBf <- listCB[grepl("Holidays", listCB)|grepl("Hobbies", listCB)|grepl("Home", listCB)]

cbr <- data.frame(matrix(ncol=3, nrow=0))
names(cbr) <- c("file", "peaksSpeech", "valleysSpeech")

# listTG <- list.files(folder, pattern=".TextGrid")
# # confFtg <- c("H-Hobbies.TextGrid", "H-Holidays.TextGrid", "H-Home.TextGrid", "L-Hobbies.TextGrid", "L-Holidays.TextGrid", "L-Home.TextGrid", "S-Hobbies.TextGrid", "S-Holidays.TextGrid", "S-Home.TextGrid")
# listTGcf <- listTG[grepl("Holidays", listTG)|grepl("Hobbies", listTG)|grepl("Home", listTG)]

### GETTING DURATION OF BREATHING CYCLES AND NUMBER OF IPUs PER CYCLE
# (not yet)

listSHIFTED <- list.files(folderCorrectedTransc, "shifted")

# see if number of peaks = number of valleys - 1
PV <- data.frame(matrix(ncol=3, nrow=0))
names(PV) <- c("file", "peaks", "valleys")

for(i in listCBf){
  breath <- tg.read(paste0(folder2, i))
  PV[nrow(PV)+1,] <- c(i, tg.getNumberOfPoints(breath, 1), tg.getNumberOfPoints(breath, 2))
}
PV$peaks <- as.numeric(PV$peaks)
PV$valleys <- as.numeric(PV$valleys)
PV$pointsok[PV$peaks == PV$valleys - 1] <- "OK!"
table(PV$pointsok, useNA = "ifany")
pointsok <- PV$file[PV$peaks == PV$valleys - 1]

listCBf <- listCBf[listCBf %in% pointsok]
#

listCBTf <- as.data.frame(cbind(listCBf, listSHIFTED))
names(listCBTf) <- c("breath", "tg")
listCBTf$worked[substr(listCBTf$breath, 1, 6)==substr(listCBTf$tg, 1, 6)] <- "worked!"
listCBTf$worked[substr(listCBTf$breath, 1, 6)!=substr(listCBTf$tg, 1, 6)] <- "NO!!!!!!!!!"
unique(listCBTf$worked) # make sure all the TXT and Textgrid files are matching in each row

for(i in 1:nrow(listCBTf)){
  breath <- tg.read(paste0(folder2, listCBTf$breath[i]))
  tg <- tg.read(paste0(folder, listCBTf$tg[i]), encoding=detectEncoding(paste0(folder, listCBTf$tg[i])))

  # get onset and offset of each interval (IPU)
  IPUtimes <- data.frame(matrix(ncol=4, nrow=0))
  colnames(IPUtimes) <- c("IPU", "label", "start", "end")
  for(n in 1:tg.getNumberOfIntervals(tg, 1)){
    IPUtimes[nrow(IPUtimes)+1,] <- c(n,
                                     tg.getLabel(tg, 1, n),
                                     tg.getIntervalStartTime(tg, 1, n),
                                     tg.getIntervalEndTime(tg, 1, n))
  }
  IPUtimes <- IPUtimes %>% filter(label!="")
  IPUtimes$IPU <- 1:nrow(IPUtimes)


  # get time of each point (peaks and valleys)
  PVtimes <- data.frame(matrix(ncol=4, nrow=0))
  colnames(PVtimes) <- c("cycle", "onset", "peak", "offset")
  for(t in 1:(tg.getNumberOfPoints(breath, 1))){ # for the number of peaks
      PVtimes[nrow(PVtimes)+1,] <- c(t, # number of breathing cycle
                                     tg.getPointTime(breath, 2, t) - tg.getIntervalStartTime(tg, 1, 2),
                                     tg.getPointTime(breath, 1, t) - tg.getIntervalStartTime(tg, 1, 2),
                                     tg.getPointTime(breath, 2, t+1) - tg.getIntervalStartTime(tg, 1, 2)) # next valley
  }
  PVtimes[, c("onset", "peak", "offset")] <- lapply(PVtimes[, c("onset", "peak", "offset")], as.numeric)
  PVtimes$cycleDur <- PVtimes$offset - PVtimes$onset # duration of each cycle
  PVtimes$cycleOK[PVtimes$onset < PVtimes$peak & PVtimes$peak < PVtimes$offset] <- "OK!"
  PVtimes$cycleOK[PVtimes$cycleOK=="NA"] <- "Not OK!"
  table(PVtimes$cycleOK, useNA="ifany") # make sure that onset < peak < offset (i.e., the cycles make sense)

  # get number of IPUs within each cycle
  ic <- data.frame(matrix(ncol=2, nrow=0))
  names(ic) <- c("cycle", "IPU")
  for(t in 1:nrow(PVtimes)){
    for(n in 1:nrow(IPUtimes)){
      if(IPUtimes$end[n] >= PVtimes$onset[t] & IPUtimes$start[n] <= PVtimes$offset[t]){ # using IPU$start <= cycle$offset because it can be that the IPU was annotated as ending later than the cycle (manually)
        ic[nrow(ic)+1,] <- c(PVtimes$cycle[t], IPUtimes$IPU[n])
      }
}
  }
}

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

for(i in listCB){
  tg <- tg.read(paste0(folder2, i))
  cbr[nrow(cbr)+1,] <- c(i, tg.getNumberOfPoints(tg, 1), tg.getNumberOfPoints(tg, 2))
}

cbr <- cbr %>%
  mutate(file = gsub("_200.TextGrid", "", file)) %>%
  mutate(file = gsub("_THORAX", "", file))

cbr$fileOG <- cbr$file
cbr$file <- paste0(substr(cbr$file, 1, 6))
cbr$file[grepl("alone", cbr$fileOG)] <- paste0(substr(cbr$fileOG[grepl("alone", cbr$fileOG)], 1, 1), "A", substr(cbr$fileOG[grepl("alone", cbr$fileOG)], 3, 6))
cbr$file[grepl("joint", cbr$fileOG)] <- paste0(substr(cbr$fileOG[grepl("joint", cbr$fileOG)], 1, 1), "J", substr(cbr$fileOG[grepl("joint", cbr$fileOG)], 3, 6))
cbr$fileOG <- NULL

#### join confederate's with participants' breathing data

br <- rbind(cbr, pbr)

##################################################################


# 3

bf <- merge(ff, br, by="file", all=TRUE)

fs <- merge(bf, sr, by="file", all=TRUE)

conffiles <- c("irs", "obb", "oli", "ome", "fer", "chw")

fs$Speaker <- substr(fs$file, 4, 6)
fs$Speaker[fs$Speaker %in% conffiles] <- "Confederate"

fs$Condition <- substr(fs$file, 1, 1)
fs$Condition[fs$Condition=="B"] <- "Baseline"
fs$Condition[fs$Condition=="H"] <- "Heavy"
fs$Condition[fs$Condition=="L"] <- "Light"
fs$Condition[fs$Condition=="S"] <- "Sitting"

fs$Condition <- as.factor(fs$Condition)

fs$Task[substr(fs$file, 2, 2) == "F"] <- "Free"
fs$Task[substr(fs$file, 2, 3) == "RA"] <- "ReadAlone"
fs$Task[substr(fs$file, 2, 3) == "RJ"] <- "ReadJoint"
fs$Task[substr(fs$file, 1, 2) == "BR"] <- "ReadAlone" # baseline reading is also alone
fs$Task[fs$Speaker == "Confederate" & substr(fs$file, 2, 2) == "A"] <- "ReadAlone"
fs$Task[fs$Speaker == "Confederate" & substr(fs$file, 2, 2) == "J"] <- "ReadJoint"
fs$Task[fs$Speaker == "Confederate" & substr(fs$file, 2, 2) == "-"] <- "Free"

fs$Task <- as.factor(fs$Task)

meta <- read.csv("C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/metadata.csv", fileEncoding="UTF-8-BOM")
names(meta)[names(meta) == "Participant"] <- "Speaker"

dat <- merge(fs, meta, by="Speaker", all=TRUE)

dat$Order[dat$Condition=="Baseline"] <- 0

dat$Order[dat$List==1 & dat$Condition=="Sitting" & dat$Task=="ReadAlone"] <- 1
dat$Order[dat$List==1 & dat$Condition=="Sitting" & dat$Task=="ReadJoint"] <- 2
dat$Order[dat$List==1 & dat$Condition=="Sitting" & dat$Task=="Free"] <- 3
dat$Order[dat$List==1 & dat$Condition=="Light" & dat$Task=="Free"] <- 4
dat$Order[dat$List==1 & dat$Condition=="Light" & dat$Task=="ReadAlone"] <- 5
dat$Order[dat$List==1 & dat$Condition=="Light" & dat$Task=="ReadJoint"] <- 6
dat$Order[dat$List==1 & dat$Condition=="Heavy" & dat$Task=="Free"] <- 7
dat$Order[dat$List==1 & dat$Condition=="Heavy" & dat$Task=="ReadAlone"] <- 8
dat$Order[dat$List==1 & dat$Condition=="Heavy" & dat$Task=="ReadJoint"] <- 9

dat$Order[dat$List==2 & dat$Condition=="Heavy" & dat$Task=="ReadAlone"] <- 1
dat$Order[dat$List==2 & dat$Condition=="Heavy" & dat$Task=="ReadJoint"] <- 2
dat$Order[dat$List==2 & dat$Condition=="Heavy" & dat$Task=="Free"] <- 3
dat$Order[dat$List==2 & dat$Condition=="Sitting" & dat$Task=="Free"] <- 4
dat$Order[dat$List==2 & dat$Condition=="Sitting" & dat$Task=="ReadAlone"] <- 5
dat$Order[dat$List==2 & dat$Condition=="Sitting" & dat$Task=="ReadJoint"] <- 6
dat$Order[dat$List==2 & dat$Condition=="Light" & dat$Task=="Free"] <- 7
dat$Order[dat$List==2 & dat$Condition=="Light" & dat$Task=="ReadAlone"] <- 8
dat$Order[dat$List==2 & dat$Condition=="Light" & dat$Task=="ReadJoint"] <- 9

dat$Order[dat$List==3 & dat$Condition=="Light" & dat$Task=="ReadAlone"] <- 1
dat$Order[dat$List==3 & dat$Condition=="Light" & dat$Task=="ReadJoint"] <- 2
dat$Order[dat$List==3 & dat$Condition=="Light" & dat$Task=="Free"] <- 3
dat$Order[dat$List==3 & dat$Condition=="Heavy" & dat$Task=="ReadAlone"] <- 4
dat$Order[dat$List==3 & dat$Condition=="Heavy" & dat$Task=="ReadJoint"] <- 5
dat$Order[dat$List==3 & dat$Condition=="Heavy" & dat$Task=="Free"] <- 6
dat$Order[dat$List==3 & dat$Condition=="Sitting" & dat$Task=="ReadAlone"] <- 7
dat$Order[dat$List==3 & dat$Condition=="Sitting" & dat$Task=="ReadJoint"] <- 8
dat$Order[dat$List==3 & dat$Condition=="Sitting" & dat$Task=="Free"] <- 9

dat$Order[dat$List==4 & dat$Condition=="Sitting" & dat$Task=="ReadAlone"] <- 1
dat$Order[dat$List==4 & dat$Condition=="Sitting" & dat$Task=="ReadJoint"] <- 2
dat$Order[dat$List==4 & dat$Condition=="Sitting" & dat$Task=="Free"] <- 3
dat$Order[dat$List==4 & dat$Condition=="Heavy" & dat$Task=="Free"] <- 4
dat$Order[dat$List==4 & dat$Condition=="Heavy" & dat$Task=="ReadAlone"] <- 5
dat$Order[dat$List==4 & dat$Condition=="Heavy" & dat$Task=="ReadJoint"] <- 6
dat$Order[dat$List==4 & dat$Condition=="Light" & dat$Task=="ReadAlone"] <- 7
dat$Order[dat$List==4 & dat$Condition=="Light" & dat$Task=="ReadJoint"] <- 8
dat$Order[dat$List==4 & dat$Condition=="Light" & dat$Task=="Free"] <- 9

dat$Order[dat$List==5 & dat$Condition=="Light" & dat$Task=="ReadAlone"] <- 1
dat$Order[dat$List==5 & dat$Condition=="Light" & dat$Task=="ReadJoint"] <- 2
dat$Order[dat$List==5 & dat$Condition=="Light" & dat$Task=="Free"] <- 3
dat$Order[dat$List==5 & dat$Condition=="Sitting" & dat$Task=="Free"] <- 4
dat$Order[dat$List==5 & dat$Condition=="Sitting" & dat$Task=="ReadAlone"] <- 5
dat$Order[dat$List==5 & dat$Condition=="Sitting" & dat$Task=="ReadJoint"] <- 6
dat$Order[dat$List==5 & dat$Condition=="Heavy" & dat$Task=="ReadAlone"] <- 7
dat$Order[dat$List==5 & dat$Condition=="Heavy" & dat$Task=="ReadJoint"] <- 8
dat$Order[dat$List==5 & dat$Condition=="Heavy" & dat$Task=="Free"] <- 9

dat$Order[dat$List==6 & dat$Condition=="Heavy" & dat$Task=="ReadAlone"] <- 1
dat$Order[dat$List==6 & dat$Condition=="Heavy" & dat$Task=="ReadJoint"] <- 2
dat$Order[dat$List==6 & dat$Condition=="Heavy" & dat$Task=="Free"] <- 3
dat$Order[dat$List==6 & dat$Condition=="Light" & dat$Task=="ReadAlone"] <- 4
dat$Order[dat$List==6 & dat$Condition=="Light" & dat$Task=="ReadJoint"] <- 5
dat$Order[dat$List==6 & dat$Condition=="Light" & dat$Task=="Free"] <- 6
dat$Order[dat$List==6 & dat$Condition=="Sitting" & dat$Task=="Free"] <- 7
dat$Order[dat$List==6 & dat$Condition=="Sitting" & dat$Task=="ReadAlone"] <- 8
dat$Order[dat$List==6 & dat$Condition=="Sitting" & dat$Task=="ReadJoint"] <- 9

read <- c("ReadAlone", "ReadJoint")

dat$Topic[dat$Condition == "Baseline" & dat$Task == "Free"] <- "Food"

dat$Topic[dat$List == 1 & dat$Condition == "Sitting" & dat$Task %in% read] <- "Hirsch"
dat$Topic[dat$List == 1 & dat$Condition == "Sitting" & dat$Task == "Free"] <- "Holidays"
dat$Topic[dat$List == 1 & dat$Condition == "Light" & dat$Task %in% read] <- "Schwalbe"
dat$Topic[dat$List == 1 & dat$Condition == "Light" & dat$Task == "Free"] <- "Hobbies"
dat$Topic[dat$List == 1 & dat$Condition == "Heavy" & dat$Task %in% read] <- "Pferd"
dat$Topic[dat$List == 1 & dat$Condition == "Heavy" & dat$Task == "Free"] <- "Home"

dat$Topic[dat$List == 2 & dat$Condition == "Heavy" & dat$Task %in% read] <- "Schwalbe"
dat$Topic[dat$List == 2 & dat$Condition == "Heavy" & dat$Task == "Free"] <- "Home"
dat$Topic[dat$List == 2 & dat$Condition == "Sitting" & dat$Task %in% read] <- "Pferd"
dat$Topic[dat$List == 2 & dat$Condition == "Sitting" & dat$Task == "Free"] <- "Holidays"
dat$Topic[dat$List == 2 & dat$Condition == "Light" & dat$Task %in% read] <- "Hirsch"
dat$Topic[dat$List == 2 & dat$Condition == "Light" & dat$Task == "Free"] <- "Hobbies"

dat$Topic[dat$List == 3 & dat$Condition == "Light" & dat$Task %in% read] <- "Pferd"
dat$Topic[dat$List == 3 & dat$Condition == "Light" & dat$Task == "Free"] <- "Hobbies"
dat$Topic[dat$List == 3 & dat$Condition == "Heavy" & dat$Task %in% read] <- "Hirsch"
dat$Topic[dat$List == 3 & dat$Condition == "Heavy" & dat$Task == "Free"] <- "Home"
dat$Topic[dat$List == 3 & dat$Condition == "Sitting" & dat$Task %in% read] <- "Schwalbe"
dat$Topic[dat$List == 3 & dat$Condition == "Sitting" & dat$Task == "Free"] <- "Holidays"

dat$Topic[dat$List == 4 & dat$Condition == "Sitting" & dat$Task %in% read] <- "Hirsch"
dat$Topic[dat$List == 4 & dat$Condition == "Sitting" & dat$Task == "Free"] <- "Home"
dat$Topic[dat$List == 4 & dat$Condition == "Heavy" & dat$Task %in% read] <- "Schwalbe"
dat$Topic[dat$List == 4 & dat$Condition == "Heavy" & dat$Task == "Free"] <- "Hobbies"
dat$Topic[dat$List == 4 & dat$Condition == "Light" & dat$Task %in% read] <- "Pferd"
dat$Topic[dat$List == 4 & dat$Condition == "Light" & dat$Task == "Free"] <- "Holidays"

dat$Topic[dat$List == 5 & dat$Condition == "Light" & dat$Task %in% read] <- "Schwalbe"
dat$Topic[dat$List == 5 & dat$Condition == "Light" & dat$Task == "Free"] <- "Hobbies"
dat$Topic[dat$List == 5 & dat$Condition == "Sitting" & dat$Task %in% read] <- "Hirsch"
dat$Topic[dat$List == 5 & dat$Condition == "Sitting" & dat$Task == "Free"] <- "Home"
dat$Topic[dat$List == 5 & dat$Condition == "Heavy" & dat$Task %in% read] <- "Pferd"
dat$Topic[dat$List == 5 & dat$Condition == "Heavy" & dat$Task == "Free"] <- "Holidays"

dat$Topic[dat$List == 6 & dat$Condition == "Heavy" & dat$Task %in% read] <- "Pferd"
dat$Topic[dat$List == 6 & dat$Condition == "Heavy" & dat$Task == "Free"] <- "Home"
dat$Topic[dat$List == 6 & dat$Condition == "Light" & dat$Task %in% read] <- "Hirsch"
dat$Topic[dat$List == 6 & dat$Condition == "Light" & dat$Task == "Free"] <- "Holidays"
dat$Topic[dat$List == 6 & dat$Condition == "Sitting" & dat$Task %in% read] <- "Schwalbe"
dat$Topic[dat$List == 6 & dat$Condition == "Sitting" & dat$Task == "Free"] <- "Hobbies"

dat$Topic[substr(dat$file, 1, 3) == "BRP"] <- "Pferd"
dat$Topic[substr(dat$file, 1, 3) == "BRH"] <- "Hirsch"
dat$Topic[substr(dat$file, 1, 3) == "BRS"] <- "Schwalbe"

dat$Topic[dat$Speaker == "Confederate" & substr(dat$file, 3, 6) == "Hobb"] <- "Hobbies"
dat$Topic[dat$Speaker == "Confederate" & substr(dat$file, 3, 6) == "Schw"] <- "Schwalbe"
dat$Topic[dat$Speaker == "Confederate" & substr(dat$file, 3, 6) == "Pfer"] <- "Pferd"
dat$Topic[dat$Speaker == "Confederate" & substr(dat$file, 3, 6) == "Home"] <- "Home"
dat$Topic[dat$Speaker == "Confederate" & substr(dat$file, 3, 6) == "Hirs"] <- "Hirsch"
dat$Topic[dat$Speaker == "Confederate" & substr(dat$file, 3, 6) == "Holi"] <- "Holidays"


### Calculate BMI

dat$BMI <- dat$Weight/((dat$Height/100)^2) # dividing height by 100 because it has to be in meters

### Calculate gender scores

# inverted items in GEPAQ: F2, M4:
inv <- c("GEPAQ.F2", "GEPAQ.M4")
dat[, inv] <- (max(dat[,inv]) + 1) - dat[, inv]

# participant CBE didn't answer GEPAQ.M5, so it's NA. I'll get the mean of the other GEPAQ.M answers of this participant and use that as GEPAQ.M5
dat$GEPAQ.M5[dat$Participant=="CBE"] <- (dat$GEPAQ.M1[dat$Participant=="CBE"] + dat$GEPAQ.M2[dat$Participant=="CBE"] + dat$GEPAQ.M3[dat$Participant=="CBE"] + dat$GEPAQ.M4[dat$Participant=="CBE"] + dat$GEPAQ.M6[dat$Participant=="CBE"] + dat$GEPAQ.M7[dat$Participant=="CBE"] + dat$GEPAQ.M8[dat$Participant=="CBE"]) / 7

# mean of all answers to each subscale
dat$GEPAQ.F <- (dat$GEPAQ.F1 + dat$GEPAQ.F2 + dat$GEPAQ.F3 + dat$GEPAQ.F4 + dat$GEPAQ.F5 + dat$GEPAQ.F6 + dat$GEPAQ.F7 + dat$GEPAQ.F8) / 8
dat$GEPAQ.M <- (dat$GEPAQ.M1 + dat$GEPAQ.M2 + dat$GEPAQ.M3 + dat$GEPAQ.M4 + dat$GEPAQ.M5 + dat$GEPAQ.M6 + dat$GEPAQ.M7 + dat$GEPAQ.M8) / 8
dat$TMF.F <- (dat$TMF.F1 + dat$TMF.F2 + dat$TMF.F3 + dat$TMF.F4 + dat$TMF.F5 + dat$TMF.F6) / 6
dat$TMF.M <- (dat$TMF.M1 + dat$TMF.M2 + dat$TMF.M3 + dat$TMF.M4 + dat$TMF.M5 + dat$TMF.M6) / 6

dat <- dat[, -c(22:49)] # get rid of the individual items

# ##### Save dataset without calculating differences:
dat$Order <- as.integer(dat$Order)
dat$List <- as.factor(dat$List)
dat$Age <- as.numeric(dat$Age)
dat$Height <- as.numeric(dat$Height)
dat$Gender <- as.factor(dat$Gender)
dat$Topic <- as.factor(dat$Topic)
dat[, c(4:12)] <- lapply(dat[, c(4:12)], as.numeric)
dat$Role[dat$Speaker != "Confederate"] <- "Participant"
dat$Role[dat$Speaker == "Confederate"] <- "Confederate"
dat$Role <- as.factor(dat$Role)

dat$breathRateSpeech <- dat$peaksSpeech/(dat$durationSpeech/60) # peaks per minute (not per second)

save(dat, file=paste0(folder, "DataNoDiff.RData"))

# #####

### Calculate difference between participants' and confederate's speech features (I'm sure there would be a more elegant solution)

# transform the dataset so it contains the confederate information for each row of interest of each participant

# here, it doesn't make sense anymore to have information per IPU (we don't want to subtract the value of each IPU, just of the entire stretch of speech)

dat <- dat[!duplicated(dat$file),]
dat$IPU <- NULL
dat$f0mean <- NULL

conf <- dat[dat$Speaker=="Confederate", c(3:12, 25, 27:30, 32)]
colnames(conf) <- sub("^","C", colnames(conf))
dat2 <-dat[dat$Speaker!="Confederate",]
dat3 <- dat2[rep(seq_len(nrow(dat2)), each = nrow(conf)),]
rownames(dat3) <- 1:nrow(dat3)
conf2 <- do.call("rbind", replicate((nrow(dat3)/nrow(conf)), conf, simplify=FALSE)) # this doesn't take long for datasets that aren't so long, but here it's taking a while. I wonder if there's a simpler solution.

dat4 <- cbind(dat3, conf2)

dat4 <- dat4[(dat4$Condition == dat4$CCondition & dat4$Task == dat4$CTask) | (dat4$Condition == "Baseline"),]
dat4[dat4$Condition=="Baseline", colnames(conf)] <- NA
dat4 <- dat4[!duplicated(dat4),] # no duplicates of the same row

dat4$CTask <- as.factor(dat4$CTask)
dat4$CTopic <- as.factor(dat4$CTopic)

# calculate differences

dat4$f0meanDiff <- dat4$f0IPUmean - dat4$Cf0IPUmean
dat4$speechRateDiff <- dat4$speechRate - dat4$CspeechRate
dat4$articRateDiff <- dat4$articRate - dat4$CarticRate
dat4$pauseDurDiff <- dat4$pauseDur - dat4$CpauseDur
dat4$breathRateSpeechDiff <- dat4$breathRateSpeech - dat4$CbreathRateSpeech

dat4[dat4$Condition=="Baseline", colnames(grepl("Diff", colnames(dat4)))] <- NA # turn the "Diff" values into NA for the baseline, because they don't make sense there

# gender differences

dat4$GenderDiff.TMFF <- dat4$ConfGenderF - dat4$TMF.F # difference between participants' gender identity and their perception of conf's gender expression
dat4$GenderDiff.TMFM <- dat4$ConfGenderM - dat4$TMF.M
dat4$GenderDiff.GEPAQF <- dat4$ConfGenderF - dat4$GEPAQ.F
dat4$GenderDiff.GEPAQM <- dat4$ConfGenderM - dat4$GEPAQ.M

dat4$GenderPercDiff.TMFF <- dat4$ConfGenderF - dat4$CTMF.F # difference between confederate's gender identity and participants' perception of conf's gender expression
dat4$GenderPercDiff.TMFM <- dat4$ConfGenderM - dat4$CTMF.M
dat4$GenderPercDiff.GEPAQF <- dat4$ConfGenderF - dat4$CGEPAQ.F
dat4$GenderPercDiff.GEPAQM <- dat4$ConfGenderM - dat4$CGEPAQ.M

# Still missing from dataset:
# How well the participants synchronized their read speech

# 4
dat <- dat4

save(dat, file=paste0(folder, "DataWithDiff.RData"))


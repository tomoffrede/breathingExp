# Get speech rate data extracted through 3 different methods
# See which method gives speech rate that change according to conditions in a linear way

library(rPraat) # to work with TextGrids
library(tidyverse)
library(sylly.de) # for counting syllables
library(pracma)

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

listTG <- list.files(folder, pattern=".TextGrid")
listTGf <- listTG[substr(listTG, 2, 2)=="F"]

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

com$condition <- NA
com$condition[substr(com$file, 1, 1) == "B"] <- "baseline"
com$condition[substr(com$file, 1, 1) == "S"] <- "sitting"
com$condition[substr(com$file, 1, 1) == "L"] <- "light"
com$condition[substr(com$file, 1, 1) == "H"] <- "heavy"

ggplot(com, aes(x=condition, y=SRsyll))+
  geom_boxplot()

ggplot(com, aes(x=condition, y=speechRate))+
  geom_boxplot()

# now we have the duration of each IPU and each pause in free speech (manually annotated)

##################################################################


####################################### FROM AMPLITUDE ENVELOPE

csv <- list.files(folder, "csv")
csv <- csv[substr(csv, 1, 6) %in% substr(listTGf, 1, 6)]
list <- as.data.frame(cbind(csv, listTGf))
list$worked[substr(list$csv, 1, 6) == substr(list$listTGf, 1, 6)] <- "worked!"
list$worked[substr(list$csv, 1, 6) != substr(list$listTGf, 1, 6)] <- "NO!!!!"
table(list$worked)

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy} # taken from the plyr package: https://github.com/hadley/plyr/blob/34188a04f0e33c4115304cbcf40e5b1c7b85fedf/R/round-any.r#L28-L30
# see https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr/46489816#46489816

i=1
for(i in 1:nrow(list)){
  amp <- read.csv(paste0(folder, list$csv[i]))
  t <- tg.read(paste0(folder, list$listTGf[i]), encoding=detectEncoding(paste0(folder, list$listTGf[i])))
  
  start <- round_any(as.numeric(tg.getIntervalStartTime(t, 2, 2)), 10, ceiling) * 1000 # in ms
  end <- round_any(as.numeric(tg.getIntervalEndTime(t, 2, 2)), 10, floor) * 1000
  dur <- end-start
  
  amp <- amp[amp$time_ms >= start & amp$time_ms <= end,]
  
  p <- findpeaks(amp$env, minpeakdistance = 100)
  plot(amp$env, type="l")
  points(p[,2], p[,1], col="red")
}

com <- merge(sr, SRS, by="file")
com <- com[, c(1, 4, 9)]
com$diff <- com$speechRate - com$SRsyll
hist(com$diff)
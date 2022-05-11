# check f0

library(tidyverse)
library(rPraat)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/CBE/wavforpraat/"

listTXT <- list.files(folder, pattern=".txt")
listTG <- list.files(folder, pattern=".TextGrid")
listTG <- listTG[!grepl("_VUV", listTG)]

listTxg <- as.data.frame(cbind(listTXT, listTG))
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
    if(tg.getIntervalStartTime(tg, 1, n) >= tg.getIntervalStartTime(tg, 2, as.numeric(tg.findLabels(tg, 2, "task"))) && tg.getIntervalEndTime(tg, 1, n) <= tg.getIntervalEndTime(tg, 2, as.numeric(tg.findLabels(tg, 2, "task")))){
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
  # f0perIPU$IPU <- 1:nrow(f0perIPU)
  f0perIPU <- f0perIPU %>%
    mutate(IPU = 1:nrow(f0perIPU)) %>%
    mutate(file = substr(listTxg$txt[i], 1, 6)) %>%
    mutate(f0z = (f0mean - mean(f0mean))/sd(f0mean)) %>%
    filter(abs(f0z) < 2) %>%
    mutate(f0IPUmean = mean(f0mean))
  f <- merge(f0perIPU, IPUtimes %>% select("IPU", "label"), by="IPU")
  ff <- rbind(ff, f)
}

ff <- ff %>% select(file, IPU, f0mean, f0IPUmean, label)

ff$speaker <- substr(ff$file, 4, 6)

ff$condition[substr(ff$file, 1, 1)=="B"] <- "baseline"
ff$condition[substr(ff$file, 1, 1)=="S"] <- "sitting"
ff$condition[substr(ff$file, 1, 1)=="L"] <- "light"
ff$condition[substr(ff$file, 1, 1)=="H"] <- "heavy"

for(i in unique(ff$file)){
  plot(ff$f0mean[ff$file==i])
  readline("Continue")
}

ggplot(ff, aes(condition, f0mean))+
  geom_boxplot()

folderAll <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
list.files(folderAll, "RData")

load(paste0(folderAll, "DataNoDiff.RData"))

dat <- dat %>% filter(file %in% ff$file)

ggplot(dat, aes(Condition, f0mean))+
  geom_boxplot()

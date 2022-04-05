library(rPraat)

folder <- "C:/Users/tomof/Documents/1.Humboldt-Universität_zu_Berlin/ExperimentBreathing/Data/DataForAnalysis/AllData/"
foldersave <- "C:/Users/tomof/Documents/1.Humboldt-Universität_zu_Berlin/ExperimentBreathing/Data/DataForAnalysis/TGsAlignedForBreathCheck/"
foldersave16 <- "C:/Users/tomof/Documents/1.Humboldt-Universität_zu_Berlin/ExperimentBreathing/Data/DataForAnalysis/TGsAlignedForBreathCheck16/"

listTG <- list.files(folder, pattern=".TextGrid")

listTG8 <- vector()
listTG16 <- vector()
count <- 0

for (i in listTG){
  count <- count + 1
  if(detectEncoding(paste0(folder, i)) == "UTF-8"){
    listTG8[count] <- i
  }
  else if(detectEncoding(paste0(folder, i)) == "UTF-16"){
    listTG16[count] <- i
  }
}

listTG8 <- listTG8[!is.na(listTG8)]
listTG16 <- listTG16[!is.na(listTG16)]

listTG8 <- listTG8[listTG8 != "SR-DIN009_AUDIO.TextGrid"]

done <- vector()
notdone <- vector()


for (i in listTG8){
  if(substr(i, 2, 2) == "F"){
    tg <- tg.read(paste0(folder, i), encoding="UTF-8")
    tg2 <- tg.cut0(tg, tStart = tg.getIntervalEndTime(tg, 3, 1), tEnd = tg.getIntervalEndTime(tg, 3, 2))
    tg.write(tg2, paste0(foldersave, i))
    done[length(done)+1] <- i
  }
  if(substr(i, 2, 2) == "R"){
    tg <- tg.read(paste0(folder, i), encoding="UTF-8")
    tg2 <- tg.cut0(tg, tStart = tg.getIntervalEndTime(tg, 2, 1), tEnd = tg.getIntervalEndTime(tg, 2, 2))
    tg.write(tg2, paste0(foldersave, i))
    done[length(done)+1] <- i
  }
  else{
    notdone[length(notdone)+1] <- i
  }
}

wrong <- c("BF-DIN003_AUDIO.TextGrid", "BF-QRC003_AUDIO.TextGrid")

`%!in%` <- Negate(`%in%`)

listTG16 <- listTG16[listTG16 %!in% wrong]

for (i in listTG16){
  if(substr(i, 2, 2) == "F"){
    tg <- tg.read(paste0(folder, i), encoding="UTF-16")
    tg2 <- tg.cut0(tg, tStart = tg.getIntervalEndTime(tg, 3, 1), tEnd = tg.getIntervalEndTime(tg, 3, 2))
    tg.write(tg2, paste0(foldersave16, i))
    done[length(done)+1] <- i
  }
  if(substr(i, 2, 2) == "R"){
    tg <- tg.read(paste0(folder, i), encoding="UTF-16")
    tg2 <- tg.cut0(tg, tStart = tg.getIntervalEndTime(tg, 2, 1), tEnd = tg.getIntervalEndTime(tg, 2, 2))
    tg.write(tg2, paste0(foldersave16, i))
    done[length(done)+1] <- i
  }
  else{
    notdone[length(notdone)+1] <- i
  }
}



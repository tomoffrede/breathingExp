library(tidyverse)

################## create plots with f0 per file per participant (f0 divided per IPU)

folderdata <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folderplot <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/f0plots/perIPU/"

load(paste0(folderdata, "DataNoDiff.RData"))
dat <- dat %>% filter(Task=="Free")

orderIPU <- 1:1000


i="Heavy" # the for loop wasn't working, so do it manually

name=paste0(i, "-f0perIPU")
png(filename=paste0(folderplot, name, ".png"), height=800, width=800)
ggplot(dat %>% filter(Condition==i), aes(IPU, f0mean))+
  geom_point(size=2)+
  facet_wrap(~Speaker)+
  scale_x_discrete(limits = orderIPU)
dev.off()

##################


################## create plots with all f0 means

# folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/" # folder with all needed files
# 
# listTXT <- list.files(folder, pattern=".txt")
# 
# folder2 <- "C:/Users/tomof/Documents/1.Humboldt-Universität_zu_Berlin/ExperimentBreathing/Data/DataForAnalysis/f0plots/f0mean/"
# setwd(folder2)
# 
# for (i in listTXT){
#   dat <- read.table(paste0(folder, i), header=TRUE)
#   i <- substr(i, 1, 6)
#   filename <- paste0(folder2, i, ".png")
#   png(file=filename)
#   print({
#     plot(dat$f0mean, ylim=c(60,500))
#   })
#   dev.off()
# }

############### done with plots


################## create plots with all f0 medians
# 
# folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/" # folder with all needed files
# 
# listTXT <- list.files(folder, pattern=".txt")
# 
# folder2 <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/f0plots/f0median/"
# setwd(folder2)
# 
# for (i in listTXT){
#   dat <- read.table(paste0(folder, i), header=TRUE)
#   i <- substr(i, 1, 6)
#   filename <- paste0(folder2, i, ".png")
#   png(file=filename)
#   print({
#     plot(dat$f0med, ylim=c(60,500))
#   })
#   dev.off()
# }

############### done with plots

######### check for f0 mean outliers

# folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
# 
# listTXT <- list.files(folder, pattern=".txt")
# 
# file <- "LR-YED009_AUDIO.txt"
# 
# dat <- read.table(paste0(folder, file), header=TRUE)
# datmean <- data.frame(matrix(ncol=2, nrow=nrow(dat)))
# colnames(datmean) <- c("f0mean", "z")
# datmean$f0mean <- as.numeric(dat$f0mean)
# datmean$z <- (datmean$f0mean - mean(datmean$f0mean))/sd(datmean$f0mean)
# sort(datmean$z, decreasing=TRUE)
# 
# datmean <- datmean[datmean$z < 3,]
# datmean <- datmean[,-c(2)]


# the clear outliers (from the plot) seem to be:
# 5.33, 4.504 (BF-ATN) next value: 2.44
# 7.21, 4.14 (BR-KRU) next values: 3.44, 2.84
# 5.93, 4.67, 3.58 (BR-OTC) next: 3.47, 3.13, 2.96
# 5.31, 4.98, 4.95, 4.28, 3.57, 2.96, 2.63, 2.29 (HF-MHU) next: 1.88
# 5.29, 4.58 (HF-RZU) nexT: 2.64
# 4.49, 4.35, 4.24, 3.89, 3.79, 3.67, 2.54
# 7.82 (LF-MHU) next: 2.8
# 4.33; 3.54, 2.1, 1.97, 1.87 (LR-YED) next: 1.68

# 3 sounds like a good z threshold

######### end of f0mean outliers


######## check for f0median outliers

# folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
# 
# listTXT <- list.files(folder, pattern=".txt")
# 
# file <- "BR-KRU002_AUDIO.txt"
# 
# dat <- read.table(paste0(folder, file), header=TRUE)
# datmed <- data.frame(matrix(ncol=2, nrow=nrow(dat)))
# colnames(datmed) <- c("f0med", "z")
# datmed$f0med <- as.numeric(dat$f0med)
# datmed$z <- (datmed$f0med - mean(datmed$f0med))/sd(datmed$f0med)
# sort(datmed$z, decreasing=TRUE)

# the clear outliers (from the plot) seem to be:
# 6.57, 4.46 (BF-DIN) next: 3.11, 2.56
# 7, 4.22, 3.59 (BR-KRU) next: 2.82

###### end of f0meadian outliers
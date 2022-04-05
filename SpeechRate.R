library(pracma) # for findpeaks()
library(textgRid) # to deal with TextGrids
library(readtextgrid)

# extract speech units first

# files you need:
# - CSV files containing amplitude envelopes
# - TextGrid files with boundaries for the interval you want to measure

folder <- "C:/Users/tomof/Documents/1.Humboldt-Universität_zu_Berlin/Experiment1/Data/SpeechRateScript/" # folder with all needed files

listTXG <- list.files(folder, pattern=".TextGrid") # list of TextGrid files
listCSV <- list.files(folder, pattern=".csv") # list of CSV files

try <- read_textgrid(C:\Users\tomof\Documents\1.Humboldt-Universität_zu_Berlin\Experiment1\Data\SpeechRateScript\BF-KRU003_AUDIO.TextGrid)

try <- read_textgrid("C:/Users/tomof/Documents/1.Humboldt-Universität_zu_Berlin/Experiment1/Data/SpeechRateScript", file="BF-KRU003_AUDIO.TextGrid")

try <- TextGrid("C:/Users/tomof/Documents/1.Humboldt-Universität_zu_Berlin/Experiment1/Data/SpeechRateScript/BF-KRU003_AUDIO.TextGrid")

for (i in listTXG) {
  file <- paste0(folder, i)
  txg <- TextGrid(file)
}


# read textgrids
# get intervals from textgrids into variable

# read csv
# for each csv dataframe, keep only interval
# findpeaks() in csv dataframe
# nrow(peaks) / (nrow(csvdataframe)/100)
# write this value into another dataframe that'll contain all speech rates

atn <- read.csv("BF-ATN003_AUDIO_ENV.csv")
atn = atn[-c(1:123, 20549:21176),] # the part of the file containing what we're looking for is between 1.246 and 205472 seconds
nrow(atn) # 20425 rows, i.e. 204250 ms (3.4 minutes)

# create a data.frame with the textgrids containing "task" intervals.
# transform all startTime and endTime values into the next round number (427 > 450)
# create a list c() with all the time points in the label. e.g.:
# startTime endTime Label
#   2           6   task
# c(3:5) i.e.: c(dat$startTime[task]:dat$endTime[task])
# then, from the envelope data frame, only keep rows within the interval

peaks = findpeaks(atn$env)
(speechrate = nrow(peaks)/(nrow(atn)/100)) # 3.39 syllables/s







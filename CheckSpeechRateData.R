### Checking if automatically detected speech rate data is good

library(ggplot2)

folder <- "C:/Users/tomof/Documents/1.Humboldt-Universität_zu_Berlin/ExperimentBreathing/Data/DataForAnalysis/SpeechRate/"

################ SPEECH RATE

###### NO PREPROCESSING

sr <- read.csv(paste0(folder, "tableNoPre.csv"))

sr <- sr[!(substr(sr$name, 2, 2)=="R" & substr(sr$name, 16, 16)!="_"),] # remove rows of undifferentiated reading file


sr$Speaker <- substr(sr$name, 4, 6)
sr$Speaker[substr(sr$name, 2, 2)=="-"] <- "Confederate"
sr$Condition[substr(sr$name, 1, 1)=="B"] <- "Baseline"
sr$Condition[substr(sr$name, 1, 1)=="L"] <- "Light"
sr$Condition[substr(sr$name, 1, 1)=="S"] <- "Sitting"
sr$Condition[substr(sr$name, 1, 1)=="H"] <- "Heavy"

colnames(sr) <- c("file", "nsyll", "npause", "dur", "phonationtime", "speechRate", "artRate", "ASD", "Speaker", "Condition")

png(paste0(folder, "SRsAutomaticNoPreProcessing.png"), width=1000, height=900)
ggplot(sr, mapping=aes(Condition, speechRate))+
  geom_point(size=3)+
  facet_wrap(~Speaker)+
  ylim(0.5,5)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=16), title=element_text(size=23), strip.text=element_text(size=18))+
  ggtitle("Speech rate (no preprocessing)")
dev.off()


###### PREPROCESSING: REDUCE NOISE

sr <- read.csv(paste0(folder, "tableRedNoise.csv"))

sr <- sr[!(substr(sr$name, 2, 2)=="R" & substr(sr$name, 16, 16)!="_"),] # remove rows of undifferentiated reading file


sr$Speaker <- substr(sr$name, 4, 6)
sr$Speaker[substr(sr$name, 2, 2)=="-"] <- "Confederate"
sr$Condition[substr(sr$name, 1, 1)=="B"] <- "Baseline"
sr$Condition[substr(sr$name, 1, 1)=="L"] <- "Light"
sr$Condition[substr(sr$name, 1, 1)=="S"] <- "Sitting"
sr$Condition[substr(sr$name, 1, 1)=="H"] <- "Heavy"

colnames(sr) <- c("file", "nsyll", "npause", "dur", "phonationtime", "speechRate", "artRate", "ASD", "Speaker", "Condition")

png(paste0(folder, "SRsAutomaticRedNoise.png"), width=1000, height=900)
ggplot(sr, mapping=aes(Condition, speechRate))+
  geom_point(size=3)+
  facet_wrap(~Speaker)+
  ylim(0.5,5)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=16), title=element_text(size=23), strip.text=element_text(size=18))+
  ggtitle("Speech rate (reduce noise in preprocessing)")
dev.off()


###### PREPROCESSING: REDUCE NOISE

sr <- read.csv(paste0(folder, "tableBandPass.csv"))

sr <- sr[!(substr(sr$name, 2, 2)=="R" & substr(sr$name, 16, 16)!="_"),] # remove rows of undifferentiated reading file


sr$Speaker <- substr(sr$name, 4, 6)
sr$Speaker[substr(sr$name, 2, 2)=="-"] <- "Confederate"
sr$Condition[substr(sr$name, 1, 1)=="B"] <- "Baseline"
sr$Condition[substr(sr$name, 1, 1)=="L"] <- "Light"
sr$Condition[substr(sr$name, 1, 1)=="S"] <- "Sitting"
sr$Condition[substr(sr$name, 1, 1)=="H"] <- "Heavy"

colnames(sr) <- c("file", "nsyll", "npause", "dur", "phonationtime", "speechRate", "artRate", "ASD", "Speaker", "Condition")

png(paste0(folder, "SRsAutomaticBandPass.png"), width=1000, height=900)
ggplot(sr, mapping=aes(Condition, speechRate))+
  geom_point(size=3)+
  facet_wrap(~Speaker)+
  ylim(0.5,5)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=16), title=element_text(size=23), strip.text=element_text(size=18))+
  ggtitle("Speech rate (band pass in preprocessing)")
dev.off()






################ ARTICULATION RATE

###### NO PREPROCESSING

sr <- read.csv(paste0(folder, "tableNoPre.csv"))

sr <- sr[!(substr(sr$name, 2, 2)=="R" & substr(sr$name, 16, 16)!="_"),] # remove rows of undifferentiated reading file


sr$Speaker <- substr(sr$name, 4, 6)
sr$Speaker[substr(sr$name, 2, 2)=="-"] <- "Confederate"
sr$Condition[substr(sr$name, 1, 1)=="B"] <- "Baseline"
sr$Condition[substr(sr$name, 1, 1)=="L"] <- "Light"
sr$Condition[substr(sr$name, 1, 1)=="S"] <- "Sitting"
sr$Condition[substr(sr$name, 1, 1)=="H"] <- "Heavy"

colnames(sr) <- c("file", "nsyll", "npause", "dur", "phonationtime", "speechRate", "artRate", "ASD", "Speaker", "Condition")

png(paste0(folder, "ARsAutomaticNoPreProcessing.png"), width=1000, height=900)
ggplot(sr, mapping=aes(Condition, artRate))+
  geom_point(size=3)+
  facet_wrap(~Speaker)+
  ylim(0,7)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=16), title=element_text(size=23), strip.text=element_text(size=18))+
  ggtitle("Articulation rate (no preprocessing)")
dev.off()


###### PREPROCESSING: REDUCE NOISE

sr <- read.csv(paste0(folder, "tableRedNoise.csv"))

sr <- sr[!(substr(sr$name, 2, 2)=="R" & substr(sr$name, 16, 16)!="_"),] # remove rows of undifferentiated reading file


sr$Speaker <- substr(sr$name, 4, 6)
sr$Speaker[substr(sr$name, 2, 2)=="-"] <- "Confederate"
sr$Condition[substr(sr$name, 1, 1)=="B"] <- "Baseline"
sr$Condition[substr(sr$name, 1, 1)=="L"] <- "Light"
sr$Condition[substr(sr$name, 1, 1)=="S"] <- "Sitting"
sr$Condition[substr(sr$name, 1, 1)=="H"] <- "Heavy"

colnames(sr) <- c("file", "nsyll", "npause", "dur", "phonationtime", "speechRate", "artRate", "ASD", "Speaker", "Condition")

png(paste0(folder, "ARsAutomaticRedNoise.png"), width=1000, height=900)
ggplot(sr, mapping=aes(Condition, artRate))+
  geom_point(size=3)+
  facet_wrap(~Speaker)+
  ylim(0,7)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=16), title=element_text(size=23), strip.text=element_text(size=18))+
  ggtitle("Articulation rate (reduce noise in preprocessing)")
dev.off()


###### PREPROCESSING: REDUCE NOISE

sr <- read.csv(paste0(folder, "tableBandPass.csv"))

sr <- sr[!(substr(sr$name, 2, 2)=="R" & substr(sr$name, 16, 16)!="_"),] # remove rows of undifferentiated reading file


sr$Speaker <- substr(sr$name, 4, 6)
sr$Speaker[substr(sr$name, 2, 2)=="-"] <- "Confederate"
sr$Condition[substr(sr$name, 1, 1)=="B"] <- "Baseline"
sr$Condition[substr(sr$name, 1, 1)=="L"] <- "Light"
sr$Condition[substr(sr$name, 1, 1)=="S"] <- "Sitting"
sr$Condition[substr(sr$name, 1, 1)=="H"] <- "Heavy"

colnames(sr) <- c("file", "nsyll", "npause", "dur", "phonationtime", "speechRate", "artRate", "ASD", "Speaker", "Condition")

png(paste0(folder, "ARsAutomaticBandPass.png"), width=1000, height=900)
ggplot(sr, mapping=aes(Condition, artRate))+
  geom_point(size=3)+
  facet_wrap(~Speaker)+
  ylim(0,7)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=16), title=element_text(size=23), strip.text=element_text(size=18))+
  ggtitle("Articulation rate (band pass in preprocessing)")
dev.off()

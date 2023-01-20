# Tom Offrede
# Make figures for the paper

library(tidyverse)
library(ggsignif)
library(ggdist)
library(tuneR)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/FiguresForPaper/"
folder3 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/PeaksValleys/"

order <- c("Sitting", "Light", "Heavy")
orderBase <- c("Baseline", order)
labels <- c("Sitting"="Sitting", "Light"="Light B.", "Heavy"="Heavy B.")

load(paste0(folder, "DataSpeech.RData"))
load(paste0(folder, "DataBreathing.RData"))
load(paste0(folder, "DataReadSpeech.RData"))

{
# check new speech rate data

# histogram
# range
# sd? exclude?
# plot per participant

# hist(fsm$speechRateIPU)
# range(fsm$speechRateIPU, na.rm=TRUE) # 0.43 - 9.27
# sd(fsm$speechRateIPU, na.rm=TRUE) # 0.92
# folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/SpeechRatePlots/"
# for(s in unique(fsm$Speaker)){
#   png(paste0(folder2, "/free/", s, ".png"))
#   plot(fsm$speechRateIPU[fsm$Speaker==s], main=s)
#   dev.off()
# }
# for(s in unique(frb$Speaker)){
#   png(paste0(folder2, "/read/", s, ".png"))
#   plot(fsm$speechRateIPU[fsm$Speaker==s], main=s)
#   dev.off()
# }

}


# Plots I want to have:
# 
# Figure of breath waves
# 
# CONFEDERATE:
# f0 across conditions, maybe one for read and one for free speech (but one plot)
# speech rate across conditions (free speech)
# breathing rate across conditions (free speech)


b <- readWave(paste0(folder3, "BF-ATN003_SUM_200_breath_100.wav"))
bw <- data.frame(w = b@left[(37.75*b@samp.rate):(46.65*b@samp.rate)],
                 time = seq(0, 8.91, length.out=891)) # 891 is the number of rows of b@left[...]
ggplot(bw, aes(time, w))+
  geom_line(size=1)+
  geom_vline(aes(xintercept=0.04004494), color="purple")+
  geom_vline(aes(xintercept=2.232506), color="purple")+
  geom_vline(aes(xintercept=6.647461), color="purple")+
  geom_vline(aes(xintercept=8.869955), color="purple")+
  geom_point(aes(x=0.59066292, y=-640), color="red", size=4)+
  geom_point(aes(x=2.953315, y=847), color="red", size=4)+
  geom_point(aes(x=6.997854, y=-391), color="red", size=4)+
  geom_segment(x = 2.272506, y = 1200, xend = 6.607461, yend = 1200,
               arrow = arrow(length = unit(0.03, "npc"), ends = "both"))+
  geom_text(aes(x=4.439984, y=1300, label="Breath Cycle"), size=6.5, color="black")+
  geom_segment(x = 2.272506, y = 847, xend = 2.913315, yend = 847,
               arrow = arrow(length = unit(0.02, "npc"), ends = "both"))+
  geom_text(aes(x=2.85, y=1000, label="Inhalation"), size=6.5, color="black")+
  labs(title="Example of Annotated Breathing Waves",
       x="Time (s)",
       y="")+
  theme(legend.position="none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size=20),
        axis.title = element_text(size=16))+
  ylim(-2450, 1400)

ggsave(paste0(folder2, "BreathWave.png"))



df <- fsm %>% 
  filter(Speaker=="Confederate")
dr <- frb %>% 
  filter(Speaker=="Confederate")
df <- df[!duplicated(df),] %>% 
  select(f0raw, Condition)
dr <- dr[!duplicated(dr),] %>% 
  select(f0raw, Condition)

dfr <- rbind(df, dr)
ggplot(dfr, aes(Condition, f0raw))+
  stat_halfeye(adjust = .6,  width = .6, justification = -.2, .width = c(.5, .95))+
  geom_boxplot(width=.1)+
  scale_x_discrete(limits = order, labels=labels)+
  geom_signif(comparisons = list(c("Sitting", "Light"), c("Light", "Heavy")), annotations = c("***", "***"))+
  labs(title = "Confederate's f0 (read and spontaneous speech)", y = "Fundamental frequency")

df <- fsm %>% 
  filter(Speaker=="Confederate")
df <- df[!duplicated(df),] %>% 
  select(articRate, speechRateIPU, Condition)

ggplot(df, aes(Condition, speechRateIPU))+
  stat_halfeye(adjust = .6,  width = .6, justification = -.2, .width = c(.5, .95))+
  geom_boxplot(width=.1)+
  scale_x_discrete(limits = order, labels=labels)+
  geom_signif(comparisons = list(c("Sitting", "Light"), c("Light", "Heavy")), annotations = c("*", "***"))+
  labs(title = "Confederate's speech rate (spontaneous speech)", y = "Speech rate")

dbr <- brm %>% filter(Speaker=="Confederate") %>% 
  mutate(grouping = paste0(Condition, Task, file)) %>% 
  filter(!duplicated(grouping)) %>%
  select(-grouping)

ggplot(dbr %>% filter(Task=="Free"), aes(Condition, breathRate))+
  geom_boxplot(width=.1)+
  geom_point(size=2)+
  scale_x_discrete(limits = order, labels=labels)+
  geom_signif(comparisons = list(c("Sitting", "Light"), c("Light", "Heavy")), annotations = c("*", "*"))+
  labs(title = "Confederate's breathing rate (spontaneous speech)", y = "Breathing rate")

ggplot(dbr %>% filter(grepl("Read", Task)), aes(Condition, breathRate))+
  stat_halfeye(adjust = .6,  width = .6, justification = -.2, .width = c(.5, .95))+
  geom_boxplot(width=.1)+
  geom_point(size=2)+
  scale_x_discrete(limits = order, labels=labels)+
  # geom_signif(comparisons = list(c("Sitting", "Light"), c("Sitting", "Heavy"), c("Light", "Heavy")), annotations = c("*", ".", "**"), y=c(23, 23.4, 23))+
  labs(title = "Confederate's breathing rate (read speech)", y = "Breathing rate")

 
# PARTICIPANTS:
# breathing rate of listening vs watching
# inhalation amplitude across conditions during watching
# inhalation duration across conditions OR in alone vs interaction (read and free speech, maybe one plot with both)
# inhalation duration per number of IPUs
# f0 across conditions during read and free speech (one plot with both or separate?)
# change in f0 per change in breathing rate and maybe per change in speech rate
# 
# 
# 
# 
# 
# 
# 
# 

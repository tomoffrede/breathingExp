# Tom Offrede
# Make figures for the paper

library(tidyverse)
library(ggsignif)
library(ggdist)
library(tuneR)
library(broom)
library(ggpubr)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/FiguresForPaper/"
folder3 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/PeaksValleys/"

order <- c("Sitting", "Light Biking", "Heavy Biking")
orderBase <- c("Baseline", order)
labels <- c("Sitting"="Sitting", "Light"="Light B.", "Heavy"="Heavy B.")

load(paste0(folder, "DataSpeech.RData"))
load(paste0(folder, "DataBreathing.RData"))
load(paste0(folder, "DataReadSpeech.RData"))


# Figure of breath waves

theme_set(theme_bw())

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
  geom_point(aes(x=0.04004494, y=-2428), fill="blue", shape=25, size=3)+
  geom_point(aes(x=2.232506, y=-2037), fill="blue", shape=25, size=3)+
  geom_point(aes(x=6.647461, y=-1420), fill="blue", shape=25, size=3)+
  geom_point(aes(x=8.869955, y=-1216), fill="blue", shape=25, size=3)+
  geom_segment(x = 2.272506, y = 1200, xend = 6.607461, yend = 1200,
               arrow = arrow(length = unit(0.03, "npc"), ends = "both"))+
  geom_text(aes(x=4.439984, y=1300, label="Breath cycle"), size=6.5, color="black")+
  geom_segment(x = 2.29, y = -2000, xend = 2.29, yend = 847,
               arrow = arrow(length = unit(0.04, "npc"), ends = "both"))+
  geom_text(aes(x=2, y=-500, label="Inhalation amplitude"), size=6.5, color="black", angle=90)+
  labs(title="Example of Annotated Breathing Waves",
       x="Time (s)",
       y="")+
  theme(legend.position="none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size=20),
        axis.title = element_text(size=16),
        panel.border = element_blank())+
  ylim(-2450, 1400)

ggsave(paste0(folder2, "BreathWave.png"), width = 2500, height=2000, units="px")

# CONFEDERATE:

## Read speech - Cycle duration
dat <- brm %>% 
  filter(Role=="Confederate", Task=="ReadAlone")

summary(b1 <- lm(cycleDur ~ Condition, dat))

c <- tidy(b1) %>%
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light Biking", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

crd <- ggplot(c, aes(Condition, Estimate))+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.1, color="purple")+
  geom_point(shape=21, color="purple", stroke=1, size=3, fill="white")+
  scale_x_discrete(limits = order)+
  geom_signif(comparisons = list(c("Sitting", "Light Biking"), c("Light Biking", "Heavy Biking")),
              annotations = c("***", "***"))+
  ggtitle("CRD")

##############################

## Read speech - Inhalation amplitude
dat <- brm %>% 
  filter(Role=="Confederate", Task=="ReadAlone")

summary(b1 <- lm(inhalAmp ~ Condition, dat))

c <- tidy(b1) %>%
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light Biking", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

cri <- ggplot(c, aes(Condition, Estimate))+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.1, color="purple")+
  geom_point(shape=21, color="purple", stroke=1, size=3, fill="white")+
  scale_x_discrete(limits = order)+
  geom_signif(comparisons = list(c("Sitting", "Light Biking"), c("Light Biking", "Heavy Biking")),
              annotations = c("***", "***"))+
  ggtitle("CRI")


##############################

## Read speech - F0
dat <- frb %>% 
  filter(Role=="Confederate", Task=="ReadAlone")

summary(b1 <- lm(f0raw ~ Condition, dat))

c <- tidy(b1) %>%
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light Biking", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

crf <- ggplot(c, aes(Condition, Estimate))+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.1, color="purple")+
  geom_point(shape=21, color="purple", stroke=1, size=3, fill="white")+
  scale_x_discrete(limits = order)+
  geom_signif(comparisons = list(c("Sitting", "Light Biking"), c("Light Biking", "Heavy Biking")),
              annotations = c("***", "***"))+
  ggtitle("CRF")

############################################################
############################################################

## Free speech - Cycle duration
dat <- brm %>% 
  filter(Role=="Confederate", Task=="Free")

summary(b1 <- lm(cycleDur ~ Condition, dat))

c <- tidy(b1) %>%
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light Biking", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

cfd <- ggplot(c, aes(Condition, Estimate))+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.1, color="purple")+
  geom_point(shape=21, color="purple", stroke=1, size=3, fill="white")+
  scale_x_discrete(limits = order)+
  geom_signif(comparisons = list(c("Sitting", "Light Biking"), c("Sitting", "Heavy Biking")),
              annotations = c("***", "***"),
              y=c(4.175, 4.275))+
  ggtitle("CFD")

##############################

## Free speech - Inhalation amplitude
dat <- brm %>% 
  filter(Role=="Confederate", Task=="Free")

summary(b1 <- lm(inhalAmp ~ Condition, dat))

c <- tidy(b1) %>%
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light Biking", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

cfi <- ggplot(c, aes(Condition, Estimate))+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.1, color="purple")+
  geom_point(shape=21, color="purple", stroke=1, size=3, fill="white")+
  scale_x_discrete(limits = order)+
  geom_signif(comparisons = list(c("Sitting", "Light Biking"), c("Light Biking", "Heavy Biking")),
              annotations = c("***", "***"))+
  ggtitle("CFI")

##############################

## Free speech - F0
dat <- fsm %>% 
  filter(Role=="Confederate", Task=="Free")

summary(b1 <- lm(f0raw ~ Condition, dat))

c <- tidy(b1) %>%
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light Biking", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

cff <- ggplot(c, aes(Condition, Estimate))+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.1, color="purple")+
  geom_point(shape=21, color="purple", stroke=1, size=3, fill="white")+
  scale_x_discrete(limits = order)+
  geom_signif(comparisons = list(c("Sitting", "Light Biking"), c("Light Biking", "Heavy Biking")),
              annotations = c("***", "***"))+
  ggtitle("CFF")

############################################################
############################################################

# arrange confederate's plots

conf <- ggarrange(crd, cfd, cri, cfi, crf, cff,
               ncol=2, nrow=3)
annotate_figure(conf, top="example")







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

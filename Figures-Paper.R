# Tom Offrede
# Make figures for the paper

library(tidyverse)
library(ggsignif)
library(ggdist)
library(tuneR)
library(broom.mixed)
library(ggpubr)
library(lme4)
library(viridis)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/FiguresForPaper/"
folder3 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/PeaksValleys/"

order <- c("Sitting", "Light Biking", "Heavy Biking")
orderBase <- c("Baseline", order)
labels <- c("Sitting"="Sitting", "Light"="Light B.", "Heavy"="Heavy B.")

theme_set(theme_bw()+
            theme(axis.ticks.y=element_blank(),
                  axis.ticks.x=element_blank(),
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.title = element_text(hjust = 0.5)))

load(paste0(folder, "DataSpeech.RData"))
load(paste0(folder, "DataBreathing.RData"))
load(paste0(folder, "DataReadSpeech.RData"))

##########
# Colors

# scales::show_col(viridis_pal(option="D")(30))

blue <- "#33628DFF"
purple <- "470E61FF"
yellow <- "#E8E419FF"
green <- "#75D054FF"

##########

# Figure of breath waves

## commenting out to save a few seconds when I want to run the entire script

# b <- readWave(paste0(folder3, "BF-ATN003_SUM_200_breath_100.wav"))
# bw <- data.frame(w = b@left[(37.75*b@samp.rate):(46.65*b@samp.rate)],
#                  time = seq(0, 8.91, length.out=891)) # 891 is the number of rows of b@left[...]
# ggplot(bw, aes(time, w))+
#   geom_line(size=1)+
#   geom_vline(aes(xintercept=0.04004494), color=purple)+
#   geom_vline(aes(xintercept=2.232506), color=purple)+
#   geom_vline(aes(xintercept=6.647461), color=purple)+
#   geom_vline(aes(xintercept=8.869955), color=purple)+
#   geom_point(aes(x=0.59066292, y=-640), color=blue, size=4)+
#   geom_point(aes(x=2.953315, y=847), color=blue, size=4)+
#   geom_point(aes(x=6.997854, y=-391), color=blue, size=4)+
#   geom_point(aes(x=0.04004494, y=-2428), fill=green, shape=25, size=3)+
#   geom_point(aes(x=2.232506, y=-2037), fill=green, shape=25, size=3)+
#   geom_point(aes(x=6.647461, y=-1420), fill=green, shape=25, size=3)+
#   geom_point(aes(x=8.869955, y=-1216), fill=green, shape=25, size=3)+
#   geom_segment(x = 2.272506, y = 1200, xend = 6.607461, yend = 1200,
#                arrow = arrow(length = unit(0.03, "npc"), ends = "both"))+
#   geom_text(aes(x=4.439984, y=1300, label="Breath cycle"), size=6.5, color="black")+
#   geom_segment(x = 2.29, y = -2000, xend = 2.29, yend = 847,
#                arrow = arrow(length = unit(0.04, "npc"), ends = "both"))+
#   geom_text(aes(x=2, y=-500, label="Inhalation amplitude"), size=6.5, color="black", angle=90)+
#   labs(title="Example of Annotated Breathing Waves",
#        x="Time (s)",
#        y="")+
#   theme(legend.position="none",
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.ticks.x=element_blank(),
#         plot.title=element_text(size=20),
#         axis.title = element_text(size=16),
#         panel.border = element_blank())+
#   ylim(-2450, 1400)
# 
# ggsave(paste0(folder2, "BreathWave.png"), width = 2500, height=2000, units="px")

#################################################
#################################################
#################################################

# CONFEDERATE:

## Read speech - Cycle duration - Across Conditions
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

(crd <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=green)+
    geom_point(shape=21, color=green, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Light Biking"), c("Light Biking", "Heavy Biking")),
                annotations = c("***", "***"),
                y = 3.3)+
    labs(title="Confederate",
         y = "Breath Cycle Duration",
         x = "")+
    ylim(c(2.63, 5.27)))

##############################

## Read speech - Inhalation amplitude - Across Conditions
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

(cri <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=green)+
    geom_point(shape=21, color=green, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Light Biking"), c("Light Biking", "Heavy Biking")),
                annotations = c("***", "***"),
                y = 0.64)+
    labs(title="",
         y = "Inhalation Amplitude",
         x = "")+
    ylim(c(0.27, 0.67)))


##############################

## Read speech - F0 - Across Conditions
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

(crf <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=green)+
    geom_point(shape=21, color=green, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Light Biking"), c("Light Biking", "Heavy Biking")),
                annotations = c("***", "***"),
                y = 223)+
    labs(title="",
         y = "F0",
         x = "Condition")+
    ylim(c(200, 225)))

############################################################
############################################################

## Free speech - Cycle duration - Across Conditions
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

(cfd <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=green)+
    geom_point(shape=21, color=green, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Light Biking"), c("Sitting", "Heavy Biking")),
                annotations = c("***", "***"),
                y=c(4.45, 4.6))+
    labs(title="Confederate",
         y = "Breath Cycle Duration",
         x = "")+
    ylim(c(2.25, 6.85)))

##############################

## Free speech - Inhalation amplitude - Across Conditions
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

(cfi <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=green)+
    geom_point(shape=21, color=green, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Light Biking"), c("Light Biking", "Heavy Biking")),
                annotations = c("***", "***"),
                y = c(0.53))+
    labs(title="",
         y = "Inhalation Amplitude",
         x = "")+
    ylim(c(0.26, 0.56)))

##############################

## Free speech - F0 - Across Conditions
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

(cff <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=green)+
    geom_point(shape=21, color=green, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Light Biking"), c("Light Biking", "Heavy Biking")),
                annotations = c("***", "***"),
                y = 218)+
    labs(title="",
         y = "F0",
         x = "Condition")+
    ylim(c(189.7, 222)))

############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################

# PARTICIPANTS

## Read speech - Cycle duration - Across Conditions
dat <- brm %>% 
  filter(Role=="Participant", Task=="ReadAlone")

dat$Condition <- relevel(dat$Condition, ref="Light")
summary(b1 <- lmer(cycleDur ~ Condition + (1+Condition| Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Light Biking", ifelse(grepl("Sitting", term), "Sitting", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(prd <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    labs(title="Participants",
         y = "",
         x = "")+
    ylim(c(2.63, 5.27)))

##############################

## Read speech - Inhalation amplitude - Across Conditions
dat <- brm %>% 
  filter(Role=="Participant", Task=="ReadAlone")

dat$Condition <- relevel(dat$Condition, ref="Light")
summary(b1 <- lmer(inhalAmp ~ Condition + (1+Condition| Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Light Biking", ifelse(grepl("Sitting", term), "Sitting", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(pri <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    labs(title="",
         y = "",
         x = "")+
    ylim(c(0.27, 0.65)))


##############################

## Read speech - F0 - Across Conditions
dat <- frb %>% 
  filter(Role=="Participant", Task=="ReadAlone")

summary(b1 <- lmer(f0raw ~ Condition + (1 + Condition | Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light Biking", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(prf <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    labs(title="",
         y = "",
         x = "Condition")+
    ylim(c(200, 223)))

############################################################
############################################################

## Free speech - Cycle duration - Across Conditions
dat <- brm %>% 
  filter(Role=="Participant", Task=="Free", act=="speaking", Condition!="Baseline")

dat$Condition <- relevel(dat$Condition, ref="Light")
summary(b1 <- lmer(cycleDur ~ Condition + (1+Condition| Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Light Biking", ifelse(grepl("Sitting", term), "Sitting", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(pfd <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    labs(title="Participants",
         y = "",
         x = "")+
    ylim(c(2.25, 6.85)))

##############################

## Free speech - Inhalation amplitude - Across Conditions
dat <- brm %>% 
  filter(Role=="Participant", Task=="Free", act=="speaking", Condition!="Baseline")

summary(b1 <- lmer(inhalAmp ~ Condition + (1+Condition| Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light Biking", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(pfi <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    labs(title="",
         y = "",
         x = "")+
    ylim(c(0.26, 0.56)))

##############################

## Free speech - F0 - Across Conditions
dat <- fsm %>% 
  filter(Role=="Participant", Task=="Free", Condition!="Baseline")

summary(b1 <- lmer(f0raw ~ Condition + (1 + Condition | Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light Biking", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(pff <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    labs(title="",
         y = "",
         x = "Condition")+
    ylim(c(189.7, 222)))

############################################################
############################################################

(condR <- ggarrange(crd, prd, cri, pri, crf, prf,
                  ncol=2, nrow=3))
annotate_figure(condR, top="Solo Read Speech")
ggsave(paste0(folder2, "SoloReadConditions.png"), width = 2000, height=2750, units="px")

(condF <- ggarrange(cfd, pfd, cfi, pfi, cff, pff,
                    ncol=2, nrow=3))
annotate_figure(condF, top="Solo Spontaneous Speech")
ggsave(paste0(folder2, "SpontaneousConditions.png"), width = 2000, height=2750, units="px")

############################################################
############################################################
############################################################
############################################################

# Solo Read Speech - Cycle Duration - Baseline vs. Interaction

dat <- brm %>%
  filter(Role=="Participant", Task %in% c("ReadAlone", "ReadBaseline"), Condition %in% c("Baseline", "Sitting"))

summary(b1 <- lmer(cycleDur ~ Condition + (1+Condition| Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Baseline", "Sitting")) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(brd <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    labs(title="Solo Read Speech",
         y = "Breath Cycle Duration",
         x = "")+
    ylim(c(4.75, 6.45)))

##############################

# Solo Read Speech - Inhalation Amplitude - Baseline vs. Interaction

dat <- brm %>%
  filter(Role=="Participant", Task %in% c("ReadAlone", "ReadBaseline"), Condition %in% c("Baseline", "Sitting"))

summary(b1 <- lmer(inhalAmp ~ Condition + (1+Condition| Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Baseline", "Sitting")) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(bri <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    labs(title="",
         y = "Inhalation Amplitude",
         x = "")+
    ylim(c(0.33, 0.445)))

##############################

# Solo Read Speech - F0 - Baseline vs. Interaction

dat <- frb %>%
  filter(Role=="Participant", Task %in% c("ReadAlone", "ReadBaseline"), Condition %in% c("Baseline", "Sitting"))

summary(b1 <- lmer(f0raw ~ Condition + (1+Condition| Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Baseline", "Sitting")) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(brf <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    geom_signif(comparisons = list(c("Baseline", "Sitting")),
                annotations = c("*"),
                y = 210)+
    labs(title="",
         y = "F0",
         x = "Condition")+
    ylim(c(188, 212)))

##############################

# Solo Free Speech - Cycle Duration - Baseline vs. Interaction

dat <- brm %>%
  filter(Role=="Participant", Task == "Free", act == "speaking", Condition %in% c("Baseline", "Sitting"))

summary(b1 <- lmer(cycleDur ~ Condition + (1+Condition| Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Baseline", "Sitting")) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(bfd <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    labs(title="Free Speech",
         y = "",
         x = "")+
    ylim(c(4.75, 6.45)))

##############################

# Solo Free Speech - Inhalation Amplitude - Baseline vs. Interaction

dat <- brm %>%
  filter(Role=="Participant", Task == "Free", act == "speaking", Condition %in% c("Baseline", "Sitting"))

summary(b1 <- lmer(inhalAmp ~ Condition + (1+Condition| Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Baseline", "Sitting")) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(bfi <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    labs(title="",
         y = "",
         x = "")+
    ylim(c(0.33, 0.445)))

##############################

# Solo Read Speech - F0 - Baseline vs. Interaction

dat <- fsm %>%
  filter(Role=="Participant", Task == "Free", Condition %in% c("Baseline", "Sitting"))

summary(b1 <- lmer(f0raw ~ Condition + (1+Condition| Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Baseline", "Sitting")) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(bff <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    labs(title="",
         y = "",
         x = "Condition")+
    ylim(c(188, 212)))

##############################

b <- ggarrange(brd, bfd, bri, bfi, brf, bff,
          ncol=2, nrow=3)
annotate_figure(b, top="Baseline vs. Interaction")
ggsave(paste0(folder2, "BaselineVsInteraction.png"), width = 2000, height=2750, units="px")

############################################################
############################################################
############################################################
############################################################

# Cycle Duration - Listening Breathing - Conditions

dat <- brm %>%
  filter(Role=="Participant", act == "listening")

summary(b1 <- lmer(cycleDur ~ Condition + (1 + Condition |Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light Biking", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(ld <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    labs(title="Breathing while Listening",
         y = "Breath Cycle Duration",
         x = ""))

##############################

# Inhalation Amplitude - Listening Breathing - Conditions

dat <- brm %>%
  filter(Role=="Participant", act == "listening")

summary(b1 <- lmer(inhalAmp ~ Condition + (1 + Condition |Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light Biking", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(li <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    labs(title="",
         y = "Inhalation Amplitude",
         x = "Condition"))

##############################

(l <- ggarrange(ld, li,
               ncol=1, nrow=2))
ggsave(paste0(folder2, "ListeningBreathing.png"), width = 1400, height=1850, units="px")

############################################################
############################################################
############################################################
############################################################

# Solo vs Sync Speech - Cycle Duration

dat <- brm %>%
  filter(Role=="Participant", Task %in% c("ReadJoint", "ReadAlone"))

summary(b1 <- lmer(cycleDur ~ Task + (1+Task | Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Read Solo", "Read Synchronous")) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(rd <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    labs(title="Solo vs. Synchronous Reading",
         y = "Breath Cycle Duration",
         x = ""))

##############################

# Solo vs Sync Speech - Inhalation Amplitude

dat <- brm %>%
  filter(Role=="Participant", Task %in% c("ReadJoint", "ReadAlone"))

summary(b1 <- lmer(inhalAmp ~ Task + (1+Task | Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Read Solo", "Read Synchronous")) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(ri <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    geom_signif(comparisons = list(c("Read Solo", "Read Synchronous")),
                annotations = c("*"))+
    labs(title="",
         y = "Inhalation Amplitude",
         x = ""))

##############################

# Solo vs Sync Speech - F0

dat <- frb %>%
  filter(Role=="Participant", Task %in% c("ReadJoint", "ReadAlone"))

summary(b1 <- lmer(f0raw ~ Task + (1+Task | Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Read Solo", "Read Synchronous")) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(rf <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    labs(title="",
         y = "F0",
         x = "Task"))

##############################

(t <- ggarrange(rd, ri, rf,
               ncol=1, nrow=3))
ggsave(paste0(folder2, "SoloSyncRead.png"), width = 1400, height=2750, units="px")

############################################################
############################################################
############################################################
############################################################

# Sync Speech - Cycle Duration - Conditions

dat <- brm %>%
  filter(Role=="Participant", Task=="ReadJoint")

summary(b1 <- lmer(cycleDur ~ Condition + (1 +Condition| Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light Biking", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(sd <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    labs(title="Synchronous Reading",
         y = "Breath Cycle Duration",
         x = ""))

##############################

# Sync Speech - Inhalation Amplitude - Conditions

dat <- brm %>%
  filter(Role=="Participant", Task=="ReadJoint")

summary(b1 <- lmer(inhalAmp ~ Condition + (1+Condition | Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light Biking", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(si <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Light Biking"), c("Light Biking", "Heavy Biking")),
                annotations = c("*", "*"),
                y = 0.419)+
    ylim(c(0.35, 0.43))+
    labs(title="",
         y = "Inhalation Amplitude",
         x = ""))

##############################

# Sync Speech - F0 - Conditions

dat <- frb %>%
  filter(Role=="Participant", Task=="ReadJoint")

summary(b1 <- lmer(f0raw ~ Condition + (1+Condition | Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light Biking", ifelse(grepl("Heavy", term), "Heavy Biking", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(sf <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=21, color=blue, stroke=1, size=2, fill="white")+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Light Biking", "Heavy Biking"), c("Sitting", "Heavy Biking")),
                annotations = c("*", "***"),
                y = c(211.5, 212.5))+
    ylim(c(203, 213.5))+
    labs(title="",
         y = "F0",
         x = "Condition"))

##############################

(t <- ggarrange(sd, si, sf,
                ncol=1, nrow=3))
ggsave(paste0(folder2, "SyncRead.png"), width = 1400, height=2750, units="px")

############################################################
############################################################
############################################################
############################################################


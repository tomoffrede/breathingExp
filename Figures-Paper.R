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

order <- c("Sitting", "Light B.", "Heavy B.")
order2 <- c("Sitting", "Light", "Heavy")
orderBase <- c("Baseline", order)
labels <- c("Sitting"="Sitting", "Light"="Light B.", "Heavy"="Heavy B.")

theme_set(theme_bw()+
            theme(axis.ticks.y=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title = element_text(size=13),
                  axis.text.x = element_text(color="black", size=12),
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.title = element_text(hjust = 0.5),
                  strip.background = element_blank()))
shape <- 16


load(paste0(folder, "DataSpeech.RData"))
load(paste0(folder, "DataBreathing.RData"))
load(paste0(folder, "DataReadSpeech.RData"))
load(paste0(folder, "DataConfBaselineBreathing.RData"))

# scales::show_col(viridis_pal(option="D")(30))

blue <- "#33628DFF"
purple <- "#470E61FF"
yellow <- "#E8E419FF"
green <- "#4FC46AFF"

##########

# Figure of breath waves

# commenting out to save a few seconds when I want to run the entire script
b <- readWave(paste0(folder3, "BF-ATN003_SUM_200_breath_100.wav"))
bw <- data.frame(w = b@left[(37.75*b@samp.rate):(46.65*b@samp.rate)],
                 time = seq(0, 8.91, length.out=891)) # 891 is the number of rows of b@left[...]
ggplot(bw, aes(time, w))+
  geom_line(size=1)+
  geom_vline(aes(xintercept=0.04004494), color=purple)+
  geom_vline(aes(xintercept=2.232506), color=purple)+
  geom_vline(aes(xintercept=6.647461), color=purple)+
  geom_vline(aes(xintercept=8.869955), color=purple)+
  geom_point(aes(x=0.59066292, y=-640), color=blue, size=4)+
  geom_point(aes(x=2.953315, y=847), color=blue, size=4)+
  geom_point(aes(x=6.997854, y=-391), color=blue, size=4)+
  geom_point(aes(x=0.04004494, y=-2428), fill=green, shape=25, size=3)+
  geom_point(aes(x=2.232506, y=-2037), fill=green, shape=25, size=3)+
  geom_point(aes(x=6.647461, y=-1420), fill=green, shape=25, size=3)+
  geom_point(aes(x=8.869955, y=-1216), fill=green, shape=25, size=3)+
  geom_segment(x = 2.272506, y = 1200, xend = 6.607461, yend = 1200,
               arrow = arrow(length = unit(0.03, "npc"), ends = "both"))+
  geom_text(aes(x=4.439984, y=1300, label="Breath cycle"), size=6.5, color="black")+
  geom_segment(x = 2.29, y = -2000, xend = 2.29, yend = 847,
               arrow = arrow(length = unit(0.04, "npc"), ends = "both"))+
  geom_text(aes(x=2, y=-500, label="Inhalation Amplitude (V)"), size=6.5, color="black", angle=90)+
  labs(title="",
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

ggsave(paste0(folder2, "BreathWave.png"), width = 2500, height=2000, units="px", dpi = "retina")

#################################################
#################################################
#################################################

# CONFEDERATE:

## Read speech - Cycle duration - Across Conditions
dat <- brm %>% 
  filter(Role=="Confederate", Task=="ReadAlone")
dat$Condition <- relevel(dat$Condition, ref="Light")
summary(b1 <- lm(cycleDur ~ Condition, dat))

c <- tidy(b1) %>%
  mutate(term = ifelse(grepl("Intercept", term), "Light B.", ifelse(grepl("Sitting", term), "Sitting", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(crd <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=green)+
    geom_point(shape=shape, color=green, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=green)+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Light B."), c("Light B.", "Heavy B.")),
                annotations = c("**", "**"), textsize=6,
                y = 3.5)+
    labs(title="Confederate (Solo)",
         y = "Breath Cycle Dur. (s)",
         x = "")+
    theme(axis.title = element_text(size=17),
          title = element_text(size=13),
          axis.text.x = element_text(size=14))+
    ylim(c(2.63, 5.27)))

##############################

## Read speech - Inhalation amplitude - Across Conditions
dat <- brm %>% 
  filter(Role=="Confederate", Task=="ReadAlone")

summary(b1 <- lm(inhalAmp ~ Condition, dat))

c <- tidy(b1) %>%
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light B.", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(cri <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=green)+
    geom_point(shape=shape, color=green, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=green)+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Light B."), c("Light B.", "Heavy B.")),
                annotations = c("***", "***"), textsize=6,
                y = 0.64)+
    labs(title="",
         y = "Inhalation Amplitude (V)",
         x = "")+
    theme(axis.title = element_text(size=17),
          title = element_text(size=17),
          axis.text.x = element_text(size=14))+
    ylim(c(0.27, 0.69)))


##############################

## Read speech - F0 - Across Conditions
dat <- frb %>% 
  filter(Role=="Confederate", Task=="ReadAlone")

summary(b1 <- lm(f0raw ~ Condition, dat))

c <- tidy(b1) %>%
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light B.", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(crf <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=green)+
    geom_point(shape=shape, color=green, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=green)+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Light B."), c("Light B.", "Heavy B.")),
                annotations = c("***", "***"), textsize=6,
                y = 223)+
    labs(title="",
         y = "Fund. Frequency (Hz)",
         x = "")+
    theme(axis.title = element_text(size=17),
          title = element_text(size=17),
          axis.text.x = element_text(size=14))+
    ylim(c(200, 227)))

############################################################
############################################################

# ## Free speech - Cycle duration - Across Conditions
# dat <- brm %>% 
#   filter(Role=="Confederate", Task=="Free")
# 
# summary(b1 <- lm(cycleDur ~ Condition, dat))
# 
# c <- tidy(b1) %>%
#   mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light B.", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
#   rename(coefficient = estimate, Condition = term)
# c$Estimate <- c$coefficient + c$coefficient[1]
# c$Estimate[1] <- c$coefficient[1]
# c <- c %>% 
#   mutate(ymin = Estimate - (std.error/2),
#          ymax= Estimate + (std.error/2))
# 
# (cfd <- ggplot(c, aes(Condition, Estimate))+
#     geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=green)+
#     geom_point(shape=shape, color=green, stroke=1, size=2, fill="white")+
#     geom_line(aes(group=1), color=green)+
#     scale_x_discrete(limits = order)+
#     geom_signif(comparisons = list(c("Sitting", "Light B."), c("Sitting", "Heavy B.")),
#                 annotations = c("***", "***"),
#                 y=c(4.5, 4.9))+
#     labs(title="Confederate",
#          y = "Breath Cycle Duration (s)",
#          x = "")+
#     ylim(c(2.25, 6.85)))
# 
# ##############################
# 
# ## Free speech - Inhalation amplitude - Across Conditions
# dat <- brm %>% 
#   filter(Role=="Confederate", Task=="Free")
# 
# summary(b1 <- lm(inhalAmp ~ Condition, dat))
# 
# c <- tidy(b1) %>%
#   mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light B.", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
#   rename(coefficient = estimate, Condition = term)
# c$Estimate <- c$coefficient + c$coefficient[1]
# c$Estimate[1] <- c$coefficient[1]
# c <- c %>% 
#   mutate(ymin = Estimate - (std.error/2),
#          ymax= Estimate + (std.error/2))
# 
# (cfi <- ggplot(c, aes(Condition, Estimate))+
#     geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=green)+
#     geom_point(shape=shape, color=green, stroke=1, size=2, fill="white")+
#     geom_line(aes(group=1), color=green)+
#     scale_x_discrete(limits = order)+
#     geom_signif(comparisons = list(c("Sitting", "Light B."), c("Light B.", "Heavy B.")),
#                 annotations = c("***", "***"),
#                 y = c(0.53))+
#     labs(title="",
#          y = "Inhalation Amplitude (V)",
#          x = "")+
#     ylim(c(0.26, 0.56)))
# 
# ##############################
# 
# ## Free speech - F0 - Across Conditions
# dat <- fsm %>% 
#   filter(Role=="Confederate", Task=="Free")
# 
# summary(b1 <- lm(f0raw ~ Condition, dat))
# 
# c <- tidy(b1) %>%
#   mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light B.", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
#   rename(coefficient = estimate, Condition = term)
# c$Estimate <- c$coefficient + c$coefficient[1]
# c$Estimate[1] <- c$coefficient[1]
# c <- c %>% 
#   mutate(ymin = Estimate - (std.error/2),
#          ymax= Estimate + (std.error/2))
# 
# (cff <- ggplot(c, aes(Condition, Estimate))+
#     geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=green)+
#     geom_point(shape=shape, color=green, stroke=1, size=2, fill="white")+
#     geom_line(aes(group=1), color=green)+
#     scale_x_discrete(limits = order)+
#     geom_signif(comparisons = list(c("Sitting", "Light B."), c("Light B.", "Heavy B.")),
#                 annotations = c("***", "***"),
#                 y = 218)+
#     labs(title="",
#          y = "Fundamental Frequency (Hz)",
#          x = "")+
#     ylim(c(189.7, 222)))

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
  mutate(term = ifelse(grepl("Intercept", term), "Light B.", ifelse(grepl("Sitting", term), "Sitting", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(prd <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=blue)+
    scale_x_discrete(limits = order)+
    labs(title="Participants: Solo",
         y = "",
         x = "")+
    theme(axis.title = element_text(size=17),
          title = element_text(size=13),
          axis.text.x = element_text(size=14))+
    ylim(c(2.63, 5.27)))

##############################

## Read speech - Inhalation amplitude - Across Conditions
dat <- brm %>% 
  filter(Role=="Participant", Task=="ReadAlone")

dat$Condition <- relevel(dat$Condition, ref="Light")
summary(b1 <- lmer(inhalAmp ~ Condition + (1+Condition| Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Light B.", ifelse(grepl("Sitting", term), "Sitting", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(pri <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=blue)+
    scale_x_discrete(limits = order)+
    labs(title="",
         y = "",
         x = "")+
    theme(axis.title = element_text(size=17),
          title = element_text(size=17),
          axis.text.x = element_text(size=14))+
    ylim(c(0.27, 0.69)))


##############################

## Read speech - F0 - Across Conditions
dat <- frb %>% 
  filter(Role=="Participant", Task=="ReadAlone")

summary(b1 <- lmer(f0raw ~ Condition + (1 + Condition | Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light B.", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(prf <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=blue)+
    scale_x_discrete(limits = order)+
    labs(title="",
         y = "",
         x = "")+
    theme(axis.title = element_text(size=17),
          title = element_text(size=17),
          axis.text.x = element_text(size=14))+
    ylim(c(200, 227)))

############################################################
############################################################

# Sync Speech - Cycle Duration - Conditions

dat <- brm %>%
  filter(Role=="Participant", Task=="ReadJoint")

summary(b1 <- lmer(cycleDur ~ Condition + (1 +Condition| Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light B.", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(sd <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=blue)+
    scale_x_discrete(limits = order)+
    labs(title="Participants: Synchronous",
         y = "",
         x = "")+
    theme(axis.title = element_text(size=17),
          title = element_text(size=13),
          axis.text.x = element_text(size=14))+
    ylim(c(2.63, 5.27)))

##############################

# Sync Speech - Inhalation Amplitude - Conditions

dat <- brm %>%
  filter(Role=="Participant", Task=="ReadJoint")

summary(b1 <- lmer(inhalAmp ~ Condition + (1+Condition | Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light B.", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(si <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=blue)+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Light B."), c("Light B.", "Heavy B.")),
                annotations = c("*", "*"), textsize=6,
                y = 0.48)+
    labs(title="",
         y = "",
         x = "")+
    theme(axis.title = element_text(size=17),
          title = element_text(size=17),
          axis.text.x = element_text(size=14))+
    ylim(c(0.27, 0.69)))

##############################

# Sync Speech - F0 - Conditions

dat <- frb %>%
  filter(Role=="Participant", Task=="ReadJoint")

summary(b1 <- lmer(f0raw ~ Condition + (1+Condition | Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light B.", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(sf <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=blue)+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Light B.", "Heavy B."), c("Sitting", "Heavy B.")),
                annotations = c("*", "***"), textsize=6,
                y = c(212.5, 218))+
    labs(title="",
         y = "",
         x = "")+
    theme(axis.title = element_text(size=17),
          title = element_text(size=17),
          axis.text.x = element_text(size=14))+
    ylim(c(200, 227)))


############################################################
############################################################

(condR <- ggarrange(crd, prd, sd, cri, pri, si, crf, prf, sf,
                  ncol=3, nrow=3))
annotate_figure(condR, bottom=text_grob("Confederate Condition", size = 18))
ggsave(paste0(folder2, "ReadConditions.png"), width = 4500, height=4500, units="px", dpi=500)

# (condF <- ggarrange(cfd, pfd, cfi, pfi, cff, pff,
#                     ncol=2, nrow=3))
# annotate_figure(condF, top=text_grob("Solo Spontaneous Speech", face="bold", size = 14))
# ggsave(paste0(folder2, "SpontaneousConditions.png"), width = 2000, height=2750, units="px")

############################################################
############################################################
############################################################
############################################################

# # Solo Read Speech - Cycle Duration - Baseline vs. Interaction
# 
# dat <- brm %>%
#   filter(Role=="Participant", Task %in% c("ReadAlone", "ReadBaseline"), Condition %in% c("Baseline", "Sitting"))
# 
# summary(b1 <- lmer(cycleDur ~ Condition + (1+Condition| Speaker), dat))
# 
# c <- tidy(b1) %>%
#   filter(effect == "fixed") %>% 
#   mutate(term = ifelse(grepl("Intercept", term), "Baseline", "Sitting")) %>% 
#   rename(coefficient = estimate, Condition = term)
# c$Estimate <- c$coefficient + c$coefficient[1]
# c$Estimate[1] <- c$coefficient[1]
# c <- c %>% 
#   mutate(ymin = Estimate - (std.error/2),
#          ymax= Estimate + (std.error/2))
# 
# (brd <- ggplot(c, aes(Condition, Estimate))+
#     geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
#     geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
#     geom_line(aes(group=1), color=blue)+
#     labs(title="Solo Read Speech",
#          y = "Breath Cycle Duration (s)",
#          x = "")+
#     ylim(c(4.75, 6.45)))
# 
# ##############################
# 
# # Solo Read Speech - Inhalation Amplitude - Baseline vs. Interaction
# 
# dat <- brm %>%
#   filter(Role=="Participant", Task %in% c("ReadAlone", "ReadBaseline"), Condition %in% c("Baseline", "Sitting"))
# 
# summary(b1 <- lmer(inhalAmp ~ Condition + (1+Condition| Speaker), dat))
# 
# c <- tidy(b1) %>%
#   filter(effect == "fixed") %>% 
#   mutate(term = ifelse(grepl("Intercept", term), "Baseline", "Sitting")) %>% 
#   rename(coefficient = estimate, Condition = term)
# c$Estimate <- c$coefficient + c$coefficient[1]
# c$Estimate[1] <- c$coefficient[1]
# c <- c %>% 
#   mutate(ymin = Estimate - (std.error/2),
#          ymax= Estimate + (std.error/2))
# 
# (bri <- ggplot(c, aes(Condition, Estimate))+
#     geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
#     geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
#     geom_line(aes(group=1), color=blue)+
#     labs(title="",
#          y = "Inhalation Amplitude (V)",
#          x = "")+
#     ylim(c(0.33, 0.445)))
# 
# ##############################
# 
# # Solo Read Speech - F0 - Baseline vs. Interaction
# 
# dat <- frb %>%
#   filter(Role=="Participant", Task %in% c("ReadAlone", "ReadBaseline"), Condition %in% c("Baseline", "Sitting"))
# 
# summary(b1 <- lmer(f0raw ~ Condition + (1+Condition| Speaker), dat))
# 
# c <- tidy(b1) %>%
#   filter(effect == "fixed") %>% 
#   mutate(term = ifelse(grepl("Intercept", term), "Baseline", "Sitting")) %>% 
#   rename(coefficient = estimate, Condition = term)
# c$Estimate <- c$coefficient + c$coefficient[1]
# c$Estimate[1] <- c$coefficient[1]
# c <- c %>% 
#   mutate(ymin = Estimate - (std.error/2),
#          ymax= Estimate + (std.error/2))
# 
# (brf <- ggplot(c, aes(Condition, Estimate))+
#     geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
#     geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
#     geom_line(aes(group=1), color=blue)+
#     geom_signif(comparisons = list(c("Baseline", "Sitting")),
#                 annotations = c("*"),
#                 y = 210)+
#     labs(title="",
#          y = "Fundamental Frequency (Hz)",
#          x = "Confederate Condition")+
#     ylim(c(188, 212)))
# 
# 
# ##############################
# 
# # Solo Free Speech - Cycle Duration - Baseline vs. Interaction
# 
# dat <- brm %>%
#   filter(Role=="Participant", Task == "Free", act == "speaking", Condition %in% c("Baseline", "Sitting"))
# 
# summary(b1 <- lmer(cycleDur ~ Condition + (1+Condition| Speaker), dat))
# 
# c <- tidy(b1) %>%
#   filter(effect == "fixed") %>% 
#   mutate(term = ifelse(grepl("Intercept", term), "Baseline", "Sitting")) %>% 
#   rename(coefficient = estimate, Condition = term)
# c$Estimate <- c$coefficient + c$coefficient[1]
# c$Estimate[1] <- c$coefficient[1]
# c <- c %>% 
#   mutate(ymin = Estimate - (std.error/2),
#          ymax= Estimate + (std.error/2))
# 
# (bfd <- ggplot(c, aes(Condition, Estimate))+
#     geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
#     geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
#     geom_line(aes(group=1), color=blue)+
#     labs(title="Free Speech",
#          y = "",
#          x = "")+
#     ylim(c(4.75, 6.45)))
# 
# ##############################
# 
# # Solo Free Speech - Inhalation Amplitude - Baseline vs. Interaction
# 
# dat <- brm %>%
#   filter(Role=="Participant", Task == "Free", act == "speaking", Condition %in% c("Baseline", "Sitting"))
# 
# summary(b1 <- lmer(inhalAmp ~ Condition + (1+Condition| Speaker), dat))
# 
# c <- tidy(b1) %>%
#   filter(effect == "fixed") %>% 
#   mutate(term = ifelse(grepl("Intercept", term), "Baseline", "Sitting")) %>% 
#   rename(coefficient = estimate, Condition = term)
# c$Estimate <- c$coefficient + c$coefficient[1]
# c$Estimate[1] <- c$coefficient[1]
# c <- c %>% 
#   mutate(ymin = Estimate - (std.error/2),
#          ymax= Estimate + (std.error/2))
# 
# (bfi <- ggplot(c, aes(Condition, Estimate))+
#     geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
#     geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
#     geom_line(aes(group=1), color=blue)+
#     labs(title="",
#          y = "",
#          x = "")+
#     ylim(c(0.33, 0.445)))
# 
# ##############################
# 
# # Solo Read Speech - F0 - Baseline vs. Interaction
# 
# dat <- fsm %>%
#   filter(Role=="Participant", Task == "Free", Condition %in% c("Baseline", "Sitting"))
# 
# summary(b1 <- lmer(f0raw ~ Condition + (1+Condition| Speaker), dat))
# 
# c <- tidy(b1) %>%
#   filter(effect == "fixed") %>% 
#   mutate(term = ifelse(grepl("Intercept", term), "Baseline", "Sitting")) %>% 
#   rename(coefficient = estimate, Condition = term)
# c$Estimate <- c$coefficient + c$coefficient[1]
# c$Estimate[1] <- c$coefficient[1]
# c <- c %>% 
#   mutate(ymin = Estimate - (std.error/2),
#          ymax= Estimate + (std.error/2))
# 
# (bff <- ggplot(c, aes(Condition, Estimate))+
#     geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
#     geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
#     geom_line(aes(group=1), color=blue)+
#     labs(title="",
#          y = "",
#          x = "Confederate Condition")+
#     ylim(c(188, 212)))
# 
# ##############################
# 
# b <- ggarrange(brd, bfd, bri, bfi, brf, bff,
#           ncol=2, nrow=3)
# annotate_figure(b, top=text_grob("Baseline vs. Interaction", face="bold", size = 14))
# ggsave(paste0(folder2, "BaselineVsInteraction.png"), width = 2000, height=2750, units="px")
# 
# ############################################################
# ############################################################
# ############################################################
# ############################################################
# 
# # Cycle Duration - Listening Breathing - Conditions
# 
# dat <- brm %>%
#   filter(Role=="Participant", act == "listening")
# 
# summary(b1 <- lmer(cycleDur ~ Condition + (1 + Condition |Speaker), dat))
# 
# c <- tidy(b1) %>%
#   filter(effect == "fixed") %>% 
#   mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light B.", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
#   rename(coefficient = estimate, Condition = term)
# c$Estimate <- c$coefficient + c$coefficient[1]
# c$Estimate[1] <- c$coefficient[1]
# c <- c %>% 
#   mutate(ymin = Estimate - (std.error/2),
#          ymax= Estimate + (std.error/2))
# 
# (ld <- ggplot(c, aes(Condition, Estimate))+
#     geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
#     geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
#     geom_line(aes(group=1), color=blue)+
#     scale_x_discrete(limits = order)+
#     labs(title="Breathing while Listening",
#          y = "Breath Cycle Duration (s)",
#          x = ""))
# 
# ##############################
# 
# # Inhalation Amplitude - Listening Breathing - Conditions
# 
# dat <- brm %>%
#   filter(Role=="Participant", act == "listening")
# 
# summary(b1 <- lmer(inhalAmp ~ Condition + (1 + Condition |Speaker), dat))
# 
# c <- tidy(b1) %>%
#   filter(effect == "fixed") %>% 
#   mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light B.", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
#   rename(coefficient = estimate, Condition = term)
# c$Estimate <- c$coefficient + c$coefficient[1]
# c$Estimate[1] <- c$coefficient[1]
# c <- c %>% 
#   mutate(ymin = Estimate - (std.error/2),
#          ymax= Estimate + (std.error/2))
# 
# (li <- ggplot(c, aes(Condition, Estimate))+
#     geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
#     geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
#     geom_line(aes(group=1), color=blue)+
#     scale_x_discrete(limits = order)+
#     labs(title="",
#          y = "Inhalation Amplitude (V)",
#          x = "Confederate Condition"))
# 
# ##############################
# 
# (l <- ggarrange(ld, li,
#                ncol=1, nrow=2))
# ggsave(paste0(folder2, "ListeningBreathing.png"), width = 1400, height=1850, units="px")

############################################################
############################################################
############################################################
############################################################

# Breathing while watching - Cycle duration - Conditions

## Confederate

dat <- bc

dat$condition <- relevel(dat$condition, ref="Light")
summary(b1 <- lm(cycleDur ~ condition, dat))

c <- tidy(b1) %>%
  mutate(term = ifelse(grepl("Intercept", term), "Light B.", ifelse(grepl("Sitting", term), "Sitting", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(cwd <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=green)+
    geom_point(shape=shape, color=green, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=green)+
    geom_signif(comparisons = list(c("Sitting", "Light B."), c("Light B.", "Heavy B.")),
                annotations = c("*", "*"),
                y=3.35)+
    scale_x_discrete(limits = order)+
    labs(title="Confederate",
         y = "Breath Cycle Duration (s)",
         x = "")+
    ylim(c(2.183, 5.139)))


# Breathing while "watching" - Inhalation Amplitude - Conditions

dat <- bc

# dat$condition <- relevel(dat$condition, ref="Light")
summary(b1 <- lm(inhalAmp ~ condition, dat))

c <- tidy(b1) %>%
  mutate(term = ifelse(grepl("Intercept", term), "Heavy B.", ifelse(grepl("Sitting", term), "Sitting", ifelse(grepl("Light", term), "Light B.", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(cwi <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=green)+
    geom_point(shape=shape, color=green, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=green)+
    scale_x_discrete(limits = order)+
    labs(title="",
         y = "Inhalation Amplitude (V)",
         x = "")+
    ylim(c(0.352,0.52)))

## Participants

# Breathing while watching - Cycle Duration - Conditions

dat <- brm %>%
  filter(Role=="Participant", act == "watching")

summary(b1 <- lmer(cycleDur ~ Condition + (1 + Condition |Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light B.", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(wd <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=blue)+
    scale_x_discrete(limits = order)+
    labs(title="Participants",
         y = "",
         x = "")+
    ylim(c(2.183, 5.139)))

##############################

# Breathing while watching - Inhalation Amplitude - Conditions

dat <- brm %>%
  filter(Role=="Participant", act == "watching")

# dat$Condition <- relevel(dat$Condition, ref="Light")
summary(b1 <- lmer(inhalAmp ~ Condition + (1 + Condition |Speaker), dat))

c <- tidy(b1) %>%
  filter(effect == "fixed") %>% 
  mutate(term = ifelse(grepl("Intercept", term), "Sitting", ifelse(grepl("Light", term), "Light B.", ifelse(grepl("Heavy", term), "Heavy B.", term)))) %>% 
  rename(coefficient = estimate, Condition = term)
c$Estimate <- c$coefficient + c$coefficient[1]
c$Estimate[1] <- c$coefficient[1]
c <- c %>% 
  mutate(ymin = Estimate - (std.error/2),
         ymax= Estimate + (std.error/2))

(wi <- ggplot(c, aes(Condition, Estimate))+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=blue)+
    geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=blue)+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Heavy B.")),
                annotations = c("*"),
                y=0.508)+
    labs(title="",
         y = "",
         x = "")+
    ylim(c(0.352,0.52)))

##############################

(w <- ggarrange(cwd, wd, cwi, wi,
                ncol=2, nrow=2))
annotate_figure(w, bottom=text_grob("Confederate Condition", size = 15))
ggsave(paste0(folder2, "WatchingBreathing.png"), width = 2000, height=2000, units="px", dpi="retina")

# width = 2750, height=2750 (the fig with many plots)
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
    geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=blue)+
    labs(title="Solo vs. Synchronous Reading",
         y = "Breath Cycle Duration (s)",
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
    geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=blue)+
    geom_signif(comparisons = list(c("Read Solo", "Read Synchronous")),
                annotations = c("*"),
                y=0.425)+
    labs(title="",
         y = "Inhalation Amplitude (V)",
         x = "")+
    ylim(c(0.374, 0.43)))

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
    geom_point(shape=shape, color=blue, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=blue)+
    labs(title="",
         y = "Fundamental Frequency (Hz)",
         x = "Task"))

##############################

(t <- ggarrange(rd, ri, rf,
               ncol=1, nrow=3))
ggsave(paste0(folder2, "SoloSyncRead.png"), width = 1400, height=2750, units="px")

############################################################
############################################################
############################################################
############################################################


# # Individual differences - Listening breathing - Inhalation amplitude
# 
# dat <- brm %>%
#   filter(Speaker%in% c("PER", "CBE", "JPW", "BND"), act == "listening") %>%
#   mutate(across(Speaker, factor, levels=c("PER", "CBE", "JPW", "BND")),
#          Speaker = ifelse(Speaker=="PER", "A", ifelse(Speaker=="CBE", "B", ifelse(Speaker=="JPW", "C", "D"))))
# 
# annotation <- data.frame(speaker = c("A", "B", "C", "D"),
#                          comp1 = c("Sitting", "Light"),
#                          comp2 = c("Light", "Heavy"),
#                          y = 0.96,
#                          label = "***")
# 
# ggplot(dat, aes(Condition, inhalAmp))+
#   geom_boxplot(color=blue)+
#   facet_wrap(~Speaker)+
#   labs(title="Breathing while Listening",
#        y = "Inhalation Amplitude (V)",
#        x = "Confederate Condition")+
#   geom_signif(data = annotation,
#               aes(xmin = comp1, xmax = comp2,
#                   y_position=y,
#                   annotations=label),
#               manual = TRUE)+
#   scale_x_discrete(limits = order2, labels=c("Sitting", "Light B.", "Heavy B."))+
#   ylim(c(0, 1))
# 
# ggsave(paste0(folder2, "IndivDiff-Listening.png"), width = 2000, height=2000, units="px")

############################################################

# # Individual differences - Baseline vs. Interaction - F0
# 
# dat <- fsm %>%
#   filter(Speaker%in% c("TKJ", "QRC"), Task == "Free", Condition %in% c("Baseline", "Sitting")) %>%
#   mutate(across(Speaker, factor, levels=c("TKJ", "QRC")),
#          Speaker = ifelse(Speaker=="TKJ", "A", "B"))
# 
# # summary(lm(f0raw ~ Condition, dat %>% filter(Speaker=="B")))
# 
# annotation <- data.frame(Speaker = c("A", "B"),
#                           comp1 = c("Baseline", "Sitting"),
#                           comp2 = c("Sitting", "Baseline"),
#                           y = c(235, 300),
#                           label = "***")
# 
# ggplot(dat, aes(Condition, f0raw))+
#   geom_boxplot(color=blue)+
#   facet_wrap(~Speaker)+
#   labs(title="Spontaneous Speech",
#        y = "Fundamental Frequency (Hz)",
#        x = "Confederate Condition")+
#   geom_signif(data = annotation,
#               aes(xmin = comp1, xmax = comp2,
#                   y_position=y,
#                   annotations=label),
#               manual = TRUE)+
#     ylim(c(95, 300))
# 
# ggsave(paste0(folder2, "IndivDiff-BaselineInteraction.png"), width = 2000, height=1500, units="px")

############################################################

# # Individual differences - Conditions - Inhalation Amplitude
# 
# dat <- brm %>%
#   filter(Speaker%in% c("QRC", "RZU", "WJH", "YRI"), act == "speaking", Task=="Free", Condition!="Baseline") %>%
#   mutate(across(Speaker, factor, levels=c("QRC", "RZU", "WJH", "YRI")),
#          Speaker = ifelse(Speaker=="QRC", "A", ifelse(Speaker=="RZU", "B", ifelse(Speaker=="WJH", "C", "D"))))
# 
# annotation <- data.frame(speaker = c("A", "B", "C", "D"),
#                          comp1 = c("Sitting", "Light"),
#                          comp2 = c("Light", "Heavy"),
#                          y = 0.95,
#                          label = "***")
# 
# ggplot(dat %>% filter(inhalAmp<0.9), aes(Condition, inhalAmp))+
#   geom_boxplot(color=blue)+
#   facet_wrap(~Speaker)+
#   labs(title="Spontaneous Speech",
#        y = "Inhalation Amplitude (V)",
#        x = "Confederate Condition")+
#   geom_signif(data = annotation,
#               aes(xmin = comp1, xmax = comp2,
#                   y_position=y,
#                   annotations=label),
#               manual = TRUE)+
#   scale_x_discrete(limits = order2, labels=c("Sitting", "Light B.", "Heavy B."))+
#   ylim(c(0, 1))
# 
# ggsave(paste0(folder2, "IndivDiff-Spontaneous.png"), width = 2000, height=2000, units="px")

############################################################
############################################################
############################################################
############################################################


# Individual differences - Watching - Cycle Duration

dat <- brm %>%
  filter(Speaker%in% c("RZU", "YED"), act == "watching") %>% 
  mutate(across(Speaker, factor, levels=c("RZU", "YED")),
         Speaker = ifelse(Speaker=="RZU", "A", "B"))

dat$Condition <- relevel(dat$Condition, ref="Light")
summary(lm(cycleDur ~ Condition, dat %>% filter(Speaker=="A")))

(a <- ggplot(dat %>% filter(Speaker=="A"), aes(Condition, cycleDur))+
    geom_boxplot(color=blue)+
    labs(title="A",
         y = "Breath Cycle Duration (s)",
         x = "")+
    geom_signif(comparisons = list(c("Sitting", "Light"), c("Sitting", "Heavy")),
                annotations = c("*", "**"),
                y=c(5.2, 5.75))+
    scale_x_discrete(limits = order2, labels=c("Sitting", "Light B.", "Heavy B."))+
    theme(axis.text.x = element_text(size=10))+
    ylim(c(2.8, 8.5)))

(b <- ggplot(dat %>% filter(Speaker=="B"), aes(Condition, cycleDur))+
    geom_boxplot(color=blue)+
    labs(title="B",
         y = "",
         x = "")+
    geom_signif(comparisons = list(c("Sitting", "Light"), c("Light", "Heavy")),
                annotations = c("**", "***"),
                y=7.75)+
    scale_x_discrete(limits = order2, labels=c("Sitting", "Light B.", "Heavy B."))+
    theme(axis.text.x = element_text(size=10))+
    ylim(c(2.8,8.5)))

w <- ggarrange(a,b, ncol=2, nrow=1)
(w <- annotate_figure(w, top=text_grob("Participant Watching (Confederate Silent)", face="bold", size = 12)))

# ggsave(paste0(folder2, "IndivDiff-Watching.png"), width = 2000, height=1200, units="px", dpi="retina")

############################################################

# Individual differences - Conditions - Inhalation Amplitude - Synch read

dat <- brm %>%
  filter(Speaker%in% c("TKJ", "WJH"), Task=="ReadJoint") %>% 
  mutate(across(Speaker, factor, levels=c("TKJ", "WJH")),
         Speaker = ifelse(Speaker=="TKJ", "A", "B"))

dat$Condition <- relevel(dat$Condition, ref="Sitting")
summary(lm(inhalAmp ~ Condition, dat %>% filter(Speaker=="B")))

(a <- ggplot(dat %>% filter(Speaker=="A"), aes(Condition, inhalAmp))+
    geom_boxplot(color=blue)+
    labs(title="C",
         y = "Inhalation Amplitude (V)",
         x = "")+
    geom_signif(comparisons = list(c("Sitting", "Light"), c("Sitting", "Heavy")),
                annotations = c("*", "*"),
                y = c(1, 1.1))+
    scale_x_discrete(limits = order2, labels=c("Sitting", "Light B.", "Heavy B."))+
    ylim(c(0.05,1.2)))

(b <- ggplot(dat %>% filter(Speaker=="B"), aes(Condition, inhalAmp))+
    geom_boxplot(color=blue)+
    labs(title="D",
         y = "",
         x = "")+
    geom_signif(comparisons = list(c("Sitting", "Light")),
                annotations = c("*"),
                y = 0.81)+
    scale_x_discrete(limits = order2, labels=c("Sitting", "Light B.", "Heavy B."))+
    ylim(c(0.05,1.2)))


sy <- ggarrange(a,b, ncol=2, nrow=1)
(sy <- annotate_figure(sy, top=text_grob("Synchronous Read Speech", face="bold", size = 12)))

# ggsave(paste0(folder2, "IndivDiff-SyncInhalAmp.png"), width = 2000, height=2000, units="px", dpi="retina")

############################################################


# Individual differences - Conditions - F0 - Solo

dat <- frb %>%
  filter(Speaker%in% c("YRI", "KRU"), Task=="ReadAlone") %>% 
  mutate(across(Speaker, factor, levels=c("YRI", "KRU")),
         Speaker = ifelse(Speaker=="YRI", "A", "B"))

dat$Condition <- relevel(dat$Condition, ref="Light")
summary(lm(f0raw ~ Condition, dat %>% filter(Speaker=="A")))

(a <- ggplot(dat %>% filter(Speaker=="A"), aes(Condition, f0raw))+
    geom_boxplot(color=blue)+
    labs(title="E",
         y = "Fundamental Frequency (Hz)",
         x = "")+
    geom_signif(comparisons = list(c("Light", "Heavy"), c("Sitting", "Heavy")),
                annotations = c("**", "***"),
                y = c(229, 236))+
    scale_x_discrete(limits = order2, labels=c("Sitting", "Light B.", "Heavy B."))+
    ylim(c(185, 252)))

(b <- ggplot(dat %>% filter(Speaker=="B"), aes(Condition, f0raw))+
    geom_boxplot(color=blue)+
    labs(title="F",
         y = "",
         x = "")+
    geom_signif(comparisons = list(c("Light", "Heavy"), c("Sitting", "Heavy")),
                annotations = c("***", "**"),
                y = c(243.3, 246))+
    scale_x_discrete(limits = order2, labels=c("Sitting", "Light B.", "Heavy B."))+
  ylim(c(185,252)))

so <- ggarrange(a,b, ncol=2, nrow=1)
(so <- annotate_figure(so, top=text_grob("Solo Read Speech", face="bold", size = 12),
                bottom=text_grob("Confederate Condition", size = 12)))

(ind <- ggarrange(w, sy, so, nrow=3, ncol=1))

ggsave(paste0(folder2, "IndiviualEffects.png"), width = 2000, height=2750, units="px", dpi="retina")

############################################################


# check confederate's movement cadence (for review rebuttal)

m <- read.csv("C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/confederateCadence.csv", sep=";", na.strings = "") %>% 
  mutate(duration = duration/60,
         cadence = footCycles/duration) %>% 
  group_by(condition) %>% 
  mutate(meanC = mean(cadence)) %>% 
  ungroup() %>% 
  mutate(file = paste0(substr(condition, 1, 1), "-", substr(text, 1, 4)))

ggplot(m, aes(condition, cadence))+
  geom_boxplot()

summary(lm(cadence ~ condition, m))

## correlation between cadence and cycle duration?

m <- m %>% 
  filter(act == "readSolo")

d <- brm %>% 
  filter(Speaker == "Confederate")

md <- merge(m, d, by="file")

cor.test(md$cadence, md$cycleDur, method="pearson")

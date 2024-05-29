# Tom Offrede
# Figures for defense (not the ones relating to the original project)

library(tidyverse)
library(ggsignif)
library(ggdist)
library(tuneR)
library(broom.mixed)
library(ggpubr)
library(lme4)
library(viridis)
Sys.setenv(LANG="en")

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/FiguresForPaper/"
folder3 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/PeaksValleys/"

order <- c("Sitting", "Light B.", "Heavy B.")
order2 <- c("Sitting", "Light", "Heavy")
orderBase <- c("Baseline", order)
labels <- c("Sitting"="Sitting", "Light"="Light B.", "Heavy"="Heavy B.")

colC <- "#470E61FF"
colP <- "#1F948CFF"

load(paste0(folder, "DataSpeech.RData"))
load(paste0(folder, "DataBreathing.RData"))
load(paste0(folder, "DataReadSpeech.RData"))
load(paste0(folder, "DataConfBaselineBreathing.RData"))

theme_set(theme_minimal()+
            theme(axis.ticks.y=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title = element_text(size=24),
                  axis.text.x = element_text(color="black", size=24),
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.title = element_text(hjust = 0.5, size=24),
                  strip.background = element_blank(),
                  strip.text = element_text(size=24)
            ))
shape <- 16

# F0 - CONFEDERATE VS PARTICIPANTS

{#############################################
# CONFEDERATE F0

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
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=colC)+
    geom_point(shape=shape, color=colC, stroke=1, size=3, fill="white")+
    geom_line(aes(group=1), color=colC)+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Light B."), c("Light B.", "Heavy B.")),
                textsize=7, annotations = c("***", "***"),
                y = 223)+
    labs(title="Confederate",
         y = "Fund. Frequency (Hz)",
         x = NULL)+
    theme(axis.title = element_text(size=17),
          title = element_text(size=17),
          axis.text.x = element_text(size=14))+
    ylim(c(200, 227)))


#############################################
# PARTICIPANTS F0 - READ ALONE

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
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=colP)+
    geom_point(shape=shape, color=colP, stroke=1, size=3, fill="white")+
    geom_line(aes(group=1), color=colP)+
    scale_x_discrete(limits = order)+
    labs(title="Participants: Solo",
         y = NULL,
         x = NULL)+
    theme(axis.title = element_text(size=17),
          title = element_text(size=17),
          axis.text.x = element_text(size=14))+
    ylim(c(200, 227)))


#############################################
# PARTICIPANTS F0 - READ JOINT


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
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=colP)+
    geom_point(shape=shape, color=colP, stroke=1, size=3, fill="white")+
    geom_line(aes(group=1), color=colP)+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Light B.", "Heavy B."), c("Sitting", "Heavy B.")),
                textsize=7, annotations = c("*", "***"),
                y = c(212.5, 218))+
    labs(title="Participants: Sync",
         y = NULL,
         x = NULL)+
    theme(axis.title = element_text(size=17),
          title = element_text(size=17),
          axis.text.x = element_text(size=14))+
    ylim(c(200, 227)))

ggarrange(crf, prf, sf, nrow=1)
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/breath-f0.png", width=7000, height=3000, units="px", dpi=500)}

# CYCLE DURATION - CONFEDERATE VS PARTICIPANTS

{##################
# CONFEDERATE
# CYCLE DURATION - READ

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
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=colC)+
    geom_point(shape=shape, color=colC, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=colC)+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Light B."), c("Light B.", "Heavy B.")),
                textsize=7, annotations = c("**", "**"),
                y = 3.5)+
    labs(title="Confederate",
         y = "Breath Cycle Dur. (s)",
         x = NULL)+
    theme(axis.title = element_text(size=17),
          title = element_text(size=13),
          axis.text.x = element_text(size=14))+
    ylim(c(2.63, 5.27)))


##################
# PARTICIPANTS
# CYCLE DURATION - READ ALONE

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
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=colP)+
    geom_point(shape=shape, color=colP, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=colP)+
    scale_x_discrete(limits = order)+
    labs(title="Participants: Solo",
         y = NULL,
         x = NULL)+
    theme(axis.title = element_text(size=17),
          title = element_text(size=13),
          axis.text.x = element_text(size=14))+
    ylim(c(2.63, 5.27)))


##################
# PARTICIPANTS
# CYCLE DURATION - READ JOINT

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
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=colP)+
    geom_point(shape=shape, color=colP, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=colP)+
    scale_x_discrete(limits = order)+
    labs(title="Participants: Sync",
         y = NULL,
         x = NULL)+
    theme(axis.title = element_text(size=17),
          title = element_text(size=13),
          axis.text.x = element_text(size=14))+
    ylim(c(2.63, 5.27)))


ggarrange(crd, prd, sd, nrow=1)
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/breath-cycleDur.png", width=7000, height=3000, units="px", dpi=500)}


# INHALATION AMPLITUDE - CONFEDERATE VS PARTICIPANTS

{##################
# CONFEDERATE
# INHALATION AMPLITUDE - READ

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
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=colC)+
    geom_point(shape=shape, color=colC, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=colC)+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Light B."), c("Light B.", "Heavy B.")),
                textsize=7, annotations = c("***", "***"),
                y = 0.64)+
    labs(title="Confederate",
         y = "Inhalation Amplitude (V)",
         x = NULL)+
    theme(axis.title = element_text(size=17),
          title = element_text(size=17),
          axis.text.x = element_text(size=14))+
    ylim(c(0.27, 0.69)))


##################
# PARTICIPANTS
# INHALATION AMPLITUDE - READ ALONE

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
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=colP)+
    geom_point(shape=shape, color=colP, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=colP)+
    scale_x_discrete(limits = order)+
    labs(title="Participants: Solo",
         y = NULL,
         x = NULL)+
    theme(axis.title = element_text(size=17),
          title = element_text(size=17),
          axis.text.x = element_text(size=14))+
    ylim(c(0.27, 0.69)))

##################
# PARTICIPANTS
# INHALATION AMPLITUDE - READ JOINT

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
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=colP)+
    geom_point(shape=shape, color=colP, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=colP)+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Light B."), c("Light B.", "Heavy B.")),
                textsize=7, annotations = c("*", "*"), 
                y = 0.48)+
    labs(title="Participants: Sync",
         y = NULL,
         x = NULL)+
    theme(axis.title = element_text(size=17),
          title = element_text(size=17),
          axis.text.x = element_text(size=14))+
    ylim(c(0.27, 0.69)))

ggarrange(cri, pri, si, nrow=1)
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/breath-inhAmp.png", width=7000, height=3000, units="px", dpi=500)}

# QUIET BREATHING CONFEDERATE VS PARTICIPANTS

########################
# CONFEDERATE

## Confederate

#### Cycle Duration

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
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=colC)+
    geom_point(shape=shape, color=colC, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=colC)+
    geom_signif(comparisons = list(c("Sitting", "Light B."), c("Light B.", "Heavy B.")),
                textsize=7, annotations = c("*", "*"),
                y=3.35)+
    scale_x_discrete(limits = order)+
    labs(title="Confederate",
         y = "Breath Cycle Duration (s)",
         x = NULL)+
    ylim(c(2.183, 5.139)))


#### Inhalation Amplitude

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
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=colC)+
    geom_point(shape=shape, color=colC, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=colC)+
    scale_x_discrete(limits = order)+
    labs(title=NULL,
         y = "Inhalation Amplitude (V)",
         x = NULL)+
    ylim(c(0.352,0.52)))




########################
# PARTICIPANTS

#### Cycle Duration

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
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=colP)+
    geom_point(shape=shape, color=colP, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=colP)+
    scale_x_discrete(limits = order)+
    labs(title="Participants",
         y = NULL,
         x = NULL)+
    ylim(c(2.183, 5.139)))


#### Inhalation Amplitude

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
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.3, color=colP)+
    geom_point(shape=shape, color=colP, stroke=1, size=2, fill="white")+
    geom_line(aes(group=1), color=colP)+
    scale_x_discrete(limits = order)+
    geom_signif(comparisons = list(c("Sitting", "Heavy B.")),
                textsize=7, annotations = c("*"),
                y=0.508)+
    labs(title=NULL,
         y = NULL,
         x = NULL)+
    ylim(c(0.352,0.52)))

annotate_figure(ggarrange(cwd, wd, cwi, wi, nrow=2, ncol=2), top=text_grob("Quiet breathing", size=25))
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/breath-quiet.png", width=4500, height=4500, units="px", dpi=500)





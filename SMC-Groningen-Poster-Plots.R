# Tom Offrede
# Plots for poster for SMC - Groningen, Aug 2022

library(tidyverse)
library(viridis)
library(cowplot)
library(ggsignif)
library(ggpubr)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folder2 <- "C:/Users/tomof/Documents/1HU/Courses, conferences, workshops/SMC-Groningen/PosterFigures/"

order <- c("Sitting", "Light", "Heavy")
orderBase <- c("Baseline", order)

load(paste0(folder, "DataSpeech.RData"))
load(paste0(folder, "DataBreathing.RData"))
load(paste0(folder, "DataReadSpeech.RData"))

# to see color codes from the viridis palette:
# scales::show_col(viridis_pal(option="D")(30))
# breathing rate 287C8EFF
# inhalation duration 440154FF? 470E61FF? 404688FF? 443B84FF?
# f0 FDE725FF? E8E419FF? D1E11CFF?
# speech rate 75D054FF? 26AD81FF? 31B57BFF? 3FBC73FF?

# darkest: #440154FF
# brightest: #FDE725FF

# # 
# > brewer.pal(name="Paired", n=12)
# [1] "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00" "#CAB2D6"
# [10] "#6A3D9A" "#FFFF99" "#B15928"
# 
# BREATH: "#A6CEE3" "#1F78B4" (blue)
# SPEECH: "#B2DF8A" "#33A02C" (green) OR "#FB9A99" "#E31A1C" (red)
# 
# ------
#   > brewer.pal(name="RdBu", n=11)
# [1] "#67001F" "#B2182B" "#D6604D" "#F4A582" "#FDDBC7" "#F7F7F7" "#D1E5F0" "#92C5DE" "#4393C3"
# [10] "#2166AC" "#053061"
# 
# BREATH: 2 of "#92C5DE" "#4393C3" "#2166AC" (blue)
# SPEECH: 2 of "#B2182B" "#D6604D" "#F4A582"  (reddish) -- probably last 2
# #

# make plot with confederate's f0, sr

dat <- fsm %>% 
  filter(Speaker=="Confederate", Task=="Free")

change <- 16
ggplot(dat, aes(Condition, f0raw)) +
  theme_bw() +
  geom_boxplot(fill="#E31A1C", color="white") +
  geom_boxplot(dat, mapping=aes(Condition, f0raw), fill=NA, color="#E31A1C", fatten=NULL) + # two `geom_boxplot()` per variable to make the median line white and all the rest of the same color
  geom_boxplot(dat, mapping=aes(Condition, speechRateIPU*change), fill="#FB9A99", color="white") +
  geom_boxplot(dat, mapping=aes(Condition, speechRateIPU*change), fill=NA, color="#FB9A99", fatten=NULL) +
  geom_signif(comparisons=list(c("Sitting", "Light"), c("Light", "Heavy")),
              annotations=c("***", "**"),
              y=c(255, 255),
              textsize = 13) +
  geom_signif(comparisons = list(c("Light", "Heavy"), c("Sitting", "Heavy")),
              annotations = c("*", "*"),
              y=c(20, 15),
              tip_length = -0.03,
              vjust = 2,
              textsize = 13) +
  scale_x_discrete(limits = order, labels=c("Sitting", "Light B.", "Heavy B.")) +
  scale_y_continuous(name="Fundamental Frequency", sec.axis=sec_axis(~ ./change, name=("Speech Rate")),
                     limits=c(17, 280)) +
  # ggtitle("F0 & Speech Rate") +
  labs(title="F0 & Speech Rate", caption = "* |t| > 2; ** |t| > 4; *** |t| > 6") +
  theme(
    axis.title.y = element_text(color = "#E31A1C", size=32),
    axis.text.y = element_text(color = "black", size=20),
    axis.title.y.right = element_text(color = "#FB9A99", size=32),
    axis.text.y.right = element_text(color = "black", size=20),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color="black", size=30),
    title = element_text(size=26),
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(paste0(folder2, "Conf-Speech.png"))


# make plot with confederate's breathing rate and inhalation duration

dat <- brm %>% 
  filter(Speaker=="Confederate", Task=="Free")

ggplot(dat %>% filter(!duplicated(file)), aes(Condition, breathRate)) +
  theme_bw() +
  geom_boxplot(fill="#1F78B4", color="white") +
  geom_boxplot(dat %>% filter(!duplicated(file)), mapping=aes(Condition, breathRate), fill=NA, color="#1F78B4", fatten=NULL) +
  # geom_boxplot(dat, mapping=aes(Condition, inhalDur*change), fill="#A6CEE3", color="white") +
  # geom_boxplot(dat, mapping=aes(Condition, inhalDur*change), fill=NA, color="#A6CEE3", fatten=NULL) +
  geom_signif(comparisons=list(c("Sitting", "Light"), c("Sitting", "Heavy")),
              annotations=c("***", "***"),
              y=c(25, 25.5),
              textsize = 13) +
  scale_x_discrete(limits = order, labels=c("Sitting", "Light B.", "Heavy B.")) +
  # scale_y_continuous(name="Breathing Rate", sec.axis=sec_axis(~ ./change, name=("Inhalation Duration"))) +
  scale_y_continuous(limits=c(14, 26.7)) +
  ggtitle("Breathing Rate") +
  ylab("Breathing Rate") +
  theme(
    axis.title.y = element_text(color = "#1F78B4", size=32),
    axis.text.y = element_text(color = "black", size=20),
    axis.title.y.right = element_text(color = "#A6CEE3", size=32),
    axis.text.y.right = element_text(color = "black", size=20),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color="black", size=30),
    title = element_text(size=26),
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(paste0(folder2, "Conf-Breath.png"))


# # make plot with participants' f0 and breathing rate (READ SPEECH)
# 
# dat <- frb %>% 
#   filter(Role == "Participant", Speaker != "DKG", grepl("Read", Task))
# 
# # dab <- brm %>% 
#   # filter(Role == "Participant", grepl("Read", Task))
# 
# {change <- 11
# ggplot() +
#   theme_bw() +
#   geom_boxplot(dat %>% filter(!duplicated(file)), mapping=aes(Condition, breathRate*change), fill="#1F78B4", color="white") +
#   geom_boxplot(dat %>% filter(!duplicated(file)), mapping=aes(Condition, breathRate*change), fill=NA, color="#1F78B4", fatten=NULL) +
#   geom_boxplot(dat, mapping=aes(Condition, f0raw), fill="#FB9A99", color="white") +
#   geom_boxplot(dat, mapping=aes(Condition, f0raw), fill=NA, color="#FB9A99", fatten=NULL) + # two `geom_boxplot()` per variable to make the median line white and all the rest of the same color
#   scale_x_discrete(limits = orderBase, labels=c("Baseline", "Sitting", "Light B.", "Heavy B.")) +
#   scale_y_continuous(name="Fundamental Frequency", sec.axis=sec_axis(~ ./change, name=("Breathing Rate"))) +
#   ggtitle("F0 & Breathing Rate") +
#   # ylab("Fundamental Frequency") +
#   theme(
#     axis.title.y = element_text(color = "#FB9A99", size=32),
#     axis.text.y = element_text(color = "black", size=20),
#     axis.title.y.right = element_text(color = "#1F78B4", size=32),
#     axis.text.y.right = element_text(color = "black", size=20),
#     axis.title.x = element_blank(),
#     axis.text.x = element_text(color="black", size=30),
#     title = element_text(size=26),
#     plot.background = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_blank(),
#     plot.title = element_text(hjust = 0.5)
#   )
# }
# ggsave(paste0(folder2, "Part-Speech_Breath-Read.png"), height = 1900, width=3000, units="px")


# make plot with participants' f0 (READ SPEECH)

dat <- frb %>%
  filter(Role == "Participant", Speaker != "DKG", grepl("Read", Task))

# dab <- brm %>%
  # filter(Role == "Participant", grepl("Read", Task))

ggplot(dat, aes(Condition, f0raw)) +
  theme_bw() +
  geom_boxplot(fill="#E31A1C", color="white") +
  geom_boxplot(dat, mapping=aes(Condition, f0raw), fill=NA, color="#E31A1C", fatten=NULL) + # two `geom_boxplot()` per variable to make the median line white and all the rest of the same color
  geom_signif(comparisons=list(c("Baseline", "Sitting"), c("Sitting", "Light"), c("Light", "Heavy")),
              annotations=c("***", "***", "***"),
              textsize = 13) +
  scale_x_discrete(limits = orderBase, labels=c("Baseline", "Sitting", "Light B.", "Heavy B.")) +
  scale_y_continuous(limits=c(114, 300)) +
  ggtitle("F0") +
  ylab("Fundamental Frequency") +
  theme(
    axis.title.y = element_text(color = "#E31A1C", size=32),
    axis.text.y = element_text(color = "black", size=20),
    axis.title.y.right = element_text(color = "#1F78B4", size=32),
    axis.text.y.right = element_text(color = "black", size=20),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color="black", size=30),
    title = element_text(size=26),
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(paste0(folder2, "Part-Speech-Read.png"), height = 1900, width=3000, units="px")


# make plot with participants' breathing rate(READ SPEECH)

dab <- brm %>%
  filter(Role == "Participant", grepl("Read", Task))

ggplot(dab %>% filter(!duplicated(file)), aes(Condition, breathRate)) +
  theme_bw() +
  geom_boxplot(fill="#1F78B4", color="white") +
  geom_boxplot(dab %>% filter(!duplicated(file)), mapping=aes(Condition, breathRate), fill=NA, color="#1F78B4", fatten=NULL) +
  geom_signif(comparisons=list(c("Baseline", "Light")),
              annotations=c("* Alone vs Interaction"),
              textsize=10) +
  scale_x_discrete(limits = orderBase, labels=c("Baseline", "Sitting", "Light B.", "Heavy B.")) +
  scale_y_continuous(limits=c(7, 23)) +
  ggtitle("Breathing Rate") +
  ylab("Breathing Rate") +
  theme(
    axis.title.y = element_text(color = "#1F78B4", size=32),
    axis.text.y = element_text(color = "black", size=20),
    axis.title.y.right = element_text(color = "#1F78B4", size=32),
    axis.text.y.right = element_text(color = "black", size=20),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color="black", size=30),
    title = element_text(size=26),
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(paste0(folder2, "Part-Breath-Read.png"), height = 1900, width=3000, units="px")

# make plot with participants' f0 and sr (FREE SPEECH)

dat <- fsm %>% 
  filter(Role == "Participant", Task == "Free")

change <- 20
ggplot(dat, aes(Condition, f0raw)) +
  theme_bw() +
  geom_boxplot(fill="#E31A1C", color="white") +
  geom_boxplot(dat, mapping=aes(Condition, f0raw), fill=NA, color="#E31A1C", fatten=NULL) + # two `geom_boxplot()` per variable to make the median line white and all the rest of the same color
  geom_boxplot(dat, mapping=aes(Condition, speechRateIPU*change), fill="#FB9A99", color="white") +
  geom_boxplot(dat, mapping=aes(Condition, speechRateIPU*change), fill=NA, color="#FB9A99", fatten=NULL) +
  # geom_signif(comparisons=list(c("Baseline", "Heavy")), annotations=c("No effect"), y=285) +
  geom_signif(comparisons = list(c("Baseline", "Light")),
              annotations = c("* Alone vs Interaction"),
              y=c(7),
              tip_length = -0.03,
              vjust = 2,
              textsize=10) +
  scale_x_discrete(limits = orderBase, labels=c("Baseline", "Sitting", "Light B.", "Heavy B.")) +
  scale_y_continuous(name="Fundamental Frequency", sec.axis=sec_axis(~ ./change, name=("Speech Rate")),
                     limits=c(0, 303)) +
  ggtitle("F0 & Speech Rate") +
  theme(
    axis.title.y = element_text(color = "#E31A1C", size=32),
    axis.text.y = element_text(color = "black", size=20),
    axis.title.y.right = element_text(color = "#FB9A99", size=32),
    axis.text.y.right = element_text(color = "black", size=20),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color="black", size=30),
    title = element_text(size=26),
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(paste0(folder2, "Part-Speech-Free.png"), height = 1900, width=3000, units="px")

# make plot with participants' breathing rate and inhalation duration (FREE SPEECH)

dat <- brm %>% 
  filter(Role=="Participant", Task=="Free")

change <- 50
ggplot(dat %>% filter(!duplicated(file)), aes(Condition, breathRate)) +
  theme_bw() +
  # geom_boxplot(dat %>% filter(inhalDur < 2), mapping=aes(Condition, inhalDur*change), fill="#A6CEE3", color="white") +
  # geom_boxplot(dat %>% filter(inhalDur < 2), mapping=aes(Condition, inhalDur*change), fill=NA, color="#A6CEE3", fatten=NULL) +
  geom_boxplot(fill="#1F78B4", color="white") +
  geom_boxplot(dat %>% filter(!duplicated(file)), mapping=aes(Condition, breathRate), fill=NA, color="#1F78B4", fatten=NULL) +
  geom_signif(comparisons=list(c("Baseline", "Light")),
              annotations=c("* Alone vs Interaction"),
              textsize=10) +
  scale_x_discrete(limits = orderBase, labels=c("Baseline ", "Sitting", "Light B.", "Heavy B.")) +
  scale_y_continuous(limits=c(7, 29)) +
  ggtitle("Breathing Rate") +
  ylab("Breathing Rate") +
  theme(
    axis.title.y = element_text(color = "#1F78B4", size=32),
    axis.text.y = element_text(color = "black", size=20),
    axis.title.y.right = element_text(color = "#A6CEE3", size=32),
    axis.text.y.right = element_text(color = "black", size=20),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color="black", size=30),
    title = element_text(size=26),
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(paste0(folder2, "Part-Breath-Free.png"), height = 1900, width=3000, units="px")

# make plot of condition during watching

dat <- brm %>% 
  filter(Role=="Participant", act=="watching")

ggplot(dat, aes(Condition, inhalAmp)) +
  theme_bw() +
  geom_boxplot(fill="#A6CEE3", color="white") +
  geom_boxplot(dat, mapping=aes(Condition, inhalAmp), fill=NA, color="#A6CEE3", fatten=NULL) +
  geom_signif(comparisons=list(c("Sitting", "Heavy")), annotations=c("*"), textsize=13) +
  scale_x_discrete(limits = order, labels=c("Sitting", "Light B.", "Heavy B.")) +
  scale_y_continuous(limits=c(0, 1.1)) +
  ggtitle("Inhalation Amplitude") +
  ylab("Inhalation Amplitude") +
  theme(
    axis.title.y = element_text(color = "#A6CEE3", size=32),
    axis.text.y = element_text(color = "black", size=20),
    # axis.title.y.right = element_text(color = "#A6CEE3", size=32),
    # axis.text.y.right = element_text(color = "black", size=20),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color="black", size=30),
    title = element_text(size=26),
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(paste0(folder2, "Part-Breath-Watching.png"))
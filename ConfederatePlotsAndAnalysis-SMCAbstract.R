library(tidyverse)
library(viridis)
library(cowplot)
library(ggsignif)
library(tuneR)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folder2 <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/SMCAbstract/"
folderbr <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Confederate/Breathing/"

order <- c("Sitting", "Light", "Heavy")


### final figure for SMC abstract
### with f0 and breathing rate through conditions

load(paste0(folder, "DataSpeech.RData"))

dat <- fsm %>%
  filter(Speaker=="Confederate") %>%
  # filter(Condition!="Baseline") %>%
  filter(Task=="Free")

# dat$Condition[dat$Condition=="Light"] <- "Light Biking"
# dat$Condition[dat$Condition=="Heavy"] <- "Heavy Biking"

png(paste0(folder2, "figure.png"), width=600, height=550)
ggplot() +
  geom_boxplot(dat, mapping=aes(Condition, f0IPUmean), fill="#1E9B8AFF") +
  geom_point(dat, mapping=aes(Condition, breathRate*12), color="#482677FF", size=4) +
  scale_x_discrete(limits = order, labels=c("Sitting", "Light Biking", "Heavy Biking")) +
  scale_y_continuous(name="Fundamental Frequency (f0)", sec.axis=sec_axis(~ ./12, name=("Breathing Rate"))) +
  ggtitle("Confederate's f0 (boxplots) and breathing rate (points)") +
  theme(
    axis.title.y = element_text(color = "#1E9B8AFF", size=18),
    axis.text.y = element_text(color = "#1E9B8AFF", size=14),
    axis.title.y.right = element_text(color = "#482677FF", size=18),
    axis.text.y.right = element_text(color = "#482677FF", size=14),
    axis.title.x = element_text(size=18),
    axis.text.x = element_text(color="black", size=16),
    title = element_text(size=18)
  )
dev.off()

range(dat$breathRate) # 15 - 30
range(dat$f0IPUmean) # 200 - 280


###### end of figure

## Free speech

load(paste0(folder, "DataNoDiff.RData"))

dat <- dat %>%
  filter(Speaker=="Confederate") %>%
  filter(Condition!="Baseline") %>%
  filter(Task=="Free")

### breathing rate

ggplot(dat %>% filter(Role=="Confederate"), aes(Condition, breathRateSpeech))+
  scale_x_discrete(limits = order)+
  geom_point(size=4)+
  facet_wrap(~Topic)

### f0 mean
dat$Condition <- relevel(dat$Condition, ref="Sitting")
summary(fm <- lm(f0mean ~ Condition, data=dat))

png(paste0(folder2, "ConditionsFreef0BOX.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = f0mean)) +
  geom_boxplot(fill="#2D708EFF")+
  geom_signif(comparisons=list(c("Sitting", "Light"), c("Light", "Heavy"), c("Sitting", "Heavy")), annotations=c("**", "***", "***"),
              map_signif_level=TRUE, y=c(237, 237, 240))+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  labs(title="Free Speech: f0 mean", y="f0 mean", caption="0: *** 0.001 **")
dev.off()

png(paste0(folder2, "ConditionsFreef0POINTS.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = f0mean)) +
  geom_point(size=4, color="#482173FF")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
    plot.title=element_text(size=25))+
  labs(title="Free Speech: f0 mean", y="f0 mean")
dev.off()


### Speech rate
dat$Condition <- relevel(dat$Condition, ref="Heavy")
summary(fm <- lm(speechRate ~ Condition, data=dat))

png(paste0(folder2, "ConditionsFreeSRBOX.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = speechRate)) +
  geom_boxplot(fill="#2D708EFF")+
  geom_signif(comparisons=list(c("Sitting", "Light"), c("Sitting", "Heavy")), annotations="**",
              map_signif_level=TRUE, y=c(3.52, 3.59))+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  labs(title="Free Speech: Speech Rate", y="Speech Rate (syllables/second)", caption="0.001: **")
dev.off()

png(paste0(folder2, "ConditionsFreeSRPerTopicPOINTS.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = speechRate)) +
  geom_point(size=4, color="#482173FF")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  facet_wrap(~Topic)+
  labs(title="Free Speech: Speech Rate", y="Speech Rate (syllables/second)")
dev.off()



### Articulation rate
dat$Condition <- relevel(dat$Condition, ref="Heavy")
summary(fm <- lm(articRate ~ Condition, data=dat))

png(paste0(folder2, "ConditionsFreeARBOX.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = articRate)) +
  geom_boxplot(fill="#2D708EFF")+
  # geom_signif(comparisons=list(c("Sitting", "Heavy")), annotations="***",
  #             map_signif_level=TRUE)+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  labs(title="Free Speech: Articulation Rate", y="Articulation Rate (syllables/second)")
dev.off()

png(paste0(folder2, "ConditionsFreeARPerTopicPOINTS.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = articRate)) +
  geom_point(size=4, color="#482173FF")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  facet_wrap(~Topic)+
  labs(title="Free Speech: Articulation Rate", y="Articulation Rate (syllables/second)")
dev.off()


### Pause duration
dat$Condition <- relevel(dat$Condition, ref="Sitting")
summary(fm <- lm(pauseDur ~ Condition, data=dat))

png(paste0(folder2, "ConditionsFreePauseBOX.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = pauseDur)) +
  geom_boxplot(fill="#2D708EFF")+
  # geom_signif(comparisons=list(c("Sitting", "Light"), c("Light", "Heavy")), annotations=".",
  #              map_signif_level=TRUE)+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  labs(title="Free Speech: Pause Duration", y="Pause Duration (seconds)", caption="0.05: .")
dev.off()

png(paste0(folder2, "ConditionsFreePausePerTopicPOINTS.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = pauseDur)) +
  geom_point(size=4, color="#482173FF")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  facet_wrap(~Topic)+
  labs(title="Free Speech: Pause Duration", y="Pause Duration (seconds)")
dev.off()


### Breathing rate
dat$Condition <- relevel(dat$Condition, ref="Sitting")
summary(fm <- lm(breathRateSpeech ~ Condition, data=dat))

png(paste0(folder2, "ConditionsFreeBRBOX.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = breathRateSpeech)) +
  geom_boxplot(fill="#2D708EFF")+
  geom_signif(comparisons=list(c("Sitting", "Light"), c("Light", "Heavy"), c("Sitting", "Heavy")), annotations=(c("**", "*", "***")),
              map_signif_level=TRUE, y=c(27.3, 27.3, 28))+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  labs(title="Free Speech: Breathing Rate", y="Breathing Rate (cycles/minute)", caption="0: *** 0.001: ** 0.01: *")
dev.off()

png(paste0(folder2, "ConditionsFreeBRPerTopicPOINTS.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = breathRateSpeech)) +
  geom_point(size=4, color="#482173FF")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  facet_wrap(~Topic)+
  labs(title="Free Speech: Breathing Rate", y="Breathing Rate (cycles/minute)")
dev.off()



###############################################################################################################################




### Read speech

load(paste0(folder, "DataNoDiff.RData"))

dat <- dat %>%
  filter(Speaker=="Confederate") %>%
  filter(Condition!="Baseline") %>%
  filter(Task=="ReadAlone")

### breathing rate

# BREATHING RATE IS LOWER IN LIGHT BIKING THAN SITTING. BUT THAT'S NOT WHAT IT LOOKS LIKE IF WE LOOK AT THE BREATHING PLOTS.

ggplot(dat %>% filter(Role=="Confederate"), aes(Condition, breathRateSpeech))+
  scale_x_discrete(limits = orderconf)+
  geom_point(size=4)+
  facet_wrap(~Topic)

### f0 mean
dat$Condition <- relevel(dat$Condition, ref="Heavy")
summary(fm <- lm(f0mean ~ Condition, data=dat))

png(paste0(folder2, "ConditionsReadf0BOX.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = f0mean)) +
  geom_boxplot(fill="#2D708EFF")+
  geom_signif(comparisons=list(c("Sitting", "Light"), c("Sitting", "Heavy")),
              annotations=c("*", "**"),
              map_signif_level=TRUE, y=c(228, 230))+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  labs(title="Read Speech: f0 mean", y="f0 mean", caption="0.001: ** 0.01: *")
dev.off()

png(paste0(folder2, "ConditionsReadf0POINTS.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = f0mean)) +
  geom_point(size=4, color="#482173FF")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  labs(title="Read Speech: f0 mean", y="f0 mean")
dev.off()


### Speech rate
dat$Condition <- relevel(dat$Condition, ref="Sitting")
summary(fm <- lm(speechRate ~ Condition, data=dat))

png(paste0(folder2, "ConditionsReadSRBOX.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = speechRate)) +
  geom_boxplot(fill="#2D708EFF")+
  geom_signif(comparisons=list(c("Sitting", "Light"), c("Sitting", "Heavy"), c("Light", "Heavy")),
              annotations=c("*", "***", "*"),
              map_signif_level=TRUE, y=c(3.75, 3.81, 3.75))+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  labs(title="Read Speech: Speech Rate", y="Speech Rate (syllables/second)", caption="0: *** 0.001: **")
dev.off()

png(paste0(folder2, "ConditionsReadSRPerTopicPOINTS.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = speechRate)) +
  geom_point(size=4, color="#482173FF")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  facet_wrap(~Topic)+
  labs(title="Read Speech: Speech Rate", y="Speech Rate (syllables/second)")
dev.off()



### Articulation rate
dat$Condition <- relevel(dat$Condition, ref="Sitting")
summary(fm <- lm(articRate ~ Condition, data=dat))

png(paste0(folder2, "ConditionsReadARBOX.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = articRate)) +
  geom_boxplot(fill="#2D708EFF")+
  geom_signif(comparisons=list(c("Sitting", "Heavy")),
              annotations=c("*"),
              map_signif_level=TRUE)+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  labs(title="Read Speech: Articulation Rate", y="Articulation Rate (syllables/second)", caption="< 0.05: *")
dev.off()

png(paste0(folder2, "ConditionsReadARPerTopicPOINTS.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = articRate)) +
  geom_point(size=4, color="#482173FF")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  facet_wrap(~Topic)+
  labs(title="Read Speech: Articulation Rate", y="Articulation Rate (syllables/second)")
dev.off()


### Pause duration
dat$Condition <- relevel(dat$Condition, ref="Sitting")
summary(fm <- lm(pauseDur ~ Condition, data=dat))

png(paste0(folder2, "ConditionsReadPauseBOX.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = pauseDur)) +
  geom_boxplot(fill="#2D708EFF")+
  #geom_signif(comparisons=list(c("Sitting", "Light"), c("Light", "Heavy"), c("Sitting", "Heavy")),
   #           annotations=c("*", "**", "***"), y=c(30, 30, 31),
    #          map_signif_level=TRUE)+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  labs(title="Read Speech: Pause Duration", y="Pause Duration (seconds)", caption="0: *** 0.001: ** 0.01: *")
dev.off()

png(paste0(folder2, "ConditionsReadPausePerTopicPOINTS.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = pauseDur)) +
  geom_point(size=4, color="#482173FF")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  facet_wrap(~Topic)+
  labs(title="Read Speech: Pause Duration", y="Pause Duration (seconds)")
dev.off()


### Breathing rate
dat$Condition <- relevel(dat$Condition, ref="Sitting")
summary(fm <- lm(breathRateSpeech ~ Condition, data=dat))

png(paste0(folder2, "ConditionsReadBRBOX.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = breathRateSpeech)) +
  geom_boxplot(fill="#2D708EFF")+
  geom_signif(comparisons=list(c("Sitting", "Light"), c("Light", "Heavy"), c("Sitting", "Heavy")), annotations=(c("**", "*", "***")),
              map_signif_level=TRUE, y=c(27.3, 27.3, 28))+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  labs(title="Read Speech: Breathing Rate", y="Breathing Rate (cycles/minute)", caption="0: *** 0.001: ** 0.01: *")
dev.off()

png(paste0(folder2, "ConditionsReadBRPerTopicPOINTS.png"), width=700, height=500)
ggplot(dat, aes(x = Condition, y = breathRateSpeech)) +
  geom_point(size=4, color="#482173FF")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))+
  facet_wrap(~Topic)+
  labs(title="Read Speech: Breathing Rate", y="Breathing Rate (cycles/minute)")
dev.off()


#### breathing plots

listWAV <- list.files(folderbr, pattern="\\.wav")


##### Read speech

i="H-Hirsch_THORAX_joint_200.wav"

brf <- readWave(paste0(folderbr, i))
br <- as.data.frame(brf@left)
colnames(br) <- "bry"
br$x <- as.numeric(row.names(br))
br$bry <- (br$bry - min(br$bry))/(max(br$bry) - min(br$bry))
br <- br %>% filter(x<10001)

png("H-Hirsch.png", width=1000, height=500)
ggplot(br, aes(x, bry))+
  geom_line()+
  theme_bw()+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=16), title=element_text(size=21))+
  ggtitle("Heavy Biking (Read Speech)")+
  labs(y="Breath", x="Time")+
  ylim(c(0,1))
dev.off()

i="L-Hirsch_THORAX_joint_200.wav"

brf <- readWave(paste0(folderbr, i))
br <- as.data.frame(brf@left)
colnames(br) <- "bry"
br$x <- as.numeric(row.names(br))
br$bry <- (br$bry - min(br$bry))/(max(br$bry) - min(br$bry))
br <- br %>% filter(x<10001)

png("L-Hirsch.png", width=1000, height=500)
ggplot(br, aes(x, bry))+
  geom_line()+
  theme_bw()+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=16), title=element_text(size=21))+
  ggtitle("Light Biking (Read Speech)")+
  labs(y="Breath", x="")+
  ylim(c(0,1))
dev.off()

i="S-Hirsch_THORAX_joint_200.wav"

brf <- readWave(paste0(folderbr, i))
br <- as.data.frame(brf@left)
colnames(br) <- "bry"
br$x <- as.numeric(row.names(br))
br$bry <- (br$bry - min(br$bry))/(max(br$bry) - min(br$bry))
br <- br %>% filter(x<10001)

png("S-Hirsch.png", width=1000, height=500)
ggplot(br, aes(x, bry))+
  geom_line()+
  theme_bw()+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=16), title=element_text(size=21))+
  ggtitle("Sitting (Read Speech)")+
  labs(y="Breath", x="")+
  ylim(c(0,1))
dev.off()


##### Free speech

i="H-Hobbies_THORAX_200.wav"

brf <- readWave(paste0(folderbr, i))
br <- as.data.frame(brf@left)
colnames(br) <- "bry"
br$x <- as.numeric(row.names(br))
br$bry <- (br$bry - min(br$bry))/(max(br$bry) - min(br$bry))
br <- br %>% filter(x<10001)

png("H-Hobbies.png", width=1000, height=500)
ggplot(br, aes(x, bry))+
  geom_line()+
  theme_bw()+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=16), title=element_text(size=21))+
  ggtitle("Heavy Biking (Free Speech)")+
  labs(y="Breath", x="Time")+
  ylim(c(0,1))
dev.off()

i="L-Hobbies_THORAX_200.wav"

brf <- readWave(paste0(folderbr, i))
br <- as.data.frame(brf@left)
colnames(br) <- "bry"
br$x <- as.numeric(row.names(br))
br$bry <- (br$bry - min(br$bry))/(max(br$bry) - min(br$bry))
br <- br %>% filter(x<10001)

png("L-Hobbies.png", width=1000, height=500)
ggplot(br, aes(x, bry))+
  geom_line()+
  theme_bw()+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=16), title=element_text(size=21))+
  ggtitle("Light Biking (Free Speech)")+
  labs(y="Breath", x="")+
  ylim(c(0,1))
dev.off()

i="S-Hobbies_THORAX_200.wav"

brf <- readWave(paste0(folderbr, i))
br <- as.data.frame(brf@left)
colnames(br) <- "bry"
br$x <- as.numeric(row.names(br))
br$bry <- (br$bry - min(br$bry))/(max(br$bry) - min(br$bry))
br <- br %>% filter(x<10001)

png("S-Hobbies.png", width=1000, height=500)
ggplot(br, aes(x, bry))+
  geom_line()+
  theme_bw()+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=16), title=element_text(size=21))+
  ggtitle("Sitting (Free Speech)")+
  labs(y="Breath", x="")+
  ylim(c(0,1))
dev.off()



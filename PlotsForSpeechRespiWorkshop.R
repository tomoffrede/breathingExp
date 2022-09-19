library(tidyverse)
library(viridis)
library(ggsignif)
library(lme4)

# scales::show_col(viridis_pal(option="D")(12))

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folderPlot <- "C:/Users/tomof/Documents/1HU/Presentations/Speech respiration workshop - 17.6.22/"

order <- c("Sitting", "Light", "Heavy")
orderWithBase <- c("Baseline", "Sitting", "Light", "Heavy")

# Confederate - f0

load(paste0(folder, "DataSpeech.RData"))

df <- fsm %>%
  filter(Task == "Free", Speaker == "Confederate") %>%
  select(c("file", "Speaker", "IPU", "f0mean", "f0IPUmean", "speechRateIPU", "durSpeech", "durPauses", "speechRate", "articRate", "Condition", "breathCycleDurMean", "breathRate", "breathCycleDur")) %>%
  mutate(f0IPUz = (f0mean - mean(f0mean, na.rm=TRUE)) / sd(f0mean, na.rm=TRUE),
         f0filez = (f0IPUmean - mean(f0IPUmean, na.rm=TRUE)) / sd(f0IPUmean, na.rm=TRUE))

png(paste0(folderPlot, "conf-f0.png"), height=550, width=550)
ggplot(df, aes(Condition, f0mean))+
  geom_boxplot(fill="#39558CFF")+
  scale_x_discrete(limits = order)+
  geom_signif(comparisons=list(c("Sitting", "Light"), c("Light", "Heavy")), annotations=c("**", "***"))+
  labs(title="Confederate: f0", y="f0", caption="**: |t| > 3; ***: |t| > 4")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))
dev.off()

df$Condition <- relevel(df$Condition, ref="Light")

summary(lm(f0IPUz ~ Condition, df))



# Confederate - speech rate

load(paste0(folder, "DataSpeech.RData"))

df <- fsm %>%
  filter(Task == "Free", Speaker == "Confederate") %>%
  select(c("file", "Speaker", "IPU", "f0mean", "f0IPUmean", "speechRateIPU", "durSpeech", "durPauses", "speechRate", "articRate", "Condition", "breathCycleDurMean", "breathRate", "breathCycleDur"))

png(paste0(folderPlot, "conf-speechRate.png"), height=550, width=550)
ggplot(df, aes(Condition, speechRateIPU))+
  geom_boxplot(fill="#39558CFF")+
  scale_x_discrete(limits = order)+
  geom_signif(comparisons=list(c("Sitting", "Heavy"), c("Light", "Heavy")), annotations=c("**", "*"), y=c(13.3, 12.75))+
  labs(title="Confederate: Speech Rate", y="Speech Rate", caption="**: |t| > 3; *: |t| > 2")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))
dev.off()

df$Condition <- relevel(df$Condition, ref="Sitting")

summary(lm(speechRateIPU ~ Condition, df))



# Confederate - breathing rate

load(paste0(folder, "DataBreathing.RData"))

dat <- brm %>%
  filter(Role=="Confederate", !duplicated(file)) %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "Condition"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Sitting","Light","Heavy")))

png(paste0(folderPlot, "conf-breathRate.png"), height=550, width=550)
ggplot(dat, aes(Condition, breathRate))+
  geom_point(color="#482173FF", size=3)+
  scale_x_discrete(limits = order)+
  geom_signif(comparisons=list(c("Sitting", "Light"), c("Light", "Heavy")), annotations=c("***", "*"), y=25.5)+
  labs(title="Confederate: Breathing Rate", y="Breathing Rate", caption="***: |t| > 9; *: |t| ~ 2")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))
dev.off()

dat$Condition <- relevel(dat$Condition, ref="Light")

summary(lm(breathRate ~ Condition, dat))



# Confederate - cycle durations

load(paste0(folder, "DataBreathing.RData"))

dat <- brm %>%
  filter(Role=="Confederate") %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "Condition"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Sitting","Light","Heavy"))) %>%
  mutate(logCycleDur = log(cycleDur))

png(paste0(folderPlot, "conf-cycleDurations-Condition.png"), height=550, width=550)
ggplot(dat, aes(Condition, cycleDur))+
  geom_boxplot(fill="#39558CFF")+
  scale_x_discrete(limits = order)+
  geom_signif(comparisons=list(c("Sitting", "Light"), c("Light", "Heavy")), annotations=c("***", "**"))+
  labs(title="Confederate: Breath Cycle Durations", y="Breath Cycle Durations", caption="***: |t| > 6; *: |t| > 2")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))
dev.off()

png(paste0(folderPlot, "conf-cycleDurations-breathCycle.png"), height=550, width=550)
ggplot(dat, aes(breathCycle, cycleDur))+
  geom_point(color="#482173FF")+
  labs(title="Confederate: Breath Cycle Durations", y="Breath Cycle Durations", x="Breath Cycle", caption="|t| > 3")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))
dev.off()

png(paste0(folderPlot, "conf-cycleDurations-breathCycleCondition.png"), height=550, width=550)
ggplot(dat, aes(breathCycle, cycleDur))+
  geom_point(color="#482173FF")+
  labs(title="Confederate: Breath Cycle Durations", y="Breath Cycle Durations", x="Breath Cycle")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))+
  facet_wrap(~Condition)
dev.off()

dat$Condition <- relevel(dat$Condition, ref="Light")

summary(lm(cycleDur ~ Condition * breathCycle, dat))


# Confederate - number of IPUs

load(paste0(folder, "DataBreathing.RData"))

dat <- brm %>%
  filter(Role=="Confederate") %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "Condition"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Sitting","Light","Heavy")))

png(paste0(folderPlot, "conf-numberIPUs.png"), height=550, width=550)
ggplot(dat, aes(Condition, numberIPUs))+
  geom_violin(fill="#39558CFF")+
  geom_boxplot(width=0.1)+
  geom_signif(comparisons=list(c("Light", "Heavy")), annotations=c("*"))+
  labs(title="Confederate: IPUs per Breath Cycle", y="Number of IPUs", caption="*: |t| > 2")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))
dev.off()

dat$Condition <- relevel(dat$Condition, ref="Light")
dat$numberIPUs <- as.factor(dat$numberIPUs)

summary(MASS::polr(numberIPUs ~ Condition * breathCycle, data=dat, Hess=TRUE))


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

# Participants - f0

load(paste0(folder, "DataSpeech.RData"))

df <- fsm %>%
  filter(Task == "Free", Role == "Participant") %>%
  mutate(f0IPUz = (f0mean - mean(f0mean, na.rm=TRUE)) / sd(f0mean, na.rm=TRUE),
         f0filez = (f0IPUmean - mean(f0IPUmean, na.rm=TRUE)) / sd(f0IPUmean, na.rm=TRUE))

png(paste0(folderPlot, "part-f0-condition.png"), height=550, width=550)
ggplot(df, aes(Condition, f0mean))+
  geom_boxplot(fill="#39558CFF")+
  labs(title="Participants: f0", y="f0")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))+
  scale_x_discrete(limits = orderWithBase)
dev.off()

png(paste0(folderPlot, "part-f0-confGender.png"), height=550, width=550)
ggplot(df, aes(as.factor(ConfGenderF), f0mean))+
  geom_boxplot(color="#482173FF")+
  labs(title="Participants: f0", y="f0", x="Perception of Confederate's Gender", caption="|t| > 2")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))
dev.off()

summary(lmer(f0IPUz ~ ConfGenderF + (1 + Condition | Speaker), df))


# Participants - speech rate

load(paste0(folder, "DataSpeech.RData"))

df <- fsm %>%
  filter(Task == "Free", Role == "Participant")

png(paste0(folderPlot, "part-speechRate-condition.png"), height=550, width=550)
ggplot(df, aes(Condition, speechRateIPU))+
  geom_boxplot(fill="#39558CFF")+
  labs(title="Participants: Speech Rate", y="Speech Rate")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))+
  scale_x_discrete(limits = orderWithBase)
dev.off()

png(paste0(folderPlot, "part-speechRate-InterEnjoy.png"), height=550, width=550)
ggplot(df, aes(InterEnjoy, speechRateIPU))+
  geom_point(color="#482173FF")+
  labs(title="Participants: Speech Rate", y="Speech Rate", caption="|t| > 2")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))
dev.off()

png(paste0(folderPlot, "part-speechRate-Order.png"), height=550, width=550)
ggplot(df, aes(Order, speechRateIPU))+
  geom_point(color="#482173FF")+
  labs(title="Participants: Speech Rate", y="Speech Rate", caption="|t| > 2")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))
dev.off()

png(paste0(folderPlot, "part-speechRate-cycleDur.png"), height=550, width=550)
ggplot(df, aes(breathCycleDur, speechRateIPU))+
  geom_point(color="#482173FF")+
  labs(title="Participants: Speech Rate", y="Speech Rate", caption="|t| > 3")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))
dev.off()

summary(s5 <- lmer(speechRateIPU ~ InterEnjoy + Order +  breathCycleDur + (1 | Speaker),  df %>% filter(!is.na(breathCycleDur))))

# Participants - breath rate - ACTS

load(paste0(folder, "DataBreathing.RData"))

dat <- brm %>%
  filter(Role=="Participant", act!="speaking", !duplicated(file)) %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Sitting","Light","Heavy"))) %>%
  mutate(across(act, factor, levels=c("watching","listening")))

png(paste0(folderPlot, "part-acts-breathRate-condition.png"), height=550, width=550)
ggplot(dat, aes(Condition, breathRate))+
  geom_boxplot(fill="#39558CFF")+
  labs(title="Participants: Breathing Rate (not speaking)", y="Breathing Rate")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))+
  scale_x_discrete(limits = order)
dev.off()

png(paste0(folderPlot, "part-acts-breathRate-actAndCondition.png"), height=550, width=550)
ggplot(dat, aes(act, breathRate))+
  geom_boxplot(fill="#39558CFF")+
  labs(title="Participants: Breathing Rate", y="Breathing Rate")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))+
  facet_wrap(~Condition)
dev.off()

png(paste0(folderPlot, "part-acts-breathRate-act.png"), height=550, width=550)
ggplot(dat, aes(act, breathRate))+
  geom_boxplot(fill="#39558CFF")+
  labs(title="Participants: Breathing Rate", y="Breathing Rate", caption="***: |t| > 4")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))+
  geom_signif(comparisons=list(c("watching", "listening")), annotations=c("***"))
dev.off()

summary(lmer(breathRate ~ act + (1 + act|Speaker), dat))

# Participants - cycle durations - ACTS

load(paste0(folder, "DataBreathing.RData"))

dat <- brm %>%
  filter(Role=="Participant", act!="speaking") %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Sitting","Light","Heavy"))) %>%
  mutate(across(act, factor, levels=c("watching","listening"))) %>%
  mutate(logCycleDur = log(cycleDur)) %>%
  mutate(cycleDurz = (cycleDur - mean(cycleDur)) / sd(cycleDur)) %>%
  filter(abs(cycleDurz) < 2)

png(paste0(folderPlot, "part-acts-cycleDur-condition.png"), height=550, width=550)
ggplot(dat, aes(Condition, cycleDur))+
  geom_boxplot(fill="#39558CFF")+
  labs(title="Participants: Breath Cycle Duration", y="Breath Cycle Duration")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))+
  scale_x_discrete(limits = order)
dev.off()

png(paste0(folderPlot, "part-acts-cycleDur-act.png"), height=550, width=550)
ggplot(dat %>% filter(cycleDur < 8), aes(act, cycleDur))+
  geom_boxplot(fill="#39558CFF")+
  labs(title="Participants: Breath Cycle Duration", y="Breath Cycle Duration", caption="***: |t| > 5")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))+
  geom_signif(comparisons=list(c("watching", "listening")), annotations=c("***"))
dev.off()

png(paste0(folderPlot, "part-acts-cycleDur-actAndCycle.png"), height=550, width=550)
ggplot(dat, aes(breathCycle, cycleDur))+
  geom_point(color="#482173FF")+
  labs(title="Participants: Breath Cycle Duration", y="Breath Cycle Duration", caption="|t| > 2")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))+
  facet_wrap(~act)
dev.off()

summary(lmer(logCycleDur ~ act * breathCycle + (1 + act | Speaker), dat))


# Participants - breath rate - SPEAKING

load(paste0(folder, "DataBreathing.RData"))

dat <- brm %>%
  filter(Role=="Participant", act=="speaking") %>%
  filter(!duplicated(file)) %>% # for some reason if I do `!duplicated()` in the line above, it weirdly deletes a bunch of rows that shouldn't be
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy")))

png(paste0(folderPlot, "part-speaking-breathRate-condition.png"), height=550, width=550)
ggplot(dat, aes(Condition, breathRate))+
  geom_boxplot(fill="#39558CFF")+
  labs(title="Participants: Breathing Rate (speaking)", y="Breathing Rate")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))+
  scale_x_discrete(limits = orderWithBase)
dev.off()

summary(lmer(breathRate ~ Condition + (1 | Speaker), dat))


# Participants - breath cycle duration - SPEAKING

load(paste0(folder, "DataBreathing.RData"))

dat <- brm %>%
  filter(Role=="Participant", act=="speaking") %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy"))) %>%
  mutate(logCycleDur = log(cycleDur))

png(paste0(folderPlot, "part-speaking-cycleDur-condition.png"), height=550, width=550)
ggplot(dat, aes(Condition, cycleDur))+
  geom_boxplot(fill="#39558CFF")+
  labs(title="Breath Cycle Duration (speaking)", y="Breath Cycle Duration")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))+
  scale_x_discrete(limits = orderWithBase)
dev.off()

dat$Condition <- relevel(dat$Condition, ref="Sitting")
summary(lmer(logCycleDur ~ Condition + (0 + Condition | Speaker), dat))


# Participants - number IPUs

load(paste0(folder, "DataBreathing.RData"))

dat <- brm %>%
  filter(Role=="Participant", act=="speaking") %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("numberIPUs", "Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy")))

png(paste0(folderPlot, "part-numberIPUs-breathCycle.png"), height=550, width=550)
ggplot(dat, aes(as.factor(breathCycle), as.integer(numberIPUs)))+
  geom_violin(fill="#39558CFF")+
  labs(title="Participants: IPUs per Cycle", y="Number of IPUs per Cycle", x="Breath Cycle", caption="|t| > 4")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))
dev.off() 

png(paste0(folderPlot, "part-numberIPUs-BMI.png"), height=550, width=550)
ggplot(dat, aes(as.factor(BMI), as.integer(numberIPUs)))+
  geom_violin(fill="#39558CFF")+
  labs(title="Participants: IPUs per Cycle", y="Number of IPUs per Cycle", x="BMI", caption="|t| > 3")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))
dev.off()

png(paste0(folderPlot, "part-numberIPUs-InterEnjoy.png"), height=550, width=550)
ggplot(dat, aes(as.factor(InterEnjoy), as.integer(numberIPUs)))+
  geom_violin(fill="#39558CFF")+
  labs(title="Participants: IPUs per Cycle", y="Number of IPUs per Cycle", x="Enjoyment of Interaction", caption="|t| > 4")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))
dev.off()

summary(MASS::polr(numberIPUs ~ breathCycle + BMI + InterEnjoy, data=dat %>% filter(!is.na(BMI)), Hess=TRUE))




library(tidyverse)
library(viridis)
library(ggsignif)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folderPlot <- "C:/Users/tomof/Documents/1HU/Presentations/Speech respiration workshop - 17.6.22/"

order <- c("Sitting", "Light", "Heavy")

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
  scale_x_discrete(limits = orderCond)+
  geom_signif(comparisons=list(c("Sitting", "Light"), c("Light", "Heavy")), annotations=c("***", "***"))+
  labs(title="Confederate: f0", y="f0", caption="***: < 0.001")+
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
  scale_x_discrete(limits = orderCond)+
  # geom_signif(comparisons=list(c("Sitting", "Light"), c("Light", "Heavy")), annotations=c("***", "***"))+
  labs(title="Confederate: f0", y="f0", caption="***: < 0.001")+
  theme(title = element_text(size=22), axis.title = element_text(size=20), axis.text = element_text(size=18))
dev.off()

df$Condition <- relevel(df$Condition, ref="Light")

summary(lm(speechRateIPU ~ Condition, df))

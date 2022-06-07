library(lme4)
library(tidyverse)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"

orderCond <- c("Sitting", "Light", "Heavy")

# SPEECH

load(paste0(folder, "DataSpeech.RData"))

df <- fsm %>%
  filter(Task == "Free", Speaker == "Confederate") %>%
  select(c("file", "Speaker", "IPU", "f0mean", "f0IPUmean", "speechRateIPU", "durSpeech", "durPauses", "speechRate", "articRate", "Condition", "breathCycleDurMean", "breathRate", "breathCycleDur")) %>%
  mutate(f0IPUz = (f0mean - mean(f0mean, na.rm=TRUE)) / sd(f0mean, na.rm=TRUE),
         f0filez = (f0IPUmean - mean(f0IPUmean, na.rm=TRUE)) / sd(f0IPUmean, na.rm=TRUE))

df$Condition <- relevel(df$Condition, ref="Sitting")

### f0

ggplot(df, aes(Condition, f0IPUz))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)

plot(df$breathRate, df$f0IPUz)

# > names(df)
# [1] "file"               "Speaker"            "IPU"                "f0mean"             "f0IPUmean"          "speechRateIPU"      "durSpeech"         
# [8] "durPauses"          "speechRate"         "articRate"          "Condition"          "breathCycleDurMean" "breathRate"         "breathCycleDur"    
# [15] "f0IPUz"             "f0filez"        

summary(m1 <- lm(f0IPUz ~ Condition, df))
summary(m1a <- lmer(f0IPUz ~ Condition + (1 | file), df))
anova(m1, m1a)
AIC(m1, m1a) # AIC for m1 is different in anova() and AIC() ???
# but either way we should keep m1

summary(m2 <- lm(f0IPUz ~ breathRate, df))
anova(m1, m2)
AIC(m1, m2)

# Condition and breathRate are of course correlated, so the models weren't very different
# the AIC of m1 is slightly lower, so let's stick with that (even if the difference is irrelevant and insignificant here)

hist(resid(m1))
qqnorm(resid(m1));qqline(resid(m1))
plot(fitted(m1), resid(m1)) # not ok! interval data???


### speech rate

ggplot(df, aes(Condition, speechRateIPU))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)

summary(s1 <- lm(speechRateIPU ~ Condition, df))
summary(s1a <- lmer(speechRateIPU ~ Condition + (1 | file), df)) # singular!
anova(s1, s1a)
AIC(s1, s1a)

summary(s2 <- lm(speechRateIPU ~ breathRate, df))
AIC(s1, s2)
# same here. we stick with s1

hist(resid(s1))
qqnorm(resid(s1));qqline(resid(s1))
plot(fitted(s1), resid(s1)) # not ok! interval data???


# BREATHING

load(paste0(folder, "DataBreathing.RData"))

### breath rate

dat <- brm %>%
  filter(Role=="Confederate", !duplicated(file)) %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "Condition"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Sitting","Light","Heavy")))

ggplot(dat, aes(Condition, breathRate))+
  geom_point()

summary(b1 <- lm(breathRate ~ Condition, dat))

### cycle Durations

dat <- brm %>%
  filter(Role=="Confederate") %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "Condition"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Sitting","Light","Heavy"))) %>%
  mutate(logCycleDur = log(cycleDur))

ggplot(dat, aes(Condition, cycleDur))+
  geom_point()

ggplot(dat, aes(breathCycle, cycleDur))+
  geom_point()+
  facet_wrap(~Condition)


summary(c1 <- lm(cycleDur ~ Condition, dat))
summary(c2 <- lm(cycleDur ~ Condition + breathCycle, dat))
summary(c3 <- lm(cycleDur ~ Condition * breathCycle, dat))
AIC(c1, c2)
AIC(c2, c3)

# c3 is the best model

# shorter cycles with more physical effort (related to increase in breath rate)
# shorter cycles the further into the speaking period : easier to visualize if you collapse all conditions, but also:
# more varied cycle durations when sitting, and less and less with physical effort

### number of IPUs

ggplot(dat, aes(Condition, numberIPUs))+
  geom_boxplot()

ggplot(dat, aes(breathCycle, numberIPUs))+
  geom_point()

dat$numberIPUs <- as.factor(dat$numberIPUs)

summary(n1 <- MASS::polr(numberIPUs ~ Condition, data=dat, Hess=TRUE))
summary(n2 <- MASS::polr(numberIPUs ~ Condition + breathCycle, data=dat, Hess=TRUE))
summary(n3 <- MASS::polr(numberIPUs ~ Condition * breathCycle, data=dat, Hess=TRUE))
AIC(n1, n2)
AIC(n2, n3)

# n3 wins!10292.33
library(lme4)
library(tidyverse)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"

orderCond <- c("Sitting", "Light", "Heavy")

# SPEECH

load(paste0(folder, "DataSpeech.RData"))

df <- fsm %>%
  filter(Task == "Free", Speaker == "Confederate") %>%
  select(c("file", "Speaker", "IPU", "f0raw", "f0IPUmean", "speechRateIPU", "durSpeech", "durPauses", "speechRate", "articRate", "Condition", "breathCycleDurMean", "breathRate", "breathCycleDur")) %>%
  mutate(f0IPUz = (f0raw - mean(f0raw, na.rm=TRUE)) / sd(f0raw, na.rm=TRUE),
         f0filez = (f0IPUmean - mean(f0IPUmean, na.rm=TRUE)) / sd(f0IPUmean, na.rm=TRUE))

df$Condition <- relevel(df$Condition, ref="Sitting")

### f0

ggplot(df, aes(Condition, f0IPUz))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)

plot(df$breathRate, df$f0IPUz)

# > names(df)
# [1] "file"               "Speaker"            "IPU"                "f0raw"             "f0IPUmean"          "speechRateIPU"      "durSpeech"         
# [8] "durPauses"          "speechRate"         "articRate"          "Condition"          "breathCycleDurMean" "breathRate"         "breathCycleDur"    
# [15] "f0IPUz"             "f0filez"        

summary(m1 <- lm(f0IPUz ~ Condition, df))
summary(m1a <- lmer(f0IPUz ~ Condition + (1 | file), df))
anova(m1, m1a)
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

df$Condition <- relevel(df$Condition, ref="Light")

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


dat$Condition <- relevel(dat$Condition, ref="Light")
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


summary(c1 <- lm(logCycleDur ~ Condition, dat))
summary(c2 <- lm(logCycleDur ~ Condition + breathCycle, dat))
summary(c3 <- lm(logCycleDur ~ Condition * breathCycle, dat))
AIC(c1, c2)
AIC(c2, c3)

# c3 is the best model

hist(resid(c3))
qqnorm(resid(c3));qqline(resid(c3))
plot(fitted(c3), resid(c3))

hist(resid(c1))
qqnorm(resid(c1));qqline(resid(c1))
plot(fitted(c1), resid(c1))


# shorter cycles with more physical effort (related to increase in breath rate)
# shorter cycles the further into the speaking period : easier to visualize if you collapse all conditions, but also:
# more varied cycle durations when sitting, and less and less with physical effort

### number of IPUs

ggplot(dat, aes(Condition, numberIPUs))+
  geom_boxplot()

ggplot(dat, aes(breathCycle, numberIPUs))+
  geom_point()+
  facet_wrap(~Condition)


# percentage of breath cycles that had n IPUs (per condition)

d <- dat %>% filter(Condition=="Sitting")
length(d$breathCycle) # 142
table(d$numberIPUs)

# 1 IPU: 63 / 142 = 0.443662
# 2 IPUs: 38 / 142 = 0.2676056
# 3 IPUs: 22 / 142 = 0.1549296
# 4 IPUs: 13 / 142 = 0.0915493
# 5 IPUs: 3 / 142 = 0.02112676
# 6 IPUs: 2 / 142 = 0.01408451
# 7 IPUs: 1 / 142 = 0.007042254

d <- dat %>% filter(Condition=="Light")
length(d$breathCycle) # 199
table(d$numberIPUs)

# 1 IPU: 103 / 199 = 0.5175879
# 2 IPUs: 77 / 199 = 0.3869347
# 3 IPUs: 17 / 199 = 0.08542714
# 4 IPUs: 2 / 199 = 0.01005025

d <- dat %>% filter(Condition=="Heavy")
length(d$breathCycle) # 173
table(d$numberIPUs)

# 1 IPU: 75 / 173 = 0.433526
# 2 IPUs: 87 / 173 = 0.5028902
# 3 IPUs: 9 / 173 = 0.05202312
# 4 IPUs: 2 / 173 = 0.01156069

p <- data.frame(matrix(nrow=0, ncol=3))
names(p) = c("Condition", "NumberIPUs", "Percentage")
p[nrow(p)+1,] <- c("Sitting", 1, 0.443662)
p[nrow(p)+1,] <- c("Sitting", 2, 0.2676056)
p[nrow(p)+1,] <- c("Sitting", 3, 0.1549296)
p[nrow(p)+1,] <- c("Sitting", 4, 0.0915493)
p[nrow(p)+1,] <- c("Sitting", 5, 0.02112676)
p[nrow(p)+1,] <- c("Sitting", 6, 0.01408451)
p[nrow(p)+1,] <- c("Sitting", 7, 0.007042254)
p[nrow(p)+1,] <- c("Light", 1, 0.5175879)
p[nrow(p)+1,] <- c("Light", 2, 0.3869347)
p[nrow(p)+1,] <- c("Light", 3, 0.08542714)
p[nrow(p)+1,] <- c("Light", 4, 0.01005025)
p[nrow(p)+1,] <- c("Heavy", 1, 0.433526)
p[nrow(p)+1,] <- c("Heavy", 2, 0.5028902)
p[nrow(p)+1,] <- c("Heavy", 3, 0.05202312)
p[nrow(p)+1,] <- c("Heavy", 4, 0.01156069)

p$Condition <- as.factor(p$Condition)
p$NumberIPUs <- as.integer(p$NumberIPUs)
p$Percentage <- as.numeric(p$Percentage)

ggplot(p, aes(NumberIPUs, Percentage))+
  geom_point()+
  facet_wrap(~Condition)

# end of percentage


dat$numberIPUs <- as.factor(dat$numberIPUs)

summary(n1 <- MASS::polr(numberIPUs ~ Condition, data=dat, Hess=TRUE))
summary(n2 <- MASS::polr(numberIPUs ~ Condition + breathCycle, data=dat, Hess=TRUE))
summary(n3 <- MASS::polr(numberIPUs ~ Condition * breathCycle, data=dat, Hess=TRUE))
AIC(n1, n2)
AIC(n2, n3)

# n3 wins!
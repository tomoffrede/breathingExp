library(lme4)
library(tidyverse)
# library(influence.ME)
# library(MASS)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"

load(paste0(folder, "DataBreathing.RData"))

orderCond <- c("Sitting", "Light", "Heavy")
orderCondBase <- c("Baseline", "Sitting", "Light", "Heavy")
orderAct <- c("watching", "listening")

# > names(brm)
# [1] "Speaker"            "file"               "act"                "breathCycle"        "cycleDur"           "numberBreathCycles" "breathCycleDurMean"
# [8] "breathRate"         "numberIPUs"         "Condition"          "Task"               "List"               "Age"                "Height"            
# [15] "Weight"             "Gender"             "Education"          "OtherL1"            "ConfFriendly"       "InterEnjoy"         "ConfGenderM"       
# [22] "ConfGenderF"        "Order"              "Topic"              "BMI"                "GEPAQ.F"            "GEPAQ.M"            "TMF.F"             
# [29] "TMF.M"              "Role" 

##### ACT: NOT SPEAKING

##### breathing rate

dat <- brm %>%
  filter(Role=="Participant", act!="speaking", !duplicated(file)) %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Sitting","Light","Heavy"))) %>%
  mutate(across(act, factor, levels=c("watching","listening")))

dat$Condition <- relevel(dat$Condition, ref = "Sitting")
dat$act <- relevel(dat$act, ref = "watching")

ggplot(dat, aes(Condition, breathRate))+
  geom_boxplot()

ggplot(dat, aes(act, breathRate))+
  geom_boxplot()

ggplot(dat, aes(Condition, breathRate))+
  geom_boxplot()+
  facet_wrap(~act)

ggplot(dat, aes(act, breathRate))+
  geom_boxplot()+
  facet_wrap(~Condition)

summary(m1 <- lmer(breathRate ~ Condition + (1|Speaker), dat))

summary(m2 <- lmer(breathRate ~ Condition + act + (1|Speaker), dat))
anova(m1, m2)
# act improved the model a lot: listening has higher breath rates than watching

# so is the model better without Condition?
summary(m2a <- lmer(breathRate ~ act + (1 + act|Speaker), dat))
anova(m2, m2a)
# the model is better without condition

summary(m3 <- lmer(breathRate ~ Condition * act + (1|Speaker), dat))
anova(m2a, m3)
# the interaction doesn't improve the model either

summary(m4 <- lmer(breathRate ~ act + BMI + (1|Speaker), dat))
anova(m2a, m4) # tried but didn't improve the model: ConfFriendly, InterEnjoy, ConfGenderF, TMF.F, BMI (using dat %>% filter(!is.na(BMI)))

summary(m4 <- lmer(breathRate ~ act + (1 + act | Speaker), dat))
anova(m2a, m4, refit=FALSE) # the random slope for act (with interaction with intercept for speaker) gave a single fit and higher AIC than just the intercept

summary(m4 <- lmer(breathRate ~ act + (1 | Speaker) + (act | Speaker), dat))
anova(m2a, m4, refit=FALSE) # this isn't good either -- also tried with (0 + act | Speaker)

# m2a looks like the best model

hist(resid(m2a))
qqnorm(resid(m2a));qqline(resid(m2a))
plot(fitted(m2a), resid(m2a))

# the residuals look ok

# although this is the best model, the plot show that there's a clear trend of sitting < light < heavy (at least during listening)

############################################################################

##### cycle Durations

dat <- brm %>%
  filter(Role=="Participant", act!="speaking") %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Sitting","Light","Heavy"))) %>%
  mutate(across(act, factor, levels=c("watching","listening"))) %>%
  mutate(logCycleDur = log(cycleDur))

dat$Condition <- relevel(dat$Condition, ref = "Sitting")
dat$act <- relevel(dat$act, ref = "watching")

ggplot(dat, aes(Condition, cycleDur))+
  geom_boxplot()

ggplot(dat, aes(act, cycleDur))+
  geom_boxplot()

ggplot(dat, aes(breathCycle, cycleDur))+
  geom_point()+
  facet_wrap(~act)

ggplot(dat, aes(act, cycleDur))+
  geom_boxplot()+
  facet_wrap(~Condition)

ggplot(dat, aes(Condition, cycleDur))+
  geom_boxplot()+
  facet_wrap(~act)

d <- dat %>% group_by(breathCycle) %>% mutate(cM = mean(cycleDur))
dw <- d %>% filter(act=="watching")
dl <- d %>% filter(act=="listening")
plot(d$breathCycle, d$cM, main="cycle duration mean clearly goes down with breath cycle!", ylab="mean of cycle durations")
plot(dw$breathCycle, dw$cM, main="cycle duration mean clearly goes down with breath cycle!", ylab="mean of cycle durations")
plot(dl$breathCycle, dl$cM, main="cycle duration mean clearly goes down with breath cycle!", ylab="mean of cycle durations")

summary(c1 <- lmer(logCycleDur ~ Condition + (1 | Speaker), dat))

summary(c2 <- lmer(logCycleDur ~ Condition + act + (1 | Speaker), dat))
anova(c1, c2)

# better without condition?
summary(c2a <- lmer(logCycleDur ~ act + (1 | Speaker), dat))
anova(c2, c2a)
# condition doesn't change the AIC, so we should keep the simpler model

summary(c3 <- lmer(logCycleDur ~ Condition * act + (1 | Speaker), dat))
anova(c2, c3)

summary(c3 <- lmer(logCycleDur ~ act  + breathCycle + (1 | Speaker), dat))
anova(c2a, c3)
# throughout the file, people produce shorter and shorter breath cycles

summary(c4 <- lmer(logCycleDur ~ act * breathCycle + (1 | Speaker), dat))
anova(c3, c4)
# the interaction improved the model
# looking at the plots, the pattern seems to be the same; it's only that in watching there are fewer breath cycles (shorter recording)

summary(c5 <- lmer(logCycleDur ~ act * breathCycle + BMI + (1 | Speaker), dat))
anova(c4, c5)
# didn't improve the model: ConfFriendly, InterEnjoy, ConfGenderF, BMI

summary(c5 <- lmer(logCycleDur ~ act * breathCycle + (1 + act | Speaker), dat))
anova(c4, c5, refit=FALSE)
# the random slope and its interaction with the random intercept improved the model a lot

# c5 seems like the best model

plot(fitted(c5), resid(c5)) # the residuals don't seem ok!
hist(resid(c5))
qqnorm(resid(c5));qqline(resid(c5)) # I think doing the log of the cycle duration improved the distribution _a bit_

#########################

##### ACT: SPEAKING

##### breathing rate

load(paste0(folder, "DataBreathing.RData"))

dat <- brm %>%
  filter(Role=="Participant", act=="speaking") %>%
  filter(!duplicated(file)) %>% # for some reason if I do `!duplicated()` in the line above, it weirdly deletes a bunch of rows that shouldn't be
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy")))

ggplot(dat, aes(Condition, breathRate))+
  geom_boxplot()

dat$Condition <- relevel(dat$Condition, ref = "Sitting")


summary(b1 <- lmer(breathRate ~ Condition + (1 | Speaker), dat))
# during speaking, a decrease in breathing rate with condition (but the t values are low)

summary(b2 <- lmer(breathRate ~ Condition + ConfGenderF + (1 | Speaker), dat))
anova(b1, b2)
# didn't improve the model: BMI (using dat %>% filter(!is.na(BMI))), TMF.F, ConfFriendly, InterEnjoy, ConfGenderF

# so we can say that there's a downward trend in breathing rate with condition, although the effect is very small (t values < 2)

hist(resid(b1))
qqnorm(resid(b1));qqline(resid(b1))
plot(fitted(b1), resid(b1))

# the residuals seem ok except for one outlier


##### breath cycle durations

dat <- brm %>%
  filter(Role=="Participant", act=="speaking") %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy"))) %>%
  mutate(logCycleDur = log(cycleDur))

ggplot(dat, aes(Condition, cycleDur))+
  geom_boxplot()

dat$Condition <- relevel(dat$Condition, ref="Sitting")

summary(c1 <- lmer(logCycleDur ~ Condition + (1 | Speaker), dat))

# sitting = baseline, but light and heavy have higher cycle durations

summary(c2 <- lmer(logCycleDur ~ Condition + (1 + Condition | Speaker), dat))
anova(c1, c2, refit=FALSE)
# the t values are now below 2, but the AIC is considerably lower
# but now there's a singularity issue!

summary(c3 <- lmer(logCycleDur ~ Condition + TMF.F + (1 + Condition | Speaker), dat))
anova(c2, c3) # didn't improve the model: BMI, ConfFriendly, ConfGenderF, InterEnjoy, TMF.F

plot(fitted(c2), resid(c2))


#### number of IPUs

dat <- brm %>%
  filter(Role=="Participant", act=="speaking") %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy"))) %>%
  mutate(logNipu = log(numberIPUs))

ggplot(dat, aes(Condition, numberIPUs))+
  geom_boxplot()

ggplot(dat, aes(breathCycle, numberIPUs))+
  geom_point()

ggplot(dat, aes(breathCycle, numberIPUs))+
  geom_point()+
  facet_wrap(~Condition)


dat$Condition <- relevel(dat$Condition, ref = "Sitting")

dat$numberIPUs <- as.factor(dat$numberIPUs)

summary(n1 <- MASS::polr(numberIPUs ~ Condition, data=dat, Hess=TRUE))

summary(n2 <- MASS::polr(numberIPUs ~ Condition + breathCycle, data=dat, Hess=TRUE))

# without condition
summary(n2a <- MASS::polr(numberIPUs ~ breathCycle, data=dat %>% filter(!is.na(BMI)), Hess=TRUE))
# lower AIC!

summary(n3 <- MASS::polr(numberIPUs ~ breathCycle + TMF.F, data=dat %>% filter(!is.na(BMI)), Hess=TRUE))
# not good predictors: ConfGenderF, ConfFriendly, TMF.F

summary(n3 <- MASS::polr(numberIPUs ~ breathCycle + BMI, data=dat %>% filter(!is.na(BMI)), Hess=TRUE))
# BMI reduced the AIC
# the higher the BMI, the fewer IPUs in the cycle (this isn't related to breath cycle duration (see analysis above),
# but could be because the higher the BMI, the less amplitude of breath you have available)

summary(n4 <- MASS::polr(numberIPUs ~ breathCycle + BMI + InterEnjoy, data=dat %>% filter(!is.na(BMI)), Hess=TRUE))
# InterEnjoy also improved the model
# the more they enjoyed the interaction, the more IPUs they had in each breath cycle
# this can be explained by difference in speech rate (see speech rate analysis): the more they enjoyed the interaction, the faster they spoke
# (this isn't related to cycle length; see analysis above)


## BUT THIS DOESN'T HAVE RANDOM EFFECTS!!
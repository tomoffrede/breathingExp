library(lme4)
library(tidyverse)
# library(influence.ME)
# library(MASS)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"

load(paste0(folder, "DataBreathing.RData"))

`%!in%` <- Negate(`%in%`)

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
  filter(Role=="Participant", act %in% c("listening", "watching"), !duplicated(file)) %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Sitting","Light","Heavy"))) %>%
  mutate(across(act, factor, levels=c("watching","listening"))) %>%
  group_by(Speaker)%>%
  mutate(inhalDurz = (inhalDur - mean(inhalDur, na.rm=TRUE) / sd(inhalDur, na.rm=TRUE)))%>%
  ungroup() %>%
  mutate(logInhalDur = log(inhalDur))

dat$logInhalDurz <- log(dat$inhalDurz)
# I calculated inhalation amplitude from the normalized values of the breathing wave form (0 to 1)
#  so should I still do the analysis based on z scores?
# and what about inhalation duration? when I look at the z scores, the difference between acts disappears. I'm not 100% sure why and how I should do this analysis

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

####################################################################

#### inhalation duration

dat <- brm %>%
  filter(Role=="Participant", act %in% c("listening", "watching"), !duplicated(file)) %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Sitting","Light","Heavy"))) %>%
  mutate(across(act, factor, levels=c("watching","listening"))) %>%
  group_by(Speaker)%>%
  mutate(inhalDurz = (inhalDur - mean(inhalDur, na.rm=TRUE) / sd(inhalDur, na.rm=TRUE)))%>%
  ungroup() %>%
  mutate(logInhalDur = log(inhalDur))

ggplot(dat, aes(Condition, inhalDur))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)+
  facet_wrap(~act)

summary(d1 <- lmer(logInhalDur ~ act + (1 | Speaker), dat))
summary(d2 <- lmer(logInhalDur ~ act + Condition + (1 | Speaker), dat))
anova(d1, d2) # the interaction between act and condition made the model even worse

summary(d2 <- lmer(logInhalDur ~ act + ConfGenderF + (1 | Speaker), dat))
anova(d1, d2) # not good: breathCycle, BMI (though BMI is almost good), TMF.F, GEPAQ.F, ConfFriendly, InterEnjoy, ConfGenderF

summary(d2 <- lmer(logInhalDur ~ act + (1 + act | Speaker), dat))
anova(d1, d2, refit=FALSE) # the random slope with the intercept didn't improve the model much

summary(d2 <- lmer(logInhalDur ~ act + (0 + act | Speaker) + (1 | Speaker), dat))
anova(d1, d2, refit=FALSE) # without the interaction it was even worse

# so the best model should be d1

hist(resid(d1))
qqnorm(resid(d1));qqline(resid(d1))
plot(fitted(d1), resid(d1))
# at first I'd done the analysis with the z scores of inhalation duration, but the residuals weren't normal or homoskedastic,
# but if we use the log of inhalation duration, they look at lot better!

# inhalation amplitude

dat <- brm %>%
  filter(Role=="Participant", act %in% c("listening", "watching"), !duplicated(file)) %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Sitting","Light","Heavy"))) %>%
  mutate(across(act, factor, levels=c("watching","listening"))) %>%
  mutate(logInhalAmp = log(inhalAmp))

ggplot(dat, aes(Condition, inhalAmp))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)+
  facet_wrap(~act)

summary(a1 <- lmer(logInhalAmp ~ act + (1 | Speaker), dat))
summary(a2 <- lmer(logInhalAmp ~ act + Condition + (1 | Speaker), dat))
anova(a1, a2) # one of the condition t values was actually 1.96, but this predictor made the model worse

summary(a2 <- lmer(logInhalAmp ~ act + ConfGenderF + (1 | Speaker), dat))
anova(a1, a2) # not good: breathCycle, BMI, ConfFriendly, InterEnjoy, TMF.F, ConfGenderF

names(dat)

# it seems that a1 is the best model

hist(resid(a1))
qqnorm(resid(a1));qqline(resid(a1))
plot(fitted(a1), resid(a1))

# again, the residuals looked a lot more normally distributed and homoskedastic with the log of the amplitude

#########################

##### ACT: SPEAKING

##### breathing rate

load(paste0(folder, "DataBreathing.RData"))

dat <- brm %>%
  filter(Role=="Participant", act=="speaking", Task=="Free") %>%
  filter(!duplicated(file)) %>% # for some reason if I do `!duplicated()` in the line above, it weirdly deletes a bunch of rows that shouldn't be
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy")))%>%
  mutate(logBreathRate = log(breathRate))

ggplot(dat, aes(Condition, breathRate))+
  geom_boxplot()

dat$Condition <- relevel(dat$Condition, ref = "Sitting")


summary(b1 <- lmer(logBreathRate ~ Condition + (1 | Speaker), dat))
# during speaking, a decrease in breathing rate with condition (but the t values are low)

summary(b2 <- lmer(breathRate ~ Condition + ConfGenderF + (1 | Speaker), dat))
anova(b1, b2)
# didn't improve the model: BMI (using dat %>% filter(!is.na(BMI))), TMF.F, ConfFriendly, InterEnjoy, ConfGenderF

# so we can say that there's a downward trend in breathing rate with condition, although the effect is very small (t values < 2)

hist(resid(b1))
qqnorm(resid(b1));qqline(resid(b1))
plot(fitted(b1), resid(b1))

# the residuals seem ok except for one outlier (log was better than raw values)


##### breath cycle durations

dat <- brm %>%
  filter(Role=="Participant", act=="speaking", Task=="Free") %>%
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
hist(resid(c2))
qqnorm(resid(c2));qqline(resid(c2))

#### inhalation duration

dat <- brm %>%
  filter(Role=="Participant", act=="speaking", Task=="Free") %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy")))%>%
  mutate(logInhalDur = log(inhalDur),
         inhalDurz = (inhalDur - mean(inhalDur)) / sd(inhalDur))

dat$Condition <- relevel(dat$Condition, ref="Sitting")

ggplot(dat, aes(Condition, inhalDur))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCondBase)

summary(d1 <- lmer(logInhalDur ~ Condition + (1 | Speaker), dat))
summary(d2 <- lmer(logInhalDur ~ Condition + numberIPUs + (1 | Speaker), dat))
anova(d1, d2) # the more IPUs the person is about to say, they longer they inhale for

summary(d3 <- lmer(logInhalDur ~ Condition + numberIPUs + ConfGenderF + (1 | Speaker), dat))
anova(d2, d3) # not good: breathCycle, BMI, ConfFriendly, InterEnjoy, ConfGenderF

summary(d3 <- lmer(logInhalDur ~ numberIPUs + (1 | Speaker), dat))
anova(d2, d3) # the model WITH condition is much better!

# Condition: no difference between the three conditions, but schon between baseline and interaction

# d2 should be the best model

hist(resid(d2))
qqnorm(resid(d2));qqline(resid(d2))
plot(fitted(d2), resid(d2))

# they look ok

#### inhalation amplitude

dat <- brm %>%
  filter(Role=="Participant", act=="speaking", Task=="Free") %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy")))%>%
  mutate(logInhalAmp = log(inhalAmp),
         inhalAmpz = (inhalAmp - mean(inhalAmp)) / sd(inhalAmp))

dat$Condition <- relevel(dat$Condition, ref="Sitting")

ggplot(dat, aes(Condition, inhalAmp))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCondBase)

summary(a1 <- lmer(logInhalAmp ~ Condition + (1 | Speaker), dat))
summary(a2 <- lmer(logInhalAmp ~ Condition + numberIPUs + (1 | Speaker), dat))
anova(a1, a2) # again, the more IPUs to be produced, the larger the amplitude (so people plan ahead)

summary(a3 <- lmer(logInhalAmp ~ Condition + numberIPUs + GEPAQ.F + (1 | Speaker), dat))
anova(a2, a3) # not good: breathCycle (though almost good), ConfFriendly, InterEnjoy, BMI, TMF.F

# GEPAQ.F actually improved the model, but we can't know why: we saw that it isn't a good measure of gender in our sample
# so we can't really explain theoretically why this inventory of personality traits correlates with higher inhalation amplitude
# so I say we should just leave this out

# so the best model should just ignore it

summary(a3 <- lmer(logInhalAmp ~ numberIPUs + (1 | Speaker), dat))
anova(a2, a3, refit=FALSE) # here the model isn't better with condition

# a3 should be the best model

hist(resid(a3))
qqnorm(resid(a3));qqline(resid(a3))
plot(fitted(a3), resid(a3))

# residuals are mostly ok I think, but not sure 

#### number of IPUs

dat <- brm %>%
  filter(Role=="Participant", act=="speaking", Task=="Free") %>%
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

##########################################################
##########################################################
##########################################################

# Joint reading!

#### inhalation duration

dat <- brm %>%
  filter(Role=="Participant", Task=="ReadJoint") %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy")))%>%
  mutate(logInhalDur = log(inhalDur),
         inhalDurz = (inhalDur - mean(inhalDur)) / sd(inhalDur))

dat$Condition <- relevel(dat$Condition, ref="Sitting")

ggplot(dat, aes(Condition, inhalDur))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)

ggplot(dat, aes(breathCycle, inhalDur))+
  geom_point()

summary(d1 <- lmer(logInhalDur ~ Condition + (1 | Speaker), dat))
summary(d2 <- lmer(logInhalDur ~ Condition + breathCycle + (1 | Speaker), dat))
anova(d1, d2) # not good: BMI, ConfFriendly, InterEnjoy, ConfGenderF, TMF.F
# the further into the text, the quicker the inhales

summary(d3 <- lmer(logInhalDur ~ breathCycle + (1 | Speaker), dat))
anova(d2, d3) # the model with condition is much better

summary(d4 <- lmer(logInhalDur ~ Condition * breathCycle + (1 | Speaker), dat))
anova(d2, d4) # the interaction didn't improve it

summary(d4 <- lmer(logInhalDur ~ Condition + breathCycle + (1 + Condition | Speaker), dat))
anova(d2, d4, refit=FALSE) # the random slope with intercept wasn't good

summary(d4 <- lmer(logInhalDur ~ Condition + breathCycle + (0 + Condition | Speaker) + (1 | Speaker), dat))
anova(d2, d4, refit=FALSE) # the random slope without the interaction was also bad

# so the best model should be d2
# condition: expected improvement, though difference between sitting and light not significant

hist(resid(d2))
qqnorm(resid(d2));qqline(resid(d2))
plot(fitted(d2), resid(d2))

# residuals look super ok!

#### inhalation amplitude

dat <- brm %>%
  filter(Role=="Participant", Task=="ReadJoint") %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy")))%>%
  mutate(logInhalAmp = log(inhalAmp),
         inhalAmpz = (inhalAmp - mean(inhalAmp)) / sd(inhalAmp))

dat$Condition <- relevel(dat$Condition, ref="Sitting")

ggplot(dat, aes(Condition, inhalAmp))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)

ggplot(dat, aes(breathCycle, inhalAmp))+
  geom_point()

summary(a1 <- lmer(logInhalAmp ~ Condition + (1 | Speaker), dat))
summary(a2 <- lmer(logInhalAmp ~ Condition + breathCycle + (1 | Speaker), dat))
summary(a3 <- lmer(logInhalAmp ~ breathCycle + (1 | Speaker), dat))
anova(a1, a2)
anova(a2, a3) # the model without condition is better (a3)

summary(a4 <- lmer(logInhalAmp ~ breathCycle + ConfGenderF + (1 | Speaker), dat))
anova(a3, a4) # not good: BMI, ConfFriendly, Interenjoy, ConfGenderF, TMF.F

# so a3 should be the best model

hist(resid(a3))
qqnorm(resid(a3));qqline(resid(a3))
plot(fitted(a3), resid(a3))

# residuals seem ok

###############
######## Inhalation: comparing ReadJoint and FreeSpeech

# Inhalation Duration

dat <- brm %>%
  filter(Role=="Participant", Task %in% c("ReadJoint", "Free"), act %!in% c("listening", "watching")) %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy")))%>%
  mutate(logInhalDur = log(inhalDur),
         inhalDurz = (inhalDur - mean(inhalDur)) / sd(inhalDur))

dat$Condition <- relevel(dat$Condition, ref="Sitting")

ggplot(dat, aes(Condition, inhalDur))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCondBase)+
  facet_wrap(~Task)

summary(d1 <- lmer(logInhalDur ~ Condition + (1 | Speaker), dat))
summary(d2 <- lmer(logInhalDur ~ Condition + Task + (1 | Speaker), dat))
anova(d1, d2) # Task is a good predictor

summary(d3 <- lmer(logInhalDur ~ Task + (1 | Speaker), dat))
anova(d2, d3) # but the model is still better with condition

summary(d3 <- lmer(logInhalDur ~ Condition * Task + (1 | Speaker), dat))
anova(d2, d3) # the interaction improves the model

summary(d4 <- lmer(logInhalDur ~ Condition * Task + TMF.F + (1 | Speaker), dat))
anova(d3, d4) # not good: breathCycle, BMI, GEPAQ.F, TMF.F

summary(d4 <- lmer(logInhalDur ~ Condition * Task + (1 + Condition | Speaker), dat))
anova(d3, d4, refit=FALSE) # the random slope with intercept improved the model

summary(d5 <- lmer(logInhalDur ~ Condition * Task + (1 + Condition | Speaker) + (0 + Task | Speaker), dat))
anova(d4, d5, refit=FALSE) # the random slope for Task also improved it

# best model should be d5

hist(resid(d5))
qqnorm(resid(d5));qqline(resid(d5))
plot(fitted(d5), resid(d5))

# residuals seem ok!

# Inhalation Amplitude

dat <- brm %>%
  filter(Role=="Participant", Task %in% c("ReadJoint", "Free"), act %!in% c("listening", "watching")) %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer) %>%
  mutate(across(Condition, factor, levels=c("Baseline", "Sitting","Light","Heavy")))%>%
  mutate(logInhalAmp = log(inhalAmp),
         inhalAmpz = (inhalAmp - mean(inhalAmp)) / sd(inhalAmp))

dat$Condition <- relevel(dat$Condition, ref="Sitting")

ggplot(dat, aes(Condition, inhalAmp))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCondBase)+
  facet_wrap(~Task)

ggplot(dat, aes(breathCycle, inhalAmp))+
  geom_point()

summary(a1 <- lmer(logInhalAmp ~ Condition + (1 | Speaker), dat))
summary(a2 <- lmer(logInhalAmp ~ Condition + Task + (1 | Speaker), dat))
anova(a1, a2) # a2 better

summary(a3 <- lmer(logInhalAmp ~ Condition * Task + (1 | Speaker), dat))
anova(a2, a3) # interaction not better

summary(a3 <- lmer(logInhalAmp ~ Condition + Task + breathCycle + (1 | Speaker), dat))
anova(a2, a3) # people breathed less deeper the longer into the text

summary(a4 <- lmer(logInhalAmp ~ Condition + Task + breathCycle + ConfFriendly + (1 | Speaker), dat))
anova(a3, a4) # the friendlier the confederate, the deeper the inhales

summary(a5 <- lmer(logInhalAmp ~ (Condition + Task + ConfFriendly)^2 + breathCycle +(1 | Speaker), dat))
anova(a4, a5) # interaction not good (though conditionLight : task had t > 2)

summary(a5 <- lmer(logInhalAmp ~ Condition + Task + breathCycle + ConfFriendly + (1 + Condition | Speaker), dat))
anova(a4, a5, refit=FALSE) # not good: BMI, InterEnjoy, ConfGenderF, TMF.F
# the random slope with intercept for Condition and speaker improved the model

summary(a6 <- lmer(logInhalAmp ~ Condition + Task + breathCycle + ConfFriendly + (1 + Condition | Speaker) + (1 + Task | Speaker), dat))
# model didn't converge like this (maybe too complex?) -- also tried with (0 + Task | Speaker)

# but try again to remove condition, since t values all kinda low
summary(a6 <- lmer(logInhalAmp ~ Task + breathCycle + ConfFriendly + (1 + Condition | Speaker), dat))
anova(a5, a6) # model is better without condition

# so a5 should be the best model

hist(resid(a5))
qqnorm(resid(a5));qqline(resid(a5))
plot(fitted(a5), resid(a5))

# the residuals look ok!
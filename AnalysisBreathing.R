library(lme4)
library(tidyverse)
library(influence.ME)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"

load(paste0(folder, "DataBreathing.RData"))

orderCond <- c("Sitting", "Light", "Heavy")
orderCondBase <- c("Baseline", "Sitting", "Light", "Heavy")
orderAct <- c("watching", "listening", "speaking")

# > names(brm)
# [1] "Speaker"            "file"               "act"                "breathCycle"        "cycleDur"           "numberBreathCycles" "breathCycleDurMean"
# [8] "breathRate"         "numberIPUs"         "Condition"          "Task"               "List"               "Age"                "Height"            
# [15] "Weight"             "Gender"             "Education"          "OtherL1"            "ConfFriendly"       "InterEnjoy"         "ConfGenderM"       
# [22] "ConfGenderF"        "Order"              "Topic"              "BMI"                "GEPAQ.F"            "GEPAQ.M"            "TMF.F"             
# [29] "TMF.M"              "Role" 

##### breathing rate

dat <- brm %>%
  filter(Role=="Participant") %>%
  filter(!duplicated(file)) %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer)

dat$Condition <- relevel(dat$Condition, ref = "Sitting")
dat$act <- relevel(dat$act, ref = "watching")

ggplot(dat, aes(Condition, breathRate))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCondBase)

ggplot(dat, aes(act, breathRate))+
  geom_boxplot()+
  scale_x_discrete(limits = orderAct)

summary(m1 <- lmer(breathRate ~ Condition + (1|Speaker) + (0 + Condition | Speaker), dat))
# the only difference seems to be baseline vs experimental conditions

summary(m2 <- lmer(breathRate ~ Condition + act + (1|Speaker) + (0 + Condition | Speaker), dat))
anova(m1, m2)
# act improved the model a lot

# so is the model better without Condition?
summary(m2a <- lmer(breathRate ~ act + (1|Speaker) + (0 + Condition | Speaker), dat))
anova(m2, m2a)
# condition still improved the model

summary(m3 <- lmer(breathRate ~ Condition * act + (1|Speaker) + (0 + Condition | Speaker), dat))
anova(m2, m3)
# the interaction made the model worse

summary(m3 <- lmer(breathRate ~ Condition + act + (1|Speaker) + (0 + Condition | Speaker) + (0 + act | Speaker), dat))
anova(m2, m3, refit=FALSE)
#  the random slope of act improved the model

summary(m4 <- lmer(breathRate ~ Condition + act + breathCycleDurMean + (1|Speaker) + (0 + Condition | Speaker) + (0 + act | Speaker), dat))
anova(m3, m4)
# mean cycle duration improved the model

summary(m5 <- lmer(breathRate ~ (Condition + act + breathCycleDurMean)^2 + (1|Speaker) + (0 + Condition | Speaker) + (0 + act | Speaker), dat))
anova(m4, m5)
# the interaction improves the model a lot

summary(m6 <- lmer(breathRate ~ (Condition + act + breathCycleDurMean)^2 + ConfFriendly + (1|Speaker) + (0 + Condition | Speaker) + (0 + act | Speaker), dat))
anova(m5, m6) # ConfFriendly didn't improve the model

summary(m6 <- lmer(breathRate ~ (Condition + act + breathCycleDurMean)^2 + InterEnjoy + (1|Speaker) + (0 + Condition | Speaker) + (0 + act | Speaker), dat))
anova(m5, m6) # InterEnjoy neither

summary(m6 <- lmer(breathRate ~ (Condition + act + breathCycleDurMean)^2 + ConfGenderF + (1|Speaker) + (0 + Condition | Speaker) + (0 + act | Speaker), dat))
anova(m5, m6) # gender neither (tried with ConfGenderF and M, and TMF.F and M)

summary(m7 <- lmer(breathRate ~ (Condition + act + breathCycleDurMean)^2 + (1 + Condition | Speaker) + (1 + act | Speaker), dat))

anova(m5, m7, refit=FALSE)
# adding the interaction between slope and intercept improved the model
# it doesn't make a difference if I add the interaction to act, Condition, or both


# the "best" model so far seems to be m7, but there are convergence issues!!!
# don't know how to fix this!!!!

############################################################################

##### cycle Durations

dat <- brm %>%
  filter(Role=="Participant") %>%
  mutate(breathCycle = gsub("cycle", "", breathCycle)) %>%
  mutate_at(c("Speaker", "act", "Condition", "Task", "Role"), as.factor) %>%
  mutate_at(c("numberIPUs", "numberBreathCycles", "breathCycle"), as.integer)

dat$Condition <- relevel(dat$Condition, ref = "Sitting")
dat$act <- relevel(dat$act, ref = "watching")

ggplot(dat, aes(Condition, cycleDur))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCondBase)+
  ggtitle("baseline seems higher because it is only speaking, where the others also have watching and listening ")

ggplot(dat, aes(act, cycleDur))+
  geom_boxplot()+
  scale_x_discrete(limits = orderAct)

ggplot(dat, aes(breathCycle, cycleDur))+
  geom_point()+
  scale_x_discrete(limits = orderAct)

d <- dat %>% group_by(breathCycle) %>% mutate(cM = mean(cycleDur))
plot(d$breathCycle, d$cM, main="cycle duration mean clearly goes down with breath cycle!", ylab="mean of cycle durations")

summary(c1 <- lmer(cycleDur ~ Condition + (1 | Speaker), dat))

summary(c2 <- lmer(cycleDur ~ Condition + act + (1 | Speaker), dat))
anova(c1, c2)

summary(c3 <- lmer(cycleDur ~ Condition * act + (1 | Speaker), dat))
anova(c2, c3)

summary(c4 <- lmer(cycleDur ~ Condition * act  + breathCycle + (1 | Speaker), dat))
anova(c3, c4)
# throughout their own speech, people produce shorter and shorter breath cycles

summary(c5 <- lmer(cycleDur ~ (Condition + act + breathCycle)^2 + (1 | Speaker), dat))
anova(c4, c5)
# the (two-way) interaction between the three factors improved the model

summary(c6 <- lmer(cycleDur ~ (Condition + act + breathCycle)^2 + ConfFriendly + (1 | Speaker), dat))
anova(c5, c6)
# ConfFriendly made the model worse

summary(c6 <- lmer(cycleDur ~ (Condition + act + breathCycle)^2 + InterEnjoy + (1 | Speaker), dat))
anova(c5, c6)
# InterEnjoy too

summary(c6 <- lmer(cycleDur ~ (Condition + act + breathCycle)^2 + ConfGenderF + (1 | Speaker), dat))
anova(c5, c6)
# gender too (tried with ConfGenderF and M, and TMF.F and M)

summary(c6 <- lmer(cycleDur ~ (Condition + act + breathCycle)^2 + (1 + Condition | Speaker), dat))
anova(c5, c6, refit=FALSE)
# the model gets better with the interaction between random intercept for speaker and random slope for condition er speaker

summary(c7 <- glmer(cycleDur ~ (Condition + act + breathCycle)^2 + (1 + Condition | Speaker) + (1 + act | Speaker), dat, control=glmerControl(optCtrl=list(maxfun=1e5))))
anova(c6, c7, refit=FALSE)
# adding the random slope for act (with or without interaction with intercept) reduces AIC, but the model has convergence issues


#########################

#### number of IPUs


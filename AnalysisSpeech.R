library(lme4)
library(tidyverse)
# library(influence.ME)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"

load(paste0(folder, "DataSpeech.RData"))

orderCond <- c("Baseline", "Sitting", "Light", "Heavy")
orderChange <- c("BS", "SL", "LH")

# ConfFriendly: how friendly the participant finds the confederate
# InterEnjoy: how much the participant enjoyed the interaction
# GenderDiff: difference between participants' gender identity and their perception of conf's gender expression
# GenderPercDiff: difference between confederate's gender identity and participants' perception of conf's gender expression
# ConfGender: participant's perception of confederate's gender

plot(fsm$BMI, fsm$f0raw)
#excluding the outlier:
fsm2 <- fsm[fsm$BMI < 26,]

cor.test(fsm2$BMI, fsm2$f0raw)

# apparently there's a significant weak correlation between BMI and f0, which is in line with Pisanski et al 2016.
# but this value is probably too high because there's multiple repeated BMI values (from the same person)
# so trying a regression controlling for participant:

lmer(f0IPUmean ~ BMI + (1 | Speaker), data=fsm %>% filter(!duplicated(file)))

fsm <- fsm %>%
  mutate(GenderDiffF = GEPAQ.F - ConfGenderF) %>%
  mutate(GenderDiffM = GEPAQ.M - ConfGenderM)

# it does seem to have a small effect

# > names(fsm)
# [1] "Speaker"       "file"          "IPU"           "f0raw"        "f0IPUmean"     "label"         "f0z"           "speechRateIPU"
# [9] "durSpeech"     "durPauses"     "speechRate"    "articRate"     "Condition"     "Task"          "List"          "Age"          
# [17] "Height"        "Weight"        "Gender"        "Education"     "OtherL1"       "ConfFriendly"  "InterEnjoy"    "ConfGenderM"  
# [25] "ConfGenderF"   "Order"         "Topic"         "BMI"           "GEPAQ.F"       "GEPAQ.M"       "TMF.F"         "TMF.M"        
# [33] "Role"          "GenderDiffF"   "GenderDiffM"  


# f0

fz <- fsm %>% 
  filter(Task == "Free", Role == "Participant") %>%
  group_by(Speaker) %>% 
  summarize(f0IPUz = (f0raw - f0raw(f0raw, na.rm=TRUE)) / sd(f0raw, na.rm=TRUE),
            f0filez = (f0IPUmean - mean(f0IPUmean, na.rm=TRUE)) / sd(f0IPUmean, na.rm=TRUE),
            IPU = IPU,
            ) %>%
  ungroup()



df <- fsm %>%
  filter(Task == "Free", Role == "Participant") %>%
  group_by(Speaker)%>%
  summarise(f0IPUz = (f0raw - mean(f0raw, na.rm=TRUE)) / sd(f0raw, na.rm=TRUE),
         f0filez = (f0IPUmean - mean(f0IPUmean, na.rm=TRUE)) / sd(f0IPUmean, na.rm=TRUE))%>%
  ungroup()

  #%>%
  # mutate(Condition = ifelse(Condition %in% c("Light", "Heavy"), "Biking", ifelse(Condition=="Sitting", "Sitting", "Baseline")),
  #        Condition = as.factor(Condition))

  # (I also tried conflating the two biking conditions, but this didn't change the result)

df$Condition <- relevel(df$Condition, ref="Sitting")

ggplot(df, aes(Condition, f0IPUz))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)

ggplot(df %>% filter(abs(f0IPUz) < 2), aes(Condition, f0IPUz))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)

plot(df$ConfGenderF, df$f0IPUz)

summary(m1 <- lmer(f0IPUz ~ Condition + (1 | Speaker), df))
summary(m2 <- lmer(f0IPUz ~ Condition + Order + (1 | Speaker), df))

anova(m1, m2) # order makes the model worse

summary(m2 <- lmer(f0IPUz ~ Condition + BMI + (1 | Speaker), df %>% filter(!is.na(BMI))))

anova(m1, m2) # BMI makes the model worse (to compare it with m1, fit m1 with %>% filter(!is.na(BMI)))

# summary(m4 <- lmer(f0IPUz ~ Condition * BMI + (1 | Speaker), df %>% filter(!is.na(BMI))))

# anova(m3, m4) # the interaction between condition and BMI increases AIC, so not good

summary(m2 <- lmer(f0IPUz ~ Condition + ConfFriendly + (1 | Speaker), df))

anova(m1, m2) # ConfFriendly makes the model worse

summary(m2 <- lmer(f0IPUz ~ Condition + InterEnjoy + (1 | Speaker), df))

anova(m1, m2) # InterEnjoy made the model worse

summary(m2a <- lmer(f0IPUz ~ Condition * InterEnjoy + (1 | Speaker), df))

anova(m2, m2a) # the interaction between Condition and InterEnjoy also made the model worse

summary(m2 <- lmer(f0IPUz ~ Condition + ConfGenderF + (1 | Speaker), df))

anova(m1, m2) # the more feminine they found the confederate, the lower f0 they produced, but the t value was only close to 2 (-1.842)
# but the AIC was only reduced by 1.5, do we consider it a significantly better model?

summary(m2a <- lmer(f0IPUz ~ Condition + ConfGenderM + (1 | Speaker), df))
# also, the more masculine they found the confederate, the higher their f0, but this effect is weaker

summary(m2b <- lmer(f0IPUz ~ Condition + ConfGenderF + ConfGenderM + (1 | Speaker), df))
# including both masculinity and femininity perceptions took away from their effect
# because ConfGenderF and ConfGenderM are correlated

cor.test(df$ConfGenderF[!duplicated(df$Speaker)], df$ConfGenderM[!duplicated(df$Speaker)])
# indeed, there's a -0.58 correlation, p = 0.004

# so let's just keep ConfGenderF in the model (its effect is stronger than that of ConfGenderM)

summary(m3<- lmer(f0IPUz ~ Condition * ConfGenderF + (1 | Speaker), df)) # the only interaction that was strong enough was of ConfGenderF with the baseline condition, which doesn't make sense because the participants hadn't seen the conf yet then

anova(m2, m3) # the Condition * ConfGenderF interaction improved the model in terms of AIC, BUT the only t value higher than 2 is of the intercaton between baseline and gender, which is meaningless, so let's not keep this interaction

summary(m3 <- lmer(f0IPUz ~ Condition + ConfGenderF + TMF.F + (1 | Speaker), df))

anova(m2, m3) # the participants' own gender identity didn't matter (I also tried the interaction with ConfGender and also TMF.M)

summary(m3 <- lmer(f0IPUz ~ Condition + GenderDiffF + (1 | Speaker), df))
# the gender difference between participant and their perception of the confederate's gender had a weaker effect than just the perception (ConfGenderF)

anova(m2, m3)
anova(m1, m3)

summary(m4 <- lmer(f0IPUz ~ Condition + ConfGenderF + (1 + Condition | Speaker), df))
summary(m4a <- lmer(f0IPUz ~ ConfGenderF + (1 + Condition | Speaker), df))
summary(m4b <- lmer(f0IPUz ~ ConfGenderF + (1 | Speaker), df))

anova(m4, m4a)
anova(m4a, m4b, refit=FALSE)
anova(m2, m4, refit=FALSE) # now comparing random effects, so refit=FALSE
# the random slope for condition per speaker makes it better

# the model without condition as a fixed effect is better
# but the one without condition as a fixed effect BUT with random slope for condition was better than without it

summary(m5 <- lmer(f0IPUz ~ Condition + ConfGenderF + breathCycleDur + (1 + Condition | Speaker), df %>% filter(!is.na(breathCycleDur))))

anova(m4, m5) # the duration of the breath cycle makes the model worse
# you have to fit both with df %>% filter(!is.na(breathCycleDur))


summary(m5 <- lmer(f0IPUz ~ Condition + ConfGenderF + breathRate + (1 + Condition | Speaker), df))
anova(m4, m5)
# breathRate improves the model!
# and it goes in the direction expected (higher breathing rate = higher f0)

summary(m5a <- lmer(f0IPUz ~ ConfGenderF + breathRate + (1 + Condition | Speaker), df))
anova(m5a, m5)
# the model without Condition is better

summary(m6 <- lmer(f0IPUz ~ ConfGenderF + breathRate + inhalDur + (1 + Condition | Speaker), df))
anova(m5a, m6) # also tried inhalAmp, neither is good

# maybe m5a is the best model

plot(fitted(m5a), residuals(m5a)) # is this alright?

qqnorm(resid(m5a));qqline(resid(m5a)) # it seems relatively normal I think?
hist(resid(m5a))


# articulation rate

ggplot(df, aes(Condition, speechRate))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)+
  ggtitle("Participants' speech rate")

ggplot(df, aes(Condition, speechRateIPU))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)+
  ggtitle("Participants' articulation rate")

plot(df$InterEnjoy, df$speechRateIPU)

plot(df$Order, df$speechRateIPU)

ggplot(fsm %>% filter(Role == "Confederate"), aes(Condition, speechRate))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)+
  ggtitle("Confederate's speech rate")

ggplot(fsm %>% filter(Role == "Confederate"), aes(Condition, speechRateIPU))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)+
  ggtitle("Confederate's articulation rate")

# the confederate's articulation rate (i.e. accounting for pauses) follows a pattern that makes much more sense than speech rate (not accounting for pauses)
# a regression shows that these differences are significant

summary(s1 <- lmer(speechRateIPU ~ Condition + (1 | Speaker), df))
# slight but significant difference between conditions! except no diff between sitting and light

summary(s2 <- lmer(speechRateIPU ~ Condition + BMI + (1 | Speaker),  df %>% filter(!is.na(BMI))))
# BMI not significant
anova(s1, s2) 

summary(s2 <- lmer(speechRateIPU ~ Condition + ConfFriendly + (1 | Speaker),  df))
anova(s1, s2)
# ConfFriendly is not significant and increases AIC

summary(s2 <- lmer(speechRateIPU ~ Condition + InterEnjoy + (1 | Speaker),  df))
anova(s1, s2)
# InterEnjoy reduced AIC by 3 and has a t value of -2.355

summary(s3 <- lmer(speechRateIPU ~ Condition * InterEnjoy + (1 | Speaker),  df))
anova(s2, s3)
# the Condition * InterEnjoy interaction makes the model worse

summary(s3 <- lmer(speechRateIPU ~ Condition + InterEnjoy + ConfGenderF + (1 | Speaker),  df))
anova(s2, s3)
# here ConfGenderF made the model a lot worse, and had a very low t value (same for ConfGenderM, and similar for participants' own TMF.F and .M values)

summary(s3 <- lmer(speechRateIPU ~ Condition + InterEnjoy + Order + (1 | Speaker),  df %>% filter(!is.na(breathCycleDur))))
anova(s2, s3)
# Order improved the model: the more time into the experiment, the faster people spoke

summary(s4 <- lmer(speechRateIPU ~ (Condition + Order)^2 + InterEnjoy + (1 | Speaker),  df))
anova(s3, s4)
# no interaction between condition, order and InterEnjoy (I also tested InterEnjoy in an interaction with the other ones)

summary(s4 <- lmer(speechRateIPU ~ Condition + InterEnjoy + Order + breathCycleDur + (1 | Speaker),  df %>% filter(!is.na(breathCycleDur))))
anova(s3, s4) # breathCycleDur is a good predictor: longer breath cycles = slower articulation rates
# (makes sense! if in general they have longer exhalations, the more time they give themselves to say the things they wanna say)
# so this would mean that if someone has more time (i.e. more breath) to speak, they won't try to fit in more information in that time, they'll just speak slower

summary(s5 <- lmer(speechRateIPU ~ Condition + InterEnjoy + Order *  breathCycleDur + (1 | Speaker),  df %>% filter(!is.na(breathCycleDur))))
anova(s4, s5) # I've tested the interaction between breathCycleDur and all the other predictors, and none of them was good

summary(s5 <- lmer(speechRateIPU ~ InterEnjoy + Order +  breathCycleDur + (1 | Speaker),  df %>% filter(!is.na(breathCycleDur))))
anova(s4, s5) # testing a model without Condition: it has slightly lower AIC , so we should keep the simpler model (without Condition)

summary(s6 <- lmer(speechRateIPU ~ InterEnjoy + Order +  breathCycleDur + (1 | Speaker) + (1 + Order | Speaker),  df %>% filter(!is.na(breathCycleDur))))
anova(s5, s6, refit=FALSE) # `(1 + Order | Speaker)` makes the model worse
# I also tried doing `(1 + Condition | Speaker)`, also made it worse

summary(s6 <- lmer(speechRateIPU ~ InterEnjoy + Order +  breathCycleDur + (1 + breathCycleDur | Speaker),  df %>% filter(!is.na(breathCycleDur))))
anova(s5, s6, refit=FALSE)
# if I include the random slope for breathCycleDur per speaker, AIC is reduced BUT the model has a singular fit. (and indeed, the correlation for the random effect of breathCycleDur is -1.00)
# so let's not include this random slope
# (I also tried with Condition instead of breathCycleDur)

summary(s6 <- lmer(speechRateIPU ~ InterEnjoy + Order +  breathCycleDur + inhalAmp + (1 | Speaker),  df %>% filter(!is.na(breathCycleDur))))
anova(s5, s6) # inhalDur and inhalAmp not good

# so we stick with s5?

hist(resid(s5))
qqnorm(resid(s5));qqline(resid(s5))
plot(fitted(s5), resid(s5))


### NOTES
# explanation of REFIT: lmer's default method for fitting models is REML. Also, REML is the appropriate method when you want to compare models with a difference in their random effects. When comparing models differing in fixed effects, though, ML is the appropriate method.

# from 18.1.22:
# - Use Speaker instead of BMI. Maybe BMI is a covariate and isn't a random effect.
# - Check Order by Speaker?
# - The difference being positive or negative depends on where the participant started (higher or lower than the confederate)
# - See if each speaker was higher or lower in baseline in comparison with Carry (just to know. Maybe there can be two groups for example)
# - Plot Carry's f0 in comparison with speakers in each condition

#### look at differences between participants' and confederate's speech

load(paste0(folder, "DataWithDifferences.RData"))

orderCondNoBase <- c("Sitting", "Light", "Heavy")

dat <- dat %>% filter(Condition != "Baseline") # there are no differences here

dat$f0diffGroup <- ifelse(dat$articRateDiff[dat$Order==3] < 0, "StartNegative", "StartPositive")
dat$ARdiffGroup <- ifelse(dat$articRateDiff[dat$Order==3] < 0, "StartNegative", "StartPositive")
dat$BRdiffGroup <- ifelse(dat$breathRateDiff[dat$Order==3] < 0, "StartNegative", "StartPositive")
dat$cyclediffGroup <- ifelse(dat$breathCycleDurDiff[dat$Order==3] < 0, "StartNegative", "StartPositive")

dat$Condition <- relevel(dat$Condition, ref="Sitting")

# f0 difference

ggplot(dat %>% filter(f0diffGroup == "StartNegative"), aes(Condition, f0rawDiff))+
  geom_point()+
  scale_x_discrete(limits = orderCondNoBase)

ggplot(dat, aes(Condition, abs(f0rawDiff)))+
  geom_point()+
  scale_x_discrete(limits = orderCondNoBase)

ggplot(dat %>% filter(f0diffGroup == "StartNegative"), aes(Order, f0rawDiff))+
  geom_point()

summary(fd1 <- lmer(f0rawDiff ~ Condition + (1 | Speaker), dat %>% filter(f0diffGroup == "StartNegative")))

summary(fd2 <- lmer(f0rawDiff ~ Condition + Order + (1 | Speaker), dat %>% filter(f0diffGroup == "StartNegative")))
anova(fd1, fd2) # order doesn't improve it: participants didn't get closer to the confederate throughout the experiment
# I've also tried adding a bunch of other predictors here, but none of them were significant or improved the model
# I've also tried adding a random slope for condition but the dataset doesn't have enough observations for that
# So condition is only good predictor -- not because people changed their speech much, but because the confederate had higher f0 with each condition, so the difference increased

hist(resid(fd1))
qqnorm(resid(fd1));qqline(resid(fd1))
plot(fitted(fd1), resid(fd1))
# weird residuals at around -100 (on the x axis) -- maybe because of the speaker DKG

# articulation rate difference

ggplot(dat %>% filter(ARdiffGroup == "StartNegative"), aes(Condition, articRateDiff))+
  geom_point()+
  scale_x_discrete(limits = orderCondNoBase)

ggplot(dat, aes(Condition, abs(articRateDiff)))+
  geom_point()+
  scale_x_discrete(limits = orderCondNoBase)

ggplot(dat %>% filter(ARdiffGroup == "StartNegative"), aes(Order, articRateDiff))+
  geom_point()


summary(ad1 <- lmer(articRateDiff ~ Condition + (1 | Speaker), dat %>% filter(ARdiffGroup == "StartNegative")))
summary(ad2 <- lmer(articRateDiff ~ Condition + InterEnjoy + (1 | Speaker), dat %>% filter(ARdiffGroup == "StartNegative")))
anova(ad1, ad2) # the more they enjoyed the interaction, the more negative (i.e. larger) the difference in articulation rate
# I tried a bunch of other predictors but none of them were good

summary(ad3 <- lmer(articRateDiff ~ Condition * InterEnjoy + (1 | Speaker), dat %>% filter(ARdiffGroup == "StartNegative")))
anova(ad2, ad3) # the interaction isn't good

hist(resid(ad2))
qqnorm(resid(ad2));qqline(resid(ad2))
plot(fitted(ad2), resid(ad2))

# breathing rate difference

ggplot(dat %>% filter(BRdiffGroup == "StartNegative"), aes(Condition, breathRateDiff))+
  geom_point()+
  scale_x_discrete(limits = orderCondNoBase)

ggplot(dat, aes(Condition, abs(breathRateDiff)))+
  geom_point()+
  scale_x_discrete(limits = orderCondNoBase)

ggplot(dat %>% filter(BRdiffGroup == "StartNegative"), aes(Order, breathRateDiff))+
  geom_point()

ggplot(dat %>% filter(BRdiffGroup == "StartPositive"), aes(Condition, breathRateDiff))+
  geom_point()+
  scale_x_discrete(limits = orderCondNoBase)

ggplot(dat %>% filter(BRdiffGroup == "StartPositive"), aes(Order, breathRateDiff))+
  geom_point()

summary(bd1n <- lmer(breathRateDiff ~ Condition + (1 | Speaker), dat %>% filter(BRdiffGroup == "StartNegative")))
summary(bd2n <- lmer(breathRateDiff ~ Condition + Order + (1 | Speaker), dat %>% filter(BRdiffGroup == "StartNegative")))
anova(bd1n, bd2n) # made the model worse: Order, ConfFriendly, InterEnjoy, ConfGenderF

hist(resid(bd1))
qqnorm(resid(bd1));qqline(resid(bd1))
plot(fitted(bd1), resid(bd1))

summary(bd1p <- lmer(breathRateDiff ~ Condition + (1 | Speaker), dat %>% filter(BRdiffGroup == "StartPositive")))

hist(resid(bd1))
qqnorm(resid(bd1));qqline(resid(bd1))
plot(fitted(bd1), resid(bd1))

# difference between strength of each condition's effect on StartPositive vs StartNegative groups
# caveat: there are only four speaker in the StartPositive group

# again, the difference in conditions is because the participants stayed mostly constant in their breathing while the confederate changed a lot

# breath cycle duration mean


ggplot(dat %>% filter(cyclediffGroup == "StartNegative"), aes(Condition, breathCycleDurDiff))+
  geom_point()+
  scale_x_discrete(limits = orderCondNoBase)

ggplot(dat, aes(Condition, abs(breathCycleDurDiff)))+
  geom_point()+
  scale_x_discrete(limits = orderCondNoBase)

ggplot(dat %>% filter(cyclediffGroup == "StartNegative"), aes(Order, breathCycleDurDiff))+
  geom_point()

ggplot(dat %>% filter(cyclediffGroup == "StartPositive"), aes(Condition, breathCycleDurDiff))+
  geom_point()+
  scale_x_discrete(limits = orderCondNoBase)

ggplot(dat %>% filter(cyclediffGroup == "StartPositive"), aes(Order, breathCycleDurDiff))+
  geom_point()

summary(cd1n <- lmer(breathCycleDurDiff ~ Condition + (1 | Speaker), dat %>% filter(cyclediffGroup == "StartNegative")))
summary(cd1p <- lmer(breathCycleDurDiff ~ Condition + (1 | Speaker), dat %>% filter(cyclediffGroup == "StartPositive")))

# I don't even know how to interpret this, I don't think this means anything to us

# Change in participants' f0 and breath rate

{dat <- fsm %>%
  filter(Role=="Participant", Task=="Free") %>%
  filter(!duplicated(file)) %>%
  group_by(file) %>%
  mutate(inhalAmpMean = mean(inhalAmp),
         inhalDurMean = mean(inhalDur)) %>%
  ungroup() %>%
  mutate(conditionChange = NA,
         f0Change = NA,
         breathRateChange = NA,
         speechRateChange = NA,
         inhalDurChange = NA,
         inhalAmpChange = NA)

dat$conditionChange[dat$Condition=="Sitting"] <- "BS"
dat$conditionChange[dat$Condition=="Light"] <- "SL"
dat$conditionChange[dat$Condition=="Heavy"] <- "LH"

for(i in dat$Speaker){
  dat$f0Change[dat$Speaker==i & dat$conditionChange=="BS"] <- dat$f0IPUmean[dat$Condition=="Sitting" & dat$Speaker==i] - dat$f0IPUmean[dat$Condition=="Baseline" & dat$Speaker==i]
  dat$f0Change[dat$Speaker==i & dat$conditionChange=="SL"] <- dat$f0IPUmean[dat$Condition=="Light" & dat$Speaker==i] - dat$f0IPUmean[dat$Condition=="Sitting" & dat$Speaker==i]
  dat$f0Change[dat$Speaker==i & dat$conditionChange=="LH"] <- dat$f0IPUmean[dat$Condition=="Heavy" & dat$Speaker==i] - dat$f0IPUmean[dat$Condition=="Light" & dat$Speaker==i]
  dat$breathRateChange[dat$Speaker==i & dat$conditionChange=="BS"] <- dat$breathRate[dat$Condition=="Sitting" & dat$Speaker==i] - dat$breathRate[dat$Condition=="Baseline" & dat$Speaker==i]
  dat$breathRateChange[dat$Speaker==i & dat$conditionChange=="SL"] <- dat$breathRate[dat$Condition=="Light" & dat$Speaker==i] - dat$breathRate[dat$Condition=="Sitting" & dat$Speaker==i]
  dat$breathRateChange[dat$Speaker==i & dat$conditionChange=="LH"] <- dat$breathRate[dat$Condition=="Heavy" & dat$Speaker==i] - dat$breathRate[dat$Condition=="Light" & dat$Speaker==i]
  dat$speechRateChange[dat$Speaker==i & dat$conditionChange=="BS"] <- dat$articRate[dat$Condition=="Sitting" & dat$Speaker==i] - dat$articRate[dat$Condition=="Baseline" & dat$Speaker==i]
  dat$speechRateChange[dat$Speaker==i & dat$conditionChange=="SL"] <- dat$articRate[dat$Condition=="Light" & dat$Speaker==i] - dat$articRate[dat$Condition=="Sitting" & dat$Speaker==i]
  dat$speechRateChange[dat$Speaker==i & dat$conditionChange=="LH"] <- dat$articRate[dat$Condition=="Heavy" & dat$Speaker==i] - dat$articRate[dat$Condition=="Light" & dat$Speaker==i]
  dat$inhalDurChange[dat$Speaker==i & dat$conditionChange=="BS"] <- dat$inhalDurMean[dat$Condition=="Sitting" & dat$Speaker==i] - dat$inhalDurMean[dat$Condition=="Baseline" & dat$Speaker==i]
  dat$inhalDurChange[dat$Speaker==i & dat$conditionChange=="SL"] <- dat$inhalDurMean[dat$Condition=="Light" & dat$Speaker==i] - dat$inhalDurMean[dat$Condition=="Sitting" & dat$Speaker==i]
  dat$inhalDurChange[dat$Speaker==i & dat$conditionChange=="LH"] <- dat$inhalDurMean[dat$Condition=="Heavy" & dat$Speaker==i] - dat$inhalDurMean[dat$Condition=="Light" & dat$Speaker==i]
  dat$inhalAmpChange[dat$Speaker==i & dat$conditionChange=="BS"] <- dat$inhalAmpMean[dat$Condition=="Sitting" & dat$Speaker==i] - dat$inhalAmpMean[dat$Condition=="Baseline" & dat$Speaker==i]
  dat$inhalAmpChange[dat$Speaker==i & dat$conditionChange=="SL"] <- dat$inhalAmpMean[dat$Condition=="Light" & dat$Speaker==i] - dat$inhalAmpMean[dat$Condition=="Sitting" & dat$Speaker==i]
  dat$inhalAmpChange[dat$Speaker==i & dat$conditionChange=="LH"] <- dat$inhalAmpMean[dat$Condition=="Heavy" & dat$Speaker==i] - dat$inhalAmpMean[dat$Condition=="Light" & dat$Speaker==i]
}

dat <- filter(dat, !is.na(conditionChange))

datPf <- dat %>% filter(f0Change >= 0)
datNf <- dat %>% filter(f0Change < 0)

datPb <- dat %>% filter(breathRateChange >= 0)
datNb <- dat %>% filter(breathRateChange < 0)

datPs <- dat %>% filter(speechRateChange >= 0)
datNs <- dat %>% filter(speechRateChange < 0)

datPd <- dat %>% filter(inhalDurChange >= 0)
datNd <- dat %>% filter(inhalDurChange < 0)

datPa <- dat %>% filter(inhalAmpChange >= 0)
datNa <- dat %>% filter(inhalAmpChange < 0)}

ggplot(dat, aes(conditionChange, f0Change))+
  geom_boxplot()+
  geom_point()+
  scale_x_discrete(limits = orderChange)

ggplot(datPf, aes(conditionChange, f0Change))+
  geom_boxplot()+
    geom_point()+
  scale_x_discrete(limits = orderChange)

ggplot(datNf, aes(conditionChange, f0Change))+
  geom_boxplot()+
  geom_point()+
  scale_x_discrete(limits = orderChange)

ggplot(dat, aes(conditionChange, breathRateChange))+
  geom_boxplot()+
  geom_point()+
  scale_x_discrete(limits = orderChange)

ggplot(datPb, aes(conditionChange, breathRateChange))+
  geom_boxplot()+
  geom_point()+
  scale_x_discrete(limits = orderChange)

ggplot(datNb, aes(conditionChange, breathRateChange))+
  geom_boxplot()+
  geom_point()+
  scale_x_discrete(limits = orderChange)

ggplot(dat, aes(conditionChange, speechRateChange))+
  geom_boxplot()+
  geom_point()+
  scale_x_discrete(limits = orderChange)

ggplot(datPs, aes(conditionChange, speechRateChange))+
  geom_boxplot()+
  geom_point()+
  scale_x_discrete(limits = orderChange)

ggplot(datNs, aes(conditionChange, speechRateChange))+
  geom_boxplot()+
  geom_point()+
  scale_x_discrete(limits = orderChange)

ggplot(dat, aes(conditionChange, inhalDurChange))+
  geom_boxplot()+
  geom_point()+
  scale_x_discrete(limits = orderChange)

ggplot(datPs, aes(conditionChange, inhalDurChange))+
  geom_boxplot()+
  geom_point()+
  scale_x_discrete(limits = orderChange)

ggplot(datNs, aes(conditionChange, inhalDurChange))+
  geom_boxplot()+
  geom_point()+
  scale_x_discrete(limits = orderChange)

ggplot(dat, aes(conditionChange, inhalAmpChange))+
  geom_boxplot()+
  geom_point()+
  scale_x_discrete(limits = orderChange)

ggplot(datPs, aes(conditionChange, inhalAmpChange))+
  geom_boxplot()+
  geom_point()+
  scale_x_discrete(limits = orderChange)

ggplot(datNs, aes(conditionChange, inhalAmpChange))+
  geom_boxplot()+
  geom_point()+
  scale_x_discrete(limits = orderChange)

ggplot(dat, aes(f0Change, breathRateChange))+
  geom_point()

ggplot(dat, aes(f0Change, speechRateChange))+
  geom_point()

summary(v1 <- lmer(f0Change ~ conditionChange + (1 | Speaker), dat))

summary(v2 <- lmer(f0Change ~ conditionChange + breathRateChange + (1 | Speaker), dat))

anova(v1, v2)

summary(v2a <- lmer(f0Change ~ breathRateChange + (1 | Speaker), dat))

anova(v2, v2a)
# v2a is better

summary(v3 <- lmer(f0Change ~ breathRateChange + ConfGenderF + (1 | Speaker), dat))
anova(v2a, v3) # didn't improve the model: InterEnjoy, ConfFriendly, ConfGenderF (although the latter was almost a good predictor and reduced AIC slightly)

summary(v3 <- lmer(f0Change ~ breathRateChange + speechRateChange + (1 | Speaker), dat))
anova(v2a, v3)

summary(v4 <- lmer(f0Change ~ breathRateChange + speechRateChange + inhalAmpChange + (1 | Speaker), dat))
anova(v3, v4) # inhalAmpChange and inhalDurChange not good

hist(resid(v3))
qqnorm(resid(v3));qqline(resid(v3))
plot(fitted(v3), resid(v3)) # it seems like this distribution isn't linear
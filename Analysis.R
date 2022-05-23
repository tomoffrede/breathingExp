library(lme4)
library(tidyverse)
library(influence.ME)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"

load(paste0(folder, "DataSpeech.RData"))

orderCond <- c("Baseline", "Sitting", "Light", "Heavy")

# ConfFriendly: how friendly the participant finds the confederate
# InterEnjoy: how much the participant enjoyed the interaction
# GenderDiff: difference between participants' gender identity and their perception of conf's gender expression
# GenderPercDiff: difference between confederate's gender identity and participants' perception of conf's gender expression
# ConfGender: participant's perception of confederate's gender

plot(fsm$BMI, fsm$f0mean)
#excluding the outlier:
fsm2 <- fsm[fsm$BMI < 26,]

cor.test(fsm2$BMI, fsm2$f0mean)

# apparently there's a significant weak correlation between BMI and f0, which is in line with Pisanski et al 2016.
# but this value is probably too high because there's multiple repeated BMI values (from the same person)
# so trying a regression controlling for participant:

lmer(f0IPUmean ~ BMI + (1 | Speaker), data=fsm %>% filter(!duplicated(file)))

fsm <- fsm %>%
  mutate(GenderDiffF = GEPAQ.F - ConfGenderF) %>%
  mutate(GenderDiffM = GEPAQ.M - ConfGenderM)

# it does seem to have a small effect

# > names(fsm)
# [1] "Speaker"       "file"          "IPU"           "f0mean"        "f0IPUmean"     "label"         "f0z"           "speechRateIPU"
# [9] "durSpeech"     "durPauses"     "speechRate"    "articRate"     "Condition"     "Task"          "List"          "Age"          
# [17] "Height"        "Weight"        "Gender"        "Education"     "OtherL1"       "ConfFriendly"  "InterEnjoy"    "ConfGenderM"  
# [25] "ConfGenderF"   "Order"         "Topic"         "BMI"           "GEPAQ.F"       "GEPAQ.M"       "TMF.F"         "TMF.M"        
# [33] "Role"          "GenderDiffF"   "GenderDiffM"  


# f0

df <- fsm %>%
  filter(Task == "Free", Role == "Participant") %>%
  mutate(f0IPUz = (f0mean - mean(f0mean, na.rm=TRUE)) / sd(f0mean, na.rm=TRUE),
         f0filez = (f0IPUmean - mean(f0IPUmean, na.rm=TRUE)) / sd(f0IPUmean, na.rm=TRUE))

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

anova(m2, m4, refit=FALSE) # now comparing random effects, so refit=FALSE
# the random slope for condition per speaker makes it better

# I fit a model only with ConfGenderF, without Condition, but the model was way worse!

summary(m5 <- lmer(f0IPUz ~ Condition + ConfGenderF + breathCycleDur + (1 + Condition | Speaker), df %>% filter(!is.na(breathCycleDur))))

anova(m4, m5) # the duration of the breath cycle makes the model worse
# you have to fit both with df %>% filter(!is.na(breathCycleDur))

# is m4 the final model?

plot(fitted(m4), residuals(m4)) # seems alright!

qqnorm(resid(m4));qqline(resid(m4)) # it seems relatively normal I think?
hist(resid(m4))




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
# I also tried doing `(1 + Condition | Speaker)`

### NOW: TRY DIFFERENT RANDOM SLOPES / RANDOM EFFECTS STRUCTURES
summary(s6 <- lmer(speechRateIPU ~ InterEnjoy + Order +  breathCycleDur + (1 + breathCycleDur | Speaker),  df %>% filter(!is.na(breathCycleDur))))

hist(resid(s6))
qqnorm(resid(s6));qqline(resid(s6))
plot(fitted(s6), resid(s6))

### NOTES
# explanation of REFIT: lmer's default method for fitting models is REML. Also, REML is the appropriate method when you want to compare models with a difference in their random effects. When comparing models differing in fixed effects, though, ML is the appropriate method.

# from 18.1.22:
# - Use Speaker instead of BMI. Maybe BMI is a covariate and isn't a random effect.
# - Check Order by Speaker?
# - The difference being positive or negative depends on where the participant started (higher or lower than the confederate)
# - See if each speaker was higher or lower in baseline in comparison with Carry (just to know. Maybe there can be two groups for example)
# - Plot Carry's f0 in comparison with speakers in each condition







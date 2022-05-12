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

lmer(f0mean ~ BMI + (1 | Speaker), data=fsm)

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
  filter(Task == "Free") %>%
  filter(Role == "Participant")

ggplot(df, aes(Condition, f0mean))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)
  

df$Condition <- relevel(df$Condition, ref="Sitting")

summary(m1 <- lmer(f0mean ~ Condition + (1 | Speaker), df))
summary(m2 <- lmer(f0mean ~ Condition + Order + (1 | Speaker), df))

anova(m1, m2) # order doesn't change the AIC enough

summary(m3 <- lmer(f0mean ~ Condition + BMI + (1 | Speaker), df %>% filter(!is.na(BMI))))

anova(m1, m3) # BMI doesn't change AIC much

# summary(m4 <- lmer(f0mean ~ Condition * BMI + (1 | Speaker), df %>% filter(!is.na(BMI))))

# anova(m3, m4) # the interaction between condition and BMI increases AIC, so not good

summary(m3 <- lmer(f0mean ~ Condition + ConfFriendly + (1 | Speaker), df))

anova(m1, m3) # ConfFriendly didn't change the AIC (and the t value was low in the model)

summary(m3 <- lmer(f0mean ~ Condition + InterEnjoy + (1 | Speaker), df))

anova(m1, m3) # InterEnjoy made the model worse

summary(m3 <- lmer(f0mean ~ Condition * InterEnjoy + (1 | Speaker), df))

anova(m1, m3) # the interaction between Condition and InterEnjoy also made the model worse

summary(m3 <- lmer(f0mean ~ Condition + ConfGenderF + (1 | Speaker), df))

anova(m1, m3) # the more feminine they found the confederate, the lower f0 they produced, but the t value was only close to 2 (-1.842)

summary(m3a <- lmer(f0mean ~ Condition + ConfGenderM + (1 | Speaker), df))
# also, the more masculine they found the confederate, the higher their f0, but this effect is weaker

summary(m3b <- lmer(f0mean ~ Condition + ConfGenderF + ConfGenderM + (1 | Speaker), df))
# including both masculinity and femininity perceptions took away from their effect
# probably because ConfGenderF and ConfGenderM are correlated

cor.test(df$ConfGenderF[!duplicated(df$Speaker)], df$ConfGenderM[!duplicated(df$Speaker)])
# indeed, there's a -0.58 correlation, p = 0.004

# so let's just keep ConfGenderF in the model (its effect is stronger than that of ConfGenderM)

summary(m4<- lmer(f0mean ~ Condition * ConfGenderF + (1 | Speaker), df)) # the only interaction that was strong enough was of ConfGenderF with the baseline condition, which doesn't make sense because the participants hadn't seen the conf yet then

anova(m3, m4) # the Condition * ConfGenderF interaction made the model worse

summary(m4 <- lmer(f0mean ~ Condition + ConfGenderF + TMF.F + (1 | Speaker), df))

anova(m3, m4) # the participants' own gender identity didn't matter (I also tried the interaction with ConfGender and also TMF.M)

summary(m4 <- lmer(f0mean ~ Condition + GenderDiffF + (1 | Speaker), df))
# the gender difference between participant and their perception of the confederate's gender had a weaker effect than just the perception (ConfGenderF)

anova(m1, m4)
anova(m1, m3)

summary(m4 <- lmer(f0mean ~ Condition + ConfGenderF + (1 + Condition | Speaker), df))

anova(m3, m4, refit=FALSE) # now comparing random effects, so refit=FALSE
# the random slope for condition per speaker makes it better

# I fit a model only with ConfGenderF, without Condition, but the model was way worse!

# is m4 the final model?

plot(fitted(m4), residuals(m4)) # seems alright!

qqnorm(resid(m4));qqline(resid(m4)) # it seems relatively normal I think?



# speech rate?

ggplot(df, aes(Condition, speechRate))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)+
  ggtitle("Participants' speech rate")

ggplot(df, aes(Condition, articRate))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)+
  ggtitle("Participants' articulation rate")

ggplot(fsm %>% filter(Role == "Confederate"), aes(Condition, speechRate))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)+
  ggtitle("Confederate's speech rate")

ggplot(fsm %>% filter(Role == "Confederate"), aes(Condition, articRate))+
  geom_boxplot()+
  scale_x_discrete(limits = orderCond)+
  ggtitle("Confederate's articulation rate")

# the confederate's articulation rate (i.e. accounting for pauses) follows a pattern that makes much more sense than speech rate (not accounting for pauses)
# a regression shows that these differences are significant

summary(s1 <- lmer(articRate ~ Condition + (1 | Speaker), df))
# slight but significant difference between conditions! except no diff between sitting and light

summary(s2 <- lmer(articRate ~ Condition + BMI + (1 | Speaker),  df %>% filter(!is.na(BMI))))
# BMI not significant
anova(s1, s2) 

summary(s2 <- lmer(articRate ~ Condition + ConfFriendly + (1 | Speaker),  df))
anova(s1, s2)
# ConfFriendly is not significant and increases AIC

summary(s2 <- lmer(articRate ~ Condition + InterEnjoy + (1 | Speaker),  df))
anova(s1, s2)
# InterEnjoy reduced AIC by 3.6 and has a t value of -2.404

summary(s3 <- lmer(articRate ~ Condition * InterEnjoy + (1 | Speaker),  df))
anova(s2, s3)
# the Condition * InterEnjoy interaction improved the model and has a bunch of high t values

summary(s4 <- lmer(articRate ~ Condition * InterEnjoy + ConfGenderF + (1 | Speaker),  df))
anova(s3, s4)
# here ConfGenderF made the model a lot worse, and had a very low t value (same for ConfGenderM, and similar for participants' own TMF.F and .M values)

summary(s4 <- lmer(articRate ~ Condition * InterEnjoy + Order + (1 | Speaker),  df))
anova(s3, s4)
# Order was very significant and improved the model: the more time into the experiment, the faster people spoke

summary(s5 <- lmer(articRate ~ (Condition + InterEnjoy + Order)^3 + (1 | Speaker),  df))
anova(s4, s5)
# the three-way interaction between Condition, InterEnjoy and Order improved the model a lot and gave lots of significant interactions.
# basically:
# the main effect of Order became negative
# the longer into the experiment, the higher articulation rate for the heavy condition, but not for the light condition
# the more the participants enjoyed the interaction, the higher the articulation rate got throughout the experiment

df$articRateC <- scale(df$articRate, center=TRUE, scale=TRUE)

summary(s6 <- lmer(articRateC ~ (Condition + InterEnjoy + Order)^3 + (1 + Condition | Speaker),  df))
# this model shows an output but it gives convergence warnings and says the model is nearly unidentifiable;
# I've tried rescaling articRate, and I've also done ` (0 + Condition | Speaker) + (1 | Speaker` instead of `(1 + Condition | Speaker)` (as per a suggestion here: https://stats.stackexchange.com/questions/242109/model-failed-to-converge-warning-in-lmer)
# depending on what I try, the warnings sometimes change, but they still appear


s

### NOTES
# explanation of REFIT: lmer's default method for fitting models is REML. Also, REML is the appropriate method when you want to compare models with a difference in their random effects. When comparing models differing in fixed effects, though, ML is the appropriate method.

# look at variability throughout experiment / conditions?

# when checking f0mean (not diff), try Cf0means a predictor

# from 18.1.22:
# - Use Speaker instead of BMI. Maybe BMI is a covariate and isn't a random effect.
# - Check Order by Speaker?
# - The difference being positive or negative depends on where the participant started (higher or lower than the confederate)
# - See if each speaker was higher or lower in baseline in comparison with Carry (just to know. Maybe there can be two groups for example)
# - Plot Carry's f0 in comparison with speakers in each condition






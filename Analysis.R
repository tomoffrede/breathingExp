library(lme4)
library(ggplot2)
# library(influence.ME)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"

load(paste0(folder, "DataWithDiff.RData"))

### Analyses

# dependent variables: f0meanDiff, f0medianDiff, speechRateDiff, BreathCycles
# main fixed effects: Condition, Order
# other fixed effects to check: ConfFriendly, InterEnjoy, TMF.F, TMF.M, GenderDiff.TMFF, GenderDiff.TMFM, BMI
# random effects to check: Speaker
# do it on separate tasks

# ConfFriendly: how friendly the participant finds the confederate
# InterEnjoy: how much the participant enjoyed the interaction
# GenderDiff: difference between participants' gender identity and their perception of conf's gender expression
# GenderPercDiff: difference between confederate's gender identity and participants' perception of conf's gender expression
# ConfGender: participant's perception of confederate's gender

plot(dat$BMI, dat$f0mean)
#excluding the outlier:
dat2 <- dat[dat$BMI < 26,]

cor.test(dat2$BMI, dat2$f0mean)

# apparently there's a significant weak correlation between BMI and f0, which is in line with Pisanski et al 2016.
# but this value is probably too high because there's multiple repeated BMI values (from the same person)
# so trying a regression controlling for participant:

lmer(f0mean ~ BMI + (1 | Speaker), data=dat)

# it does seem to have an effect


### FREE SPEECH

df <- dat[dat$Task == "Free",]

## f0 mean difference

summary(m1 <- lmer(f0meanDiff ~ Condition + (1 | Speaker), data = df))
summary(m1a <- lmer(f0meanDiff ~ CbreathRateSpeech + (1 | Speaker), data = df))

summary(m2 <- lmer(f0meanDiff ~ Condition + Order + (1 | Speaker), data = df))

anova(m1, m2) # including Order doesn't improve the model

summary(m2 <- lmer(f0meanDiff ~ Condition + BMI + (1 | Speaker), data = df, subset=complete.cases(df)))

anova(m1, m2) # including BMI reduced AIC by 1. usually (?) the threshold for considering the more complex model better is 2. so let's keep m1.
# also, by keeping m1, we keep all the participants, including the one that doesn't have BMI info

summary(m2 <- lmer(f0meanDiff ~ Condition + ConfFriendly + (1 | Speaker), data = df))

anova(m1, m2) # including ConfFriendly increased AIC by 2

summary(m2 <- lmer(f0meanDiff ~ Condition + InterEnjoy + (1 | Speaker), data = df))

anova(m1, m2) # same for InterEnjoy

summary(m2 <- lmer(f0meanDiff ~ Condition + TMF.F + (1 | Speaker), data = df)) # TMF.F seems to be almost significant

anova(m1, m2) # including TMF.F reduced AIC by a bit over 1. so let's keep m1

summary(m2 <- lmer(f0meanDiff ~ Condition + TMF.M + (1 | Speaker), data = df))

anova(m1, m2) # TMF.M increased AIC by 1.5

summary(m2 <- lmer(f0meanDiff ~ Condition + GenderDiff.TMFF + (1 | Speaker), data = df))

anova(m1, m2) # including GenderDiff.TMFF increased AIC slightly

summary(m2 <- lmer(f0meanDiff ~ Condition + ConfGenderF + (1 | Speaker), data = df))

anova(m1, m2) # including ConfGenderF increased AIC slightly

summary(m2b <- lmer(f0meanDiff ~ Condition + ConfGenderF + (1 | Speaker), data = df))
summary(m2 <- lmer(f0meanDiff ~ Condition + TMF.F * ConfGenderF + (1 | Speaker), data = df))

anova(m2b, m2) # adding an interaction between TMF.F and ConfGenderF also doesn't help

summary(m2 <- lmer(f0meanDiff ~ Condition + GEPAQ.F + (1 | Speaker), data = df))

anova(m1, m2) # including GEPAQ.F increased AIC by 2

### NEXT: PLOT RAW DATA; SEE NOTES BELOW


### NOTES
# maybe try to add a random (average) BMI number to the person whose BMI is NA? so we have more datapoints
# explanation of REFIT: lmer's default method for fitting models is REML. Also, REML is the appropriate method when you want to compare models with a difference in their random effects. When comparing models differing in fixed effects, though, ML is the appropriate method.

# i guess it doesn't make sense to include Speaker as a random factor, since there's already gender

# look at variability throughout experiment / conditions?

# when checking f0mean (not diff), try Cf0means a predictor

# from 18.1.22:
# - Use Speaker instead of BMI. Maybe BMI is a covariate and isn't a random effect.
# - Check if correlation between BMI and f0 (probably not!).
# - Check Order by Speaker?
#   - Heavy biking had the strongest effect on f0 - good (expected)
# - See if participants' f0 changes (not only f0Diff) - also speech rate
# - The difference being positive or negative depends on where the participant started (higher or lower than the confederate)
# - See if each speaker was higher or lower in baseline in comparison with Carry (just to know. Maybe there can be two groups for example)
# - Plot Carry's f0 in comparison with speakers in each condition
# - Do each task (free speech, reading) separately!
#   - Leave out Topic (we already saw that it was zero in the model, so we can assume it's controlled for).



## outliers
## variability is smaller after baseline > people do something
library(tidyverse)
library(viridis)
library(cowplot)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folder1 <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Plots/new/"
folder2 <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Plots/FreeSpeech/new/"
folder3 <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Plots/FreeSpeech/Differences/new/"

order <- c("Baseline", "Sitting", "Light", "Heavy")
orderconf <- order[-1]

load(paste0(folder, "DataNoDiff.RData"))

#########################
# if you want to plot the participants' data across the conf's breathing rate,
# you want to use this dataset, and then, make sure that you filter only the participants
# and then only the confederate for the visualization:

# load(paste0(folder, "DataNoDiff.RData"))
# 
# conf <- dat[dat$Speaker=="Confederate", c(3, 4, 5, 7:13, 26, 28:31, 33)]
# colnames(conf) <- sub("^","C", colnames(conf))
# dat2 <-dat
# dat3 <- dat2[rep(seq_len(nrow(dat2)), each = nrow(conf)),]
# rownames(dat3) <- 1:nrow(dat3)
# conf2 <- do.call("rbind", replicate((nrow(dat3)/nrow(conf)), conf, simplify=FALSE))
# 
# dat4 <- cbind(dat3, conf2)
# 
# dat4 <- dat4[(dat4$Condition == dat4$CCondition & dat4$Task == dat4$CTask) | (dat4$Condition == "Baseline"),]
# dat4 <- dat4[!duplicated(dat4$file),]
# 
# dat <- dat4

#########################

##### ACROSS CONDITIONS

## f0

dat$Order <- as.integer(dat$Order)

df <- dat[dat$Task=="Free",]

df <- df[df$f0mean>125,]

png(paste0(folder2, "ConditionsFreef0.png"), width=700, height=500)
ggplot(df) +
  geom_point(aes(x = Condition, y = f0mean, color = Role), size=4) +
  geom_point(data = subset(df, Role == 'Confederate'),
             aes(x = Condition, y = f0mean, color = Role), size=4)+
  scale_color_viridis_d("Speaker", end=.65)+
  ggtitle("f0 mean (free speech) - no outliers ")+
  scale_x_discrete(limits = order)+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=16),
        axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))
dev.off()


#########################
# if you want to plot the participants' data across the conf's breathing rate,
# you want to use this dataset, and then, make sure that you filter only the participants
# and then only the confederate for the visualization:

load(paste0(folder, "DataNoDiff.RData"))

conf <- dat[dat$Speaker=="Confederate", c(3, 4, 5, 7:13, 26, 28:31, 33)]
colnames(conf) <- sub("^","C", colnames(conf))
dat2 <-dat
dat3 <- dat2[rep(seq_len(nrow(dat2)), each = nrow(conf)),]
rownames(dat3) <- 1:nrow(dat3)
conf2 <- do.call("rbind", replicate((nrow(dat3)/nrow(conf)), conf, simplify=FALSE))

dat4 <- cbind(dat3, conf2)

dat4 <- dat4[(dat4$Condition == dat4$CCondition & dat4$Task == dat4$CTask) | (dat4$Condition == "Baseline"),]
dat4 <- dat4[!duplicated(dat4$file),]

dat <- dat4

#########################

df <- dat %>%
  filter(Task=="Free") %>%
  filter(f0mean > 125)


# png(paste0(folder2, "ConditionsFreef0.png"), width=700, height=500)
ggplot() +
  geom_point(df %>% filter(Role=="Participant"),
    mapping=aes(x = CbreathRateSpeech, y = f0mean, color = Role), size=4) +
  geom_point(df %>% filter(Role=="Confederate"),
             mapping=aes(x = CbreathRateSpeech, y = f0mean, color = Role), size=4)+
  scale_color_viridis_d("Speaker", direction=-1, end=.65)+
  ggtitle("f0 mean (free speech) - no outliers ")+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=16),
        axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))
# dev.off

#########################


#### Confederate's

df <- dat[dat$Task=="Free",]
df <- df[df$Condition!="Baseline",]
df <- df[df$Speaker=="Confederate",]

df <- droplevels(df)

png(paste0(folder2, "ConditionsFreePeaksConf.png"), width=700, height=500)
ggplot() +
  geom_point(df, mapping=aes(x = Condition, y = f0mean), size=4, color="#482173FF")+
  scale_color_viridis_d()+
  ggtitle("f0 - Conf - (free speech)")+
  scale_x_discrete(limits = orderconf)+
  # facet_wrap(~Topic)+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=16),
        axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))
dev.off()

###

## speech rate

df <- dat[dat$Task=="Free",] # do it again so when removing the speechRate outlines, we include the f0 outliers we removed before

png(paste0(folder2, "ConditionsFreeSR.png"), width=700, height=500)
ggplot() +
  geom_point(subset(df, Role=="Participant"), mapping=aes(x = Condition, y = speechRate, color = Role), size=4) +
  geom_point(data = subset(df, Role == 'Confederate'),
             aes(x = Condition, y = speechRate, color = Role), size=4)+
  scale_color_viridis_d("Speaker", end=.65)+
  ggtitle("Speech rate (free speech)")+
  scale_x_discrete(limits = order)+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=16),
        axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))
dev.off()

## articulation rate


df <- dat[dat$Task=="Free",] # do it again so when removing the speechRate outlines, we include the f0 outliers we removed before

png(paste0(folder2, "ConditionsFreeAR.png"), width=700, height=500)
ggplot() +
  geom_point(subset(df, Role=="Participant"), mapping=aes(x = Condition, y = articRate, color = Role), size=4) +
  geom_point(data = subset(df, Role == 'Confederate'),
             aes(x = Condition, y = articRate, color = Role), size=4)+
  scale_color_viridis_d("Speaker", end=.65)+
  ggtitle("Articulation rate (free speech)")+
  scale_x_discrete(limits = order)+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=16),
        axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))
dev.off()



### boxplots

## f0 mean

df <- dat[dat$Task=="Free",] %>%
  filter(f0mean > 125)


png(paste0(folder2, "ConditionsFreef0AllTopicsboxplots.png"), width=700, height=500)
ggplot()+
  geom_boxplot(df %>% filter(Role=="Participant"), mapping=aes(Condition, f0mean), fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  geom_point(df %>% filter(Role=="Confederate"), mapping=aes(x=Condition, y=f0mean), color="#7AD151FF", size=5)+
  scale_colour_manual(values=c("Participants"="#2D708EFF", "Confederate"="#7AD151FF"))+
  ggtitle("f0 mean (free speech) - no outliers")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18), title=element_text(size=23))
dev.off()

png(paste0(folder2, "ConditionsFreef0PerTopicboxplots.png"), width=700, height=500)
ggplot()+
  geom_boxplot(df %>% filter(Role=="Participant"), mapping=aes(Condition, f0mean), fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  geom_point(df %>% filter(Role=="Confederate"), mapping=aes(x=Condition, y=f0mean), color="#7AD151FF", size=5)+
  scale_colour_manual(values=c("Participants"="#2D708EFF", "Confederate"="#7AD151FF"))+
  ggtitle("f0 mean (free speech) - no outliers")+
  facet_wrap(~Topic)+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18), title=element_text(size=23))
dev.off()

## speech rate

df <- dat[dat$Task=="Free",]

png(paste0(folder2, "ConditionsFreeSRAllTopicsboxplots.png"), width=700, height=500)
ggplot(df)+
  geom_boxplot(aes(Condition, speechRate), fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  geom_point(df %>% filter(Role=="Confederate"), mapping=aes(x=Condition, y=speechRate), color="#7AD151FF", size=5)+
  scale_colour_manual(values=c("Participants"="#2D708EFF", "Confederate"="#7AD151FF"))+
  ggtitle("Speech rate (free speech)")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18), title=element_text(size=23))
dev.off()

png(paste0(folder2, "ConditionsFreeSRPerTopicboxplots.png"), width=700, height=500)
ggplot()+
  geom_boxplot(df %>% filter(Role=="Participant"), mapping=aes(Condition, speechRate), fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  geom_point(df %>% filter(Role=="Confederate"), mapping=aes(x=Condition, y=speechRate), color="#7AD151FF", size=5)+
  scale_colour_manual(values=c("Participants"="#2D708EFF", "Confederate"="#7AD151FF"))+
  ggtitle("Speech rate (free speech) - no outliers")+
  facet_wrap(~Topic)+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18), title=element_text(size=23))
dev.off()

## articulation rate

df <- dat[dat$Task=="Free",]

png(paste0(folder2, "ConditionsFreeARAllTopicsboxplots.png"), width=700, height=500)
ggplot(df)+
  geom_boxplot(aes(Condition, articRate), fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  geom_point(df %>% filter(Role=="Confederate"), mapping=aes(x=Condition, y=articRate), color="#7AD151FF", size=5)+
  scale_colour_manual(values=c("Participants"="#2D708EFF", "Confederate"="#7AD151FF"))+
  ggtitle("Articulation rate (free speech)")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18), title=element_text(size=23))
dev.off()

png(paste0(folder2, "ConditionsFreeARPerTopicboxplots.png"), width=700, height=500)
ggplot()+
  geom_boxplot(df %>% filter(Role=="Participant"), mapping=aes(Condition, articRate), fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  geom_point(df %>% filter(Role=="Confederate"), mapping=aes(x=Condition, y=articRate), color="#7AD151FF", size=5)+
  scale_colour_manual(values=c("Participants"="#2D708EFF", "Confederate"="#7AD151FF"))+
  ggtitle("Articulation rate (free speech) - no outliers")+
  facet_wrap(~Topic)+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18), title=element_text(size=23))
dev.off()

## breath rate

#### Confederate's

df <- dat[dat$Task=="Free",]

png(paste0(folder2, "ConditionsFreePeaksConf.png"), width=700, height=500)
ggplot() +
  geom_point(df %>% filter(Speaker=="Confederate"), mapping=aes(x = Condition, y = breathRateSpeech), size=4)+
  scale_color_viridis_d()+
  ggtitle("Breath rate - Conf - (free speech)")+
  scale_x_discrete(limits = order)+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=16),
        axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))
dev.off()

png(paste0(folder2, "ConditionsFreePeaksConfPerTopic.png"), width=700, height=500)
ggplot() +
  geom_point(df %>% filter(Speaker=="Confederate"), mapping=aes(x = Condition, y = breathRateSpeech), size=4)+
  scale_color_viridis_d()+
  ggtitle("Breath rate - Conf - (free speech)")+
  scale_x_discrete(limits = order)+
  facet_wrap(~Topic)+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=16),
        axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))
dev.off()

###

df <- dat[dat$Task=="Free",]

png(paste0(folder2, "ConditionsFreePeaks.png"), width=700, height=500)
ggplot() +
  geom_boxplot(subset(df, Role=="Participant"), mapping=aes(x = Condition, y = breathRateSpeech))+
  scale_color_viridis_d()+
  ggtitle("Breath rate - DATA MISSING - (free speech)")+
  scale_x_discrete(limits = order)+
  # facet_wrap(~Topic)+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=16),
        axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))
dev.off()


#### individual f0s across conditions:

df <- dat[dat$Task=="Free",]

# turn Confederate's values into their mean
for(c in df$Condition){
  df$f0mean[df$Speaker=="Confederate" & df$Condition==c] <- mean(df$f0mean[df$Speaker=="Confederate" & df$Condition==c])
}

df <- df %>% distinct(f0mean, Speaker, Condition, .keep_all=TRUE)

participants <- unique(df$Speaker[df$Speaker!="Confederate"])

plots <- list()
abc <- c("ATN", "BND", "CBE")
`%!in%` <- Negate(`%in%`)

for(i in participants){
  if(i %in% abc){
    duo <- c(i, "Confederate")
    dp <- df[df$Speaker %in% duo,]
    p <- ggplot(data=dp, aes(x=Condition, y=f0mean, group=Speaker))+
      geom_line(aes(color=Speaker), size=3)+
      geom_point(aes(color=Speaker), size=7)+
      theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
            axis.title=element_text(size=28), axis.text=element_text(size=26))+
      scale_x_discrete(limits = order)+
      scale_color_viridis_d(end=.65, direction=-1) # have to change the direction of the color scale for three participants because their names come alphabetically before "Confederate", so they were getting the opposite colors as the other participants
    plots[[i]] <- p
  } else if (i %!in% abc){
    duo <- c(i, "Confederate")
    dp <- df[df$Speaker %in% duo,]
    p <- ggplot(data=dp, aes(x=Condition, y=f0mean, group=Speaker))+
      geom_line(aes(color=Speaker), size=3)+
      geom_point(aes(color=Speaker), size=7)+
      theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
            axis.title=element_text(size=28), axis.text=element_text(size=26))+
      scale_x_discrete(limits = order)+
      scale_color_viridis_d(end=.65, direction=1)
    plots[[i]] <- p
  }
}


plotgrid <- plot_grid(plotlist=plots)

title <- ggdraw() + 
  draw_label("Individual f0 (Free Speech)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "ConditionsIndividualFreef0s.png"), width=3000, height=2000)
plot_grid(title, plotgrid, ncol=1, rel_heights = c(0.1, 1))
dev.off()

####

#### individual speech rates across conditions:

df <- dat[dat$Task=="Free",]

# turn Confederate's values into their mean
for(c in df$Condition){
  df$speechRate[df$Speaker=="Confederate" & df$Condition==c] <- mean(df$speechRate[df$Speaker=="Confederate" & df$Condition==c])
}

df <- df %>% distinct(speechRate, Speaker, Condition, .keep_all=TRUE)

participants <- unique(df$Speaker[df$Speaker!="Confederate"])

plots <- list()
abc <- c("ATN", "BND", "CBE")
`%!in%` <- Negate(`%in%`)

for(i in participants){
  if(i %in% abc){
    duo <- c(i, "Confederate")
    dp <- df[df$Speaker %in% duo,]
    p <- ggplot(data=dp, aes(x=Condition, y=speechRate, group=Speaker))+
      geom_line(aes(color=Speaker), size=3)+
      geom_point(aes(color=Speaker), size=7)+
      theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
            axis.title=element_text(size=28), axis.text=element_text(size=26))+
      scale_x_discrete(limits = order)+
      scale_color_viridis_d(end=.65, direction=-1) # have to change the direction of the color scale for three participants because their names come alphabetically before "Confederate", so they were getting the opposite colors as the other participants
    plots[[i]] <- p
  } else if (i %!in% abc){
    duo <- c(i, "Confederate")
    dp <- df[df$Speaker %in% duo,]
    p <- ggplot(data=dp, aes(x=Condition, y=speechRate, group=Speaker))+
      geom_line(aes(color=Speaker), size=3)+
      geom_point(aes(color=Speaker), size=7)+
      theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
            axis.title=element_text(size=28), axis.text=element_text(size=26))+
      scale_x_discrete(limits = order)+
      scale_color_viridis_d(end=.65, direction=1)
    plots[[i]] <- p
  }
}

plotgrid <- plot_grid(plotlist=plots)

title <- ggdraw() + 
  draw_label("Individual Speech Rates (Free Speech)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "ConditionsIndividualFreeSRs.png"), width=3000, height=2000)
plot_grid(title, plotgrid, ncol=1, rel_heights = c(0.1, 1))
dev.off()

####

#### individual articulation rates across conditions:

df <- dat[dat$Task=="Free",]

# turn Confederate's values into their mean
for(c in df$Condition){
  df$articRate[df$Speaker=="Confederate" & df$Condition==c] <- mean(df$articRate[df$Speaker=="Confederate" & df$Condition==c])
}

df <- df %>% distinct(articRate, Speaker, Condition, .keep_all=TRUE)

participants <- unique(df$Speaker[df$Speaker!="Confederate"])

plots <- list()
abc <- c("ATN", "BND", "CBE")
`%!in%` <- Negate(`%in%`)

for(i in participants){
  if(i %in% abc){
    duo <- c(i, "Confederate")
    dp <- df[df$Speaker %in% duo,]
    p <- ggplot(data=dp, aes(x=Condition, y=articRate, group=Speaker))+
      geom_line(aes(color=Speaker), size=3)+
      geom_point(aes(color=Speaker), size=7)+
      theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
            axis.title=element_text(size=28), axis.text=element_text(size=26))+
      scale_x_discrete(limits = order)+
      scale_color_viridis_d(end=.65, direction=-1) # have to change the direction of the color scale for three participants because their names come alphabetically before "Confederate", so they were getting the opposite colors as the other participants
    plots[[i]] <- p
  } else if (i %!in% abc){
    duo <- c(i, "Confederate")
    dp <- df[df$Speaker %in% duo,]
    p <- ggplot(data=dp, aes(x=Condition, y=articRate, group=Speaker))+
      geom_line(aes(color=Speaker), size=3)+
      geom_point(aes(color=Speaker), size=7)+
      theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
            axis.title=element_text(size=28), axis.text=element_text(size=26))+
      scale_x_discrete(limits = order)+
      scale_color_viridis_d(end=.65, direction=1)
    plots[[i]] <- p
  }
}

plotgrid <- plot_grid(plotlist=plots)

title <- ggdraw() + 
  draw_label("Individual Articulation Rates (Free Speech)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "ConditionsIndividualFreeARs.png"), width=3000, height=2000)
plot_grid(title, plotgrid, ncol=1, rel_heights = c(0.1, 1))
dev.off()

####

#### individual breathing rates across conditions:

df <- dat[dat$Task=="Free",]

# turn Confederate's values into their mean
for(c in df$Condition){
  df$breathRateSpeech[df$Speaker=="Confederate" & df$Condition==c] <- mean(df$breathRateSpeech[df$Speaker=="Confederate" & df$Condition==c])
}

df <- df %>% distinct(breathRateSpeech, Speaker, Condition, .keep_all=TRUE)

participants <- unique(df$Speaker[df$Speaker!="Confederate"])

plots <- list()
abc <- c("ATN", "BND", "CBE")
`%!in%` <- Negate(`%in%`)

for(i in participants){
  if(i %in% abc){
    duo <- c(i, "Confederate")
    dp <- df[df$Speaker %in% duo,]
    p <- ggplot(data=dp, aes(x=Condition, y=breathRateSpeech, group=Speaker))+
      geom_line(aes(color=Speaker), size=3)+
      geom_point(aes(color=Speaker), size=7)+
      theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
            axis.title=element_text(size=28), axis.text=element_text(size=26))+
      scale_x_discrete(limits = order)+
      scale_color_viridis_d(end=.65, direction=-1) # have to change the direction of the color scale for three participants because their names come alphabetically before "Confederate", so they were getting the opposite colors as the other participants
    plots[[i]] <- p
  } else if (i %!in% abc){
    duo <- c(i, "Confederate")
    dp <- df[df$Speaker %in% duo,]
    p <- ggplot(data=dp, aes(x=Condition, y=breathRateSpeech, group=Speaker))+
      geom_line(aes(color=Speaker), size=3)+
      geom_point(aes(color=Speaker), size=7)+
      theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
            axis.title=element_text(size=28), axis.text=element_text(size=26))+
      scale_x_discrete(limits = order)+
      scale_color_viridis_d(end=.65, direction=1)
    plots[[i]] <- p
  }
}


plotgrid <- plot_grid(plotlist=plots)

title <- ggdraw() + 
  draw_label("Individual breathing rates (Free Speech)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "ConditionsIndividualFreeBRs.png"), width=3000, height=2000)
plot_grid(title, plotgrid, ncol=1, rel_heights = c(0.1, 1))
dev.off()


#####

##### ACROSS ORDER

load(paste0(folder, "DataWithDiff.RData"))

df <- dat[dat$Task=="Free",]

# create data frame with confederate's order for each participant

alld <- list()

for(i in df$Speaker){
  df1 <- df[df$Speaker==i, c(1, 3, 5, 6, 20, 28, 30, 31)]
  df2 <- df1[rep(seq_len(nrow(df1)), each=2),]
  rownames(df2) <- 1:nrow(df2)
  df2[c(2, 4, 6, 8),]$Speaker <- paste0("Confederate-",i)
  df2$f0mean[df2$Speaker==paste0("Confederate-",i)] <- df2$Cf0mean[df2$Speaker==paste0("Confederate-",i)]
  df2$speechRate[df2$Speaker==paste0("Confederate-",i)] <- df2$CspeechRate[df2$Speaker==paste0("Confederate-",i)]
  df2$articRate[df2$Speaker==paste0("Confederate-",i)] <- df2$CarticRate[df2$Speaker==paste0("Confederate-",i)]
  df3 <- df2[!(df2$Order==0 & df2$Speaker==paste0("Confederate-",i)),]
  df3$Cf0mean <- NULL
  df3$CspeechRate <- NULL
  df3$CarticRate <- NULL
  alld[[i]] <- df3
}

dfo <- bind_rows(alld)

dfo$Role[substr(dfo$Speaker, 1, 6) == "Confed"] <- "Confederate"
dfo$Role[substr(dfo$Speaker, 1, 6) != "Confed"] <- "Participant"

dfo$Order <- as.integer(dfo$Order)

# individual f0s across order:

participants <- unique(dfo$Speaker[substr(dfo$Speaker, 1, 6)!="Confed"])

plots <- list()
abc <- c("ATN", "BND", "CBE")
`%!in%` <- Negate(`%in%`)

for(i in participants){
  for(c in df$Order){
    if(i %in% abc){
      duo <- c(i, paste0("Confederate-", i))
      dp <- dfo[dfo$Speaker %in% duo,]
      plots[[i]] <- ggplot(data=dp, aes(x=Order, y=f0mean, group=Role))+
        geom_line(aes(color=Speaker), size=3)+
        geom_point(aes(color=Speaker), size=7)+
        theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
              axis.title=element_text(size=28), axis.text=element_text(size=26))+
        scale_color_viridis_d(end=.65, direction=-1)
    } else if(i %!in% abc){
      duo <- c(i, paste0("Confederate-", i))
      dp <- dfo[dfo$Speaker %in% duo,]
      plots[[i]] <- ggplot(data=dp, aes(x=Order, y=f0mean, group=Role))+
        geom_line(aes(color=Speaker), size=3)+
        geom_point(aes(color=Speaker), size=7)+
        theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
              axis.title=element_text(size=28), axis.text=element_text(size=26))+
        scale_color_viridis_d(end=.65)
    }
  }
}

plotgrid <- plot_grid(plotlist=plots)

title <- ggdraw() + 
  draw_label("Individual f0 (Free Speech)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "OrderIndividualFreef0s.png"), width=3000, height=2000)
plot_grid(title, plotgrid, ncol=1, rel_heights = c(0.1, 1))
dev.off()

####

#### individual speech rates across order

participants <- unique(dfo$Speaker[substr(dfo$Speaker, 1, 6)!="Confed"])

plots <- list()
abc <- c("ATN", "BND", "CBE")
`%!in%` <- Negate(`%in%`)


for(i in participants){
  for(c in df$Order){
    if(i %in% abc){
      duo <- c(i, paste0("Confederate-", i))
      dp <- dfo[dfo$Speaker %in% duo,]
      p <- ggplot(data=dp, aes(x=Order, y=speechRate, group=Role))+
        geom_line(aes(color=Speaker), size=3)+
        geom_point(aes(color=Speaker), size=7)+
        theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
              axis.title=element_text(size=28), axis.text=element_text(size=26))+
        scale_color_viridis_d(end=.65, direction=-1)
      plots[[i]] <- p
    } else if(i %!in% abc){
      duo <- c(i, paste0("Confederate-", i))
      dp <- dfo[dfo$Speaker %in% duo,]
      p <- ggplot(data=dp, aes(x=Order, y=speechRate, group=Role))+
        geom_line(aes(color=Speaker), size=3)+
        geom_point(aes(color=Speaker), size=7)+
        theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
              axis.title=element_text(size=28), axis.text=element_text(size=26))+
        scale_color_viridis_d(end=.65)
      plots[[i]] <- p
    }
  }
}

plotgrid <- plot_grid(plotlist=plots)

title <- ggdraw() + 
  draw_label("Individual speech rates (Free Speech)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "OrderIndividualFreeSRs.png"), width=3000, height=2000)
plot_grid(title, plotgrid, ncol=1, rel_heights = c(0.1, 1))
dev.off()

####

#### individual articulation rates across order

participants <- unique(dfo$Speaker[substr(dfo$Speaker, 1, 6)!="Confed"])

plots <- list()
abc <- c("ATN", "BND", "CBE")
`%!in%` <- Negate(`%in%`)


for(i in participants){
  for(c in df$Order){
    if(i %in% abc){
      duo <- c(i, paste0("Confederate-", i))
      dp <- dfo[dfo$Speaker %in% duo,]
      p <- ggplot(data=dp, aes(x=Order, y=articRate, group=Role))+
        geom_line(aes(color=Speaker), size=3)+
        geom_point(aes(color=Speaker), size=7)+
        theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
              axis.title=element_text(size=28), axis.text=element_text(size=26))+
        scale_color_viridis_d(end=.65, direction=-1)
      plots[[i]] <- p
    } else if(i %!in% abc){
      duo <- c(i, paste0("Confederate-", i))
      dp <- dfo[dfo$Speaker %in% duo,]
      p <- ggplot(data=dp, aes(x=Order, y=articRate, group=Role))+
        geom_line(aes(color=Speaker), size=3)+
        geom_point(aes(color=Speaker), size=7)+
        theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
              axis.title=element_text(size=28), axis.text=element_text(size=26))+
        scale_color_viridis_d(end=.65)
      plots[[i]] <- p
    }
  }
}

plotgrid <- plot_grid(plotlist=plots)

title <- ggdraw() + 
  draw_label("Individual articulation rates (Free Speech)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "OrderIndividualFreeARs.png"), width=3000, height=2000)
plot_grid(title, plotgrid, ncol=1, rel_heights = c(0.1, 1))
dev.off()

####


##### ACROSS TASKS

# f0

load(paste0(folder, "DataNoDiff.RData"))
datn <- dat[dat$f0mean > 125,]

png(paste0(folder1, "Tasksf0boxplots.png"), width=700, height=500)
ggplot()+
  geom_boxplot(subset(datn, Role=="Participant"), mapping=aes(Task, f0mean), fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  geom_point(subset(datn, Role=="Confederate"), mapping=aes(Task, f0mean), color="#7AD151FF", size=5)+
  scale_colour_manual(values=c("Participants"="#2D708EFF", "Confederate"="#7AD151FF"))+
  ggtitle("f0 (free speech) - no outliers")+
  facet_wrap(~Condition)+
  theme(axis.title=element_text(size=20), axis.text.y=element_text(size=18), axis.text.x=element_text(size=12),
        title=element_text(size=23),
        strip.text=element_text(size=18)) # size of condition name (of facet_wrap)
dev.off()


# speech rate

load(paste0(folder, "DataNoDiff.RData"))

png(paste0(folder1, "TasksSRboxplots.png"), width=700, height=500)
ggplot()+
  geom_boxplot(subset(dat, Role=="Participant"), mapping=aes(Task, speechRate), fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  geom_point(subset(dat, Role=="Confederate"), mapping=aes(Task, speechRate), color="#7AD151FF", size=5)+
  scale_colour_manual(values=c("Participants"="#2D708EFF", "Confederate"="#7AD151FF"))+
  ggtitle("Speech rate (free speech)")+
  facet_wrap(~Condition)+
  theme(axis.title=element_text(size=20), axis.text.y=element_text(size=18), axis.text.x=element_text(size=12),
        title=element_text(size=23),
        strip.text=element_text(size=18)) # size of condition name (of facet_wrap)
dev.off()


# articulation rate

load(paste0(folder, "DataNoDiff.RData"))

png(paste0(folder1, "TasksARboxplots.png"), width=700, height=500)
ggplot()+
  geom_boxplot(subset(dat, Role=="Participant"), mapping=aes(Task, articRate), fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  geom_point(subset(dat, Role=="Confederate"), mapping=aes(Task, articRate), color="#7AD151FF", size=5)+
  scale_colour_manual(values=c("Participants"="#2D708EFF", "Confederate"="#7AD151FF"))+
  ggtitle("Articulation rate (free speech)")+
  facet_wrap(~Condition)+
  theme(axis.title=element_text(size=20), axis.text.y=element_text(size=18), axis.text.x=element_text(size=12),
        title=element_text(size=23),
        strip.text=element_text(size=18)) # size of condition name (of facet_wrap)
dev.off()

####

########### PLOTTING THE DIFFERENCE BETWEEN PARTICIPANTS' AND CONFEDERATE'S DATA

#### ACROSS CONDITIONS

## f0

load(paste0(folder, "DataWithDiff.RData"))
df <- dat[dat$Task=="Free" & dat$Condition!="Baseline",]

png(paste0(folder3, "Conditionsf0DiffFreeboxplots.png"), width=700, height=500)
ggplot(df, aes(Condition, f0meanDiff))+
  geom_boxplot(fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  ggtitle("f0 difference (participant - confederate) (free speech)")+
  scale_x_discrete(limits = orderconf)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder3, "ConditionsIndividualf0DiffFreeboxplots.png"), width=1500, height=1200)
ggplot(df, aes(Condition, f0meanDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("f0 difference (participant - confederate) (free speech)")+
  facet_wrap(~Speaker)+
  scale_x_discrete(limits = orderconf)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()


#############################

# png(paste0(folder3, "ConditionsIndividualf0DiffFreeboxplots.png"), width=1500, height=1200)
ggplot(df, aes(CbreathRateSpeech, f0meanDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("f0 difference (participant - confederate) (free speech)")+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
# dev.off()


#############################


## speech rate

load(paste0(folder, "DataWithDiff.RData"))
df <- dat[dat$Task=="Free" & dat$Condition!="Baseline",]

png(paste0(folder3, "ConditionsSRDiffFreeboxplots.png"), width=700, height=500)
ggplot(df, aes(Condition, speechRateDiff))+
  geom_boxplot(fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  ggtitle("Speech rate difference (participant - confederate) (free speech)")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder3, "ConditionsIndividualSRDiffFreeboxplots.png"), width=1500, height=1200)
ggplot(df, aes(Condition, speechRateDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("Speech rate difference (participant - confederate) (free speech)")+
  scale_x_discrete(limits = order)+
  facet_wrap(~Speaker)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()


## articulation rate

load(paste0(folder, "DataWithDiff.RData"))
df <- dat[dat$Task=="Free" & dat$Condition!="Baseline",]

png(paste0(folder3, "ConditionsARDiffFreeboxplots.png"), width=700, height=500)
ggplot(df, aes(Condition, articRateDiff))+
  geom_boxplot(fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  ggtitle("Articulation rate difference (participant - confederate) (free speech)")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder3, "ConditionsIndividualARDiffFreeboxplots.png"), width=1500, height=1200)
ggplot(df, aes(Condition, articRateDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("Articulation rate difference (participant - confederate) (free speech)")+
  scale_x_discrete(limits = order)+
  facet_wrap(~Speaker)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()



#### ACROSS ORDER

## f0

load(paste0(folder, "DataWithDiff.RData"))
df <- dat[dat$Task=="Free" & dat$Condition!="Baseline",]
df$Order <- as.factor(df$Order)

png(paste0(folder3, "Orderf0DiffFreeboxplots.png"), width=700, height=500)
ggplot(df, aes(Order, f0meanDiff))+
  geom_boxplot(fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  ggtitle("f0 difference (participant - confederate) (free speech)")+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder3, "OrderIndividualf0DiffFreeboxplots.png"), width=1500, height=1200)
ggplot(df, aes(Order, f0meanDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("f0 difference (participant - confederate) (free speech)")+
  facet_wrap(~Speaker)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

## speech rate

load(paste0(folder, "DataWithDiff.RData"))
df <- dat[dat$Task=="Free" & dat$Condition!="Baseline",]
df$Order <- as.factor(df$Order)

png(paste0(folder3, "OrderSRDiffFreeboxplots.png"), width=700, height=500)
ggplot(df, aes(Order, speechRateDiff))+
  geom_boxplot(fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  ggtitle("Speech rate difference (participant - confederate) (free speech)")+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder3, "OrderIndividualSRDiffFreeboxplots.png"), width=1500, height=1200)
ggplot(df, aes(Order, speechRateDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("Speech rate difference (participant - confederate) (free speech)")+
  facet_wrap(~Speaker)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()


## articulation rate

load(paste0(folder, "DataWithDiff.RData"))
df <- dat[dat$Task=="Free" & dat$Condition!="Baseline",]
df$Order <- as.factor(df$Order)

png(paste0(folder3, "OrderARDiffFreeboxplots.png"), width=700, height=500)
ggplot(df, aes(Order, articRateDiff))+
  geom_boxplot(fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  ggtitle("Articulation rate difference (participant - confederate) (free speech)")+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder3, "OrderIndividualARDiffFreeboxplots.png"), width=1500, height=1200)
ggplot(df, aes(Order, articRateDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("Articulation rate difference (participant - confederate) (free speech)")+
  facet_wrap(~Speaker)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()


#### ACROSS TASKS

## f0

load(paste0(folder, "DataWithDiff.RData"))
df <- dat[dat$Condition!="Baseline",]

png(paste0(folder1, "Tasksf0Diffboxplots.png"), width=700, height=500)
ggplot(df, aes(Task, f0meanDiff))+
  geom_boxplot(fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  ggtitle("f0 difference (participant - confederate)")+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder1, "TasksIndividualf0Diff.png"), width=1500, height=1200)
ggplot(df, aes(Task, f0meanDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("f0 difference (participant - confederate)")+
  facet_wrap(~Speaker)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder1, "TasksConditionsf0Diffboxplots.png"), width=1500, height=1200)
ggplot(df, aes(Condition, f0meanDiff))+
  geom_boxplot(fill="#2D708EFF")+
  ggtitle("f0 difference (participant - confederate)")+
  facet_wrap(~Task)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23),
        strip.text=element_text(size=20))
dev.off()

## speech rate

load(paste0(folder, "DataWithDiff.RData"))
df <- dat[dat$Condition!="Baseline",]

png(paste0(folder1, "TasksSRDiffboxplots.png"), width=700, height=500)
ggplot(df, aes(Task, speechRateDiff))+
  geom_boxplot(fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  ggtitle("Speech rate difference (participant - confederate)")+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder1, "TasksIndividualSRDiff.png"), width=1500, height=1200)
ggplot(df, aes(Task, speechRateDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("Speech rate difference (participant - confederate)")+
  facet_wrap(~Speaker)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder1, "TasksConditionsSRDiffboxplots.png"), width=1500, height=1200)
ggplot(df, aes(Condition, speechRateDiff))+
  geom_boxplot(fill="#2D708EFF")+
  ggtitle("Speech rate difference (participant - confederate)")+
  facet_wrap(~Task)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23),
        strip.text=element_text(size=20))
dev.off()


## articulation rate

load(paste0(folder, "DataWithDiff.RData"))
df <- dat[dat$Condition!="Baseline",]

png(paste0(folder1, "TasksARDiffboxplots.png"), width=700, height=500)
ggplot(df, aes(Task, articRateDiff))+
  geom_boxplot(fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  ggtitle("Articulation rate difference (participant - confederate)")+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder1, "TasksIndividualARDiff.png"), width=1500, height=1200)
ggplot(df, aes(Task, articRateDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("Articulation rate difference (participant - confederate)")+
  facet_wrap(~Speaker)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder1, "TasksConditionsARDiffboxplots.png"), width=1500, height=1200)
ggplot(df, aes(Condition, articRateDiff))+
  geom_boxplot(fill="#2D708EFF")+
  ggtitle("Articulation rate difference (participant - confederate)")+
  facet_wrap(~Task)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23),
        strip.text=element_text(size=20))
dev.off()


# colors?
  #7AD151FF"
  #2D708EFF
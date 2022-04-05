library(tidyverse)
library(viridis)
library(cowplot)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folder1 <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis//Plots/"
folder2 <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Plots/ReadAlone/"
folder3 <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Plots/ReadAlone/Differences/"

order <- c("Baseline", "Sitting", "Light", "Heavy")

load(paste0(folder, "DataNoDiff.RData"))

##### ACROSS CONDITIONS

## f0

df <- dat[dat$Task == "ReadAlone",]

df <- df[df$f0mean>125,]

png(paste0(folder2, "ConditionsAlonef0.png"), width=700, height=500)
ggplot(df) +
  geom_point(aes(x = Condition, y = f0mean, color = Role), size=4) +
  geom_point(data = subset(df, Role == 'Confederate'),
             aes(x = Condition, y = f0mean, color = Role), size=4)+
  scale_color_viridis_d("Speaker", end=.65)+
  ggtitle("f0 mean (read alone) - no outliers ")+
  scale_x_discrete(limits = order)+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=16),
        axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))
dev.off()

## speech rate

df <- dat[dat$Task=="ReadAlone",] # do it again so when removing the speechRate outlines, we include the f0 outliers we removed before

png(paste0(folder2, "ConditionsAloneSR.png"), width=700, height=500)
ggplot() +
  geom_point(subset(df, Role=="Participant"), mapping=aes(x = Condition, y = speechRate, color = Role), size=4) +
  geom_point(data = subset(df, Role == 'Confederate'),
             aes(x = Condition, y = speechRate, color = Role), size=4)+
  scale_color_viridis_d("Speaker", end=.65)+
  ggtitle("Speech rate (read alone)")+
  scale_x_discrete(limits = order)+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=16),
        axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))
dev.off()

## articulation rate


df <- dat[dat$Task=="ReadAlone",] # do it again so when removing the speechRate outlines, we include the f0 outliers we removed before

png(paste0(folder2, "ConditionsAloneAR.png"), width=700, height=500)
ggplot() +
  geom_point(subset(df, Role=="Participant"), mapping=aes(x = Condition, y = articRate, color = Role), size=4) +
  geom_point(data = subset(df, Role == 'Confederate'),
             aes(x = Condition, y = articRate, color = Role), size=4)+
  scale_color_viridis_d("Speaker", end=.65)+
  ggtitle("Articulation rate (read alone)")+
  scale_x_discrete(limits = order)+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=16),
        axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))
dev.off()

### boxplots

## f0 mean

df <- dat[dat$Task=="ReadAlone",]

png(paste0(folder2, "ConditionsAlonef0AllTopicsboxplots.png"), width=700, height=500)
ggplot()+
  geom_boxplot(subset(df, Role=="Participant"), mapping=aes(Condition, f0mean), fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  geom_point(subset(df, Role=="Confederate"), mapping=aes(x=Condition, y=f0mean), color="#7AD151FF", size=5)+
  scale_colour_manual(values=c("Participants"="#2D708EFF", "Confederate"="#7AD151FF"))+
  ggtitle("f0 mean (read alone) - no outliers")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18), title=element_text(size=23))
dev.off()

png(paste0(folder2, "ConditionsAlonef0PerTopicboxplots.png"), width=700, height=500)
ggplot()+
  geom_boxplot(df %>% filter(Role=="Participant"), mapping=aes(Condition, f0mean), fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  geom_point(subset(df, Role=="Confederate"), mapping=aes(x=Condition, y=f0mean), color="#7AD151FF", size=5)+
  scale_colour_manual(values=c("Participants"="#2D708EFF", "Confederate"="#7AD151FF"))+
  ggtitle("f0 mean (read alone) - no outliers")+
  facet_wrap(~Topic)+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18), title=element_text(size=23))
dev.off()

## speech rate

df <- df[!(df$Role=="Participant" & df$f0mean > 260),]

png(paste0(folder2, "ConditionsAloneSRAllTopicsboxplots.png"), width=700, height=500)
ggplot(df)+
  geom_boxplot(aes(Condition, speechRate), fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  geom_point(subset(df, Role=="Confederate"), mapping=aes(x=Condition, y=speechRate), color="#7AD151FF", size=5)+
  scale_colour_manual(values=c("Participants"="#2D708EFF", "Confederate"="#7AD151FF"))+
  ggtitle("Speech rate (read alone)")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18), title=element_text(size=23))
dev.off()

png(paste0(folder2, "ConditionsAloneSRPerTopicboxplots.png"), width=700, height=500)
ggplot()+
  geom_boxplot(df %>% filter(Role=="Participant"), mapping=aes(Condition, speechRate), fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  geom_point(subset(df, Role=="Confederate"), mapping=aes(x=Condition, y=speechRate), color="#7AD151FF", size=5)+
  scale_colour_manual(values=c("Participants"="#2D708EFF", "Confederate"="#7AD151FF"))+
  ggtitle("Speech rate (read alone) - no outliers")+
  facet_wrap(~Topic)+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18), title=element_text(size=23))
dev.off()

## articulation rate

df <- df[!(df$Role=="Participant" & df$f0mean > 260),]

png(paste0(folder2, "ConditionsAloneARAllTopicsboxplots.png"), width=700, height=500)
ggplot(df)+
  geom_boxplot(aes(Condition, articRate), fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  geom_point(subset(df, Role=="Confederate"), mapping=aes(x=Condition, y=articRate), color="#7AD151FF", size=5)+
  scale_colour_manual(values=c("Participants"="#2D708EFF", "Confederate"="#7AD151FF"))+
  ggtitle("Articulation rate (read alone)")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18), title=element_text(size=23))
dev.off()

png(paste0(folder2, "ConditionsAloneARPerTopicboxplots.png"), width=700, height=500)
ggplot()+
  geom_boxplot(df %>% filter(Role=="Participant"), mapping=aes(Condition, articRate), fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  geom_point(subset(df, Role=="Confederate"), mapping=aes(x=Condition, y=articRate), color="#7AD151FF", size=5)+
  scale_colour_manual(values=c("Participants"="#2D708EFF", "Confederate"="#7AD151FF"))+
  ggtitle("Articulation rate (read alone) - no outliers")+
  facet_wrap(~Topic)+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18), title=element_text(size=23))
dev.off()

##

df <- dat[dat$Task=="ReadAlone",]

png(paste0(folder2, "ConditionsAlonePeaks.png"), width=700, height=500)
ggplot() +
  geom_boxplot(subset(df, Role=="Participant"), mapping=aes(x = Condition, y = breathRateSpeech))+
  scale_color_viridis_d()+
  ggtitle("Breath rate - DATA MISSING - (read alone)")+
  scale_x_discrete(limits = order)+
  # facet_wrap(~Topic)+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=16),
        axis.title=element_text(size=21), axis.text=element_text(size=20),
        plot.title=element_text(size=25))
dev.off()


#### individual f0s across conditions:

df <- dat[dat$Task=="ReadAlone",]

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
      scale_x_discrete(limits = order)+
      theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
            axis.title=element_text(size=28), axis.text=element_text(size=26))+
      scale_color_viridis_d(end=.65, direction=-1) # have to change the direction of the color scale for three participants because their names come alphabetically before "Confederate", so they were getting the opposite colors as the other participants
    plots[[i]] <- p
  } else if (i %!in% abc){
    duo <- c(i, "Confederate")
    dp <- df[df$Speaker %in% duo,]
    p <- ggplot(data=dp, aes(x=Condition, y=f0mean, group=Speaker))+
      geom_line(aes(color=Speaker), size=3)+
      geom_point(aes(color=Speaker), size=7)+
      scale_x_discrete(limits = order)+
      theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
            axis.title=element_text(size=28), axis.text=element_text(size=26))+
      scale_color_viridis_d(end=.65, direction=1)
    plots[[i]] <- p
  }
}


plotgrid <- plot_grid(plotlist=plots)

title <- ggdraw() + 
  draw_label("Individual f0 (Read Alone)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "ConditionsIndividualAlonef0s.png"), width=3000, height=2000)
plot_grid(title, plotgrid, ncol=1, rel_heights = c(0.1, 1))
dev.off()

####

#### individual speech rates across conditions:

df <- dat[dat$Task=="ReadAlone",]

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
      scale_x_discrete(limits = order)+
      theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
            axis.title=element_text(size=28), axis.text=element_text(size=26))+
      scale_color_viridis_d(end=.65, direction=-1) # have to change the direction of the color scale for three participants because their names come alphabetically before "Confederate", so they were getting the opposite colors as the other participants
    plots[[i]] <- p
  } else if (i %!in% abc){
    duo <- c(i, "Confederate")
    dp <- df[df$Speaker %in% duo,]
    p <- ggplot(data=dp, aes(x=Condition, y=speechRate, group=Speaker))+
      geom_line(aes(color=Speaker), size=3)+
      scale_x_discrete(limits = order)+
      geom_point(aes(color=Speaker), size=7)+
      theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
            axis.title=element_text(size=28), axis.text=element_text(size=26))+
      scale_color_viridis_d(end=.65, direction=1)
    plots[[i]] <- p
  }
}

plotgrid <- plot_grid(plotlist=plots)

title <- ggdraw() + 
  draw_label("Individual Speech Rates (Read Alone)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "ConditionsIndividualAloneSRs.png"), width=3000, height=2000)
plot_grid(title, plotgrid, ncol=1, rel_heights = c(0.1, 1))
dev.off()

####

#### individual articulation rates across conditions:

df <- dat[dat$Task=="ReadAlone",]

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
      scale_x_discrete(limits = order)+
      theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
            axis.title=element_text(size=28), axis.text=element_text(size=26))+
      scale_color_viridis_d(end=.65, direction=-1) # have to change the direction of the color scale for three participants because their names come alphabetically before "Confederate", so they were getting the opposite colors as the other participants
    plots[[i]] <- p
  } else if (i %!in% abc){
    duo <- c(i, "Confederate")
    dp <- df[df$Speaker %in% duo,]
    p <- ggplot(data=dp, aes(x=Condition, y=articRate, group=Speaker))+
      geom_line(aes(color=Speaker), size=3)+
      geom_point(aes(color=Speaker), size=7)+
      scale_x_discrete(limits = order)+
      theme(legend.title=element_text(size=26), legend.text=element_text(size=24),
            axis.title=element_text(size=28), axis.text=element_text(size=26))+
      scale_color_viridis_d(end=.65, direction=1)
    plots[[i]] <- p
  }
}

plotgrid <- plot_grid(plotlist=plots)

title <- ggdraw() + 
  draw_label("Individual Articulation Rates (Read Alone)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "ConditionsIndividualAloneARs.png"), width=3000, height=2000)
plot_grid(title, plotgrid, ncol=1, rel_heights = c(0.1, 1))
dev.off()

####



#### individual breathing rates across conditions:

df <- dat[dat$Task=="ReadAlone",]

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
  draw_label("Individual breathing rates (Read Alone)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "ConditionsIndividualAloneBRs.png"), width=3000, height=2000)
plot_grid(title, plotgrid, ncol=1, rel_heights = c(0.1, 1))
dev.off()


##### ACROSS ORDER

load(paste0(folder, "DataWithDiff.RData"))

df <- dat[dat$Task=="ReadAlone",]

# create data frame with confederate's order for each participant

alld <- list()
i="TSE"
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
  draw_label("Individual f0 (Read Alone)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "OrderIndividualAlonef0s.png"), width=3000, height=2000)
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
  draw_label("Individual speech rates (Read Alone)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "OrderIndividualAloneSRs.png"), width=3000, height=2000)
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
  draw_label("Individual articulation rates (Read Alone)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "OrderIndividualAloneARs.png"), width=3000, height=2000)
plot_grid(title, plotgrid, ncol=1, rel_heights = c(0.1, 1))
dev.off()

####


########### PLOTTING THE DIFFERENCE BETWEEN PARTICIPANTS' AND CONFEDERATE'S DATA

#### ACROSS CONDITIONS

## f0

load(paste0(folder, "DataWithDiff.RData"))
df <- dat[dat$Task=="ReadAlone" & dat$Condition!="Baseline",]

png(paste0(folder3, "Conditionsf0DiffAloneboxplots.png"), width=700, height=500)
ggplot(df, aes(Condition, f0meanDiff))+
  geom_boxplot(fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  ggtitle("f0 difference (participant - confederate) (read alone)")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder3, "ConditionsIndividualf0DiffAloneboxplots.png"), width=1500, height=1200)
ggplot(df, aes(Condition, f0meanDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("f0 difference (participant - confederate) (read alone)")+
  facet_wrap(~Speaker)+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

## speech rate

load(paste0(folder, "DataWithDiff.RData"))
df <- dat[dat$Task=="ReadAlone" & dat$Condition!="Baseline",]

png(paste0(folder3, "ConditionsSRDiffFreeboxplots.png"), width=700, height=500)
ggplot(df, aes(Condition, speechRateDiff))+
  geom_boxplot(fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  ggtitle("Speech rate difference (participant - confederate) (read alone)")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder3, "ConditionsIndividualSRDiffAloneboxplots.png"), width=1500, height=1200)
ggplot(df, aes(Condition, speechRateDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("Speech rate difference (participant - confederate) (read alone)")+
  facet_wrap(~Speaker)+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()


## articulation rate

load(paste0(folder, "DataWithDiff.RData"))
df <- dat[dat$Task=="ReadAlone" & dat$Condition!="Baseline",]

png(paste0(folder3, "ConditionsARDiffAloneboxplots.png"), width=700, height=500)
ggplot(df, aes(Condition, articRateDiff))+
  geom_boxplot(fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  ggtitle("Articulation rate difference (participant - confederate) (read alone)")+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder3, "ConditionsIndividualARDiffAloneboxplots.png"), width=1500, height=1200)
ggplot(df, aes(Condition, articRateDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("Articulation rate difference (participant - confederate) (read alone)")+
  facet_wrap(~Speaker)+
  scale_x_discrete(limits = order)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()



#### ACROSS ORDER

## f0

load(paste0(folder, "DataWithDiff.RData"))
df <- dat[dat$Task=="ReadAlone" & dat$Condition!="Baseline",]
df$Order <- as.factor(df$Order)

png(paste0(folder3, "Orderf0DiffAloneboxplots.png"), width=700, height=500)
ggplot(df, aes(Order, f0meanDiff))+
  geom_boxplot(fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  ggtitle("f0 difference (participant - confederate) (read alone)")+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder3, "OrderIndividualf0DiffAloneboxplots.png"), width=1500, height=1200)
ggplot(df, aes(Order, f0meanDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("f0 difference (participant - confederate) (read alone)")+
  facet_wrap(~Speaker)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

## speech rate

load(paste0(folder, "DataWithDiff.RData"))
df <- dat[dat$Task=="ReadAlone" & dat$Condition!="Baseline",]
df$Order <- as.factor(df$Order)

png(paste0(folder3, "OrderSRDiffAloneboxplots.png"), width=700, height=500)
ggplot(df, aes(Order, speechRateDiff))+
  geom_boxplot(fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  ggtitle("Speech rate difference (participant - confederate) (read alone)")+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder3, "OrderIndividualSRDiffAloneboxplots.png"), width=1500, height=1200)
ggplot(df, aes(Order, speechRateDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("Speech rate difference (participant - confederate) (read alone)")+
  facet_wrap(~Speaker)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()


## articulation rate

load(paste0(folder, "DataWithDiff.RData"))
df <- dat[dat$Task=="ReadAlone" & dat$Condition!="Baseline",]
df$Order <- as.factor(df$Order)

png(paste0(folder3, "OrderARDiffAloneboxplots.png"), width=700, height=500)
ggplot(df, aes(Order, articRateDiff))+
  geom_boxplot(fill="#2D708EFF")+ # I got this color code from a viridis palette with scales::show_col(viridis_pal(option="D)(12))
  ggtitle("Articulation rate difference (participant - confederate) (read alone)")+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()

png(paste0(folder3, "OrderIndividualARDiffAloneboxplots.png"), width=1500, height=1200)
ggplot(df, aes(Order, articRateDiff))+
  geom_point(color="#2D708EFF", size=4)+
  ggtitle("Articulation rate difference (participant - confederate) (read alone)")+
  facet_wrap(~Speaker)+
  theme(axis.title=element_text(size=20), axis.text=element_text(size=18),
        title=element_text(size=23))
dev.off()


# colors?
#7AD151FF"
#2D708EFF
# Tom Offrede
# Plot each individual participant's and confederate's f0 in one plot per condition, to compare

# most of the code taken from PlotsFree.R (which is a very messy script)

library(tidyverse)
library(viridis)
library(cowplot)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/Plots/Feb2023Update/"

order <- c("Baseline", "Sitting", "Light", "Heavy")

load(paste0(folder, "DataSpeech.RData"))
load(paste0(folder, "DataReadSpeech.RData"))

# Free speech

df <- fsm %>% 
  group_by(Speaker, Condition) %>% 
  summarize(f0mean = mean(f0raw, na.rm=TRUE)) %>% 
  ungroup()

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

png(paste0(folder2, "FreeSpeech-F0.png"), width=3000, height=2000)
plot_grid(title, plotgrid, ncol=1, rel_heights = c(0.1, 1))
dev.off()

################################################
################################################
################################################

# Solo read speech

df <- frb %>% 
  filter(Task!="ReadJoint") %>% 
  group_by(Speaker, Condition) %>% 
  summarize(f0mean = mean(f0raw, na.rm=TRUE)) %>% 
  ungroup()

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
  draw_label("Individual f0 (Solo Read Speech)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "SoloReadSpeech-F0.png"), width=3000, height=2000)
plot_grid(title, plotgrid, ncol=1, rel_heights = c(0.1, 1))
dev.off()


################################################
################################################
################################################

# Solo read speech

df <- frb %>% 
  filter(Task!="ReadAlone") %>% 
  group_by(Speaker, Condition) %>% 
  summarize(f0mean = mean(f0raw, na.rm=TRUE)) %>% 
  ungroup()

c <- frb %>% 
  filter(Speaker=="Confederate") %>% 
  group_by(Speaker, Condition) %>% 
  summarize(f0mean = mean(f0raw, na.rm=TRUE)) %>% 
  ungroup()

df <- rbind(df, c)

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
  draw_label("Individual f0 (Joint Read Speech)", size=50, fontface = 'bold', x = 0, hjust = 0)+
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

png(paste0(folder2, "JointReadSpeech-F0.png"), width=3000, height=2000)
plot_grid(title, plotgrid, ncol=1, rel_heights = c(0.1, 1))
dev.off()
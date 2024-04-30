# Tom Offrede
# make figure(s?) for thesis

library(tidyverse)
library(ggsignif)
library(ggdist)
library(tuneR)
library(broom.mixed)
library(ggpubr)
library(lme4)
library(viridis)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/AllData/"
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/FiguresForPaper/"
folder3 <- "C:/Users/offredet/Documents/1HU/ExperimentBreathing/Data/DataForAnalysis/PeaksValleys/"

theme_set(theme_bw()+
            theme(axis.ticks.y=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title = element_text(size=13),
                  axis.text.x = element_text(color="black", size=12),
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.title = element_text(hjust = 0.5),
                  strip.background = element_blank()))
shape <- 16

b <- readWave(paste0(folder3, "SB-ATN008_SUM_200.wav"))
bq <- data.frame(w = b@left[(28.122*b@samp.rate):(40.9*b@samp.rate)],
                 time = seq(0, 25.56, length.out=2556)) # 2556 is the number of rows of b@left[...]
b <- readWave(paste0(folder3, "SR-ATN010_SUM_200_joint_breath.wav"))
bs <- data.frame(w = b@left[(27.8*b@samp.rate):(40.76*b@samp.rate)],
                 time = seq(0, 25.93, length.out=2593)) # 2593 is the number of rows of b@left[...]

(quiet <- ggplot(bq, aes(time, w))+
  geom_line(linewidth=1)+
  labs(title="",
       x=NULL,
       y=NULL)+
  ggtitle("Quiet Breathing")+
  theme(legend.position="none",
        axis.text.x=element_text(size=16),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size=22),
        axis.title = element_text(size=20),
        panel.border = element_blank())+
  ylim(c(-1300,1300)))

(speech <- ggplot(bs, aes(time, w))+
  geom_line(linewidth=1)+
  labs(title="",
       x=NULL,
       y=NULL)+
  ggtitle("Speech Breathing")+
  theme(legend.position="none",
        axis.text.y=element_blank(),
        axis.text.x=element_text(size=16),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size=22),
        axis.title = element_text(size=20),
        panel.border = element_blank())+
  ylim(c(-2300,700)))

annotate_figure(ggarrange(quiet, speech, ncol=2, nrow=1),
                bottom=text_grob("Time (s)", size=20),
                lef=text_grob("Lung Volume (V)", rot=90, size=20))+
  border()

ggsave("C:/Users/offredet/Documents/1HU/Thesis/breathingTypes.png", width = 2500, height=1500, units="px", dpi = "retina")
# originally: width = 2500, height=2000

###########


library(tuneR)
library(tidyverse)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentBreathing/Data/Confederate/BreathingAllChannels/"

al <- list.files(folder, "ABDOMEN")
tl <- list.files(folder, "THORAX")
sl <- list.files(folder, "SUM")
l <- as.data.frame(cbind(al, tl))
l <- cbind(l, sl)

for(i in 1:nrow(l)){
  # i=0
  # i=i+1
  a <- readWave(paste0(folder, l$al[i]))@left
  t <- readWave(paste0(folder, l$tl[i]))@left
  s <- readWave(paste0(folder, l$sl[i]))@left
  b <- as.data.frame(cbind(a, t))
  b <- cbind(b, s)
  b$ind <- 1:nrow(b)
  b <- b %>% # scale the data
    mutate(a = (a-min(a)/max(a)-min(a))) %>%
    mutate(s = (s-min(s)/max(s)-min(s))) %>%
    mutate(t = (t-min(t)/max(t)-min(t)))
  name <- substr(l$al[i], 1, 6)
  png(paste0(folder, name, ".png"), width=3000) # make a very long plot so you can zoom in and look closely
  ggplot()+
    geom_line(b, mapping=aes(ind, a), color="green")+
    geom_line(b, mapping=aes(ind, t), color="blue")+
    geom_line(b, mapping=aes(ind, s), color="red")+
    ggtitle("Abdomen=green; thorax=blue; sum=red")+
    theme(title=element_text(size=25))+
    scale_color_manual(breaks=c('Abdomen', 'Thorax', 'Sum'), # the legend isn't being added...
                       values=c('Admonen'='green', 'Thorax'='blue', 'Sum'='red'))
  dev.off()
}


# maybe the loop doesn't work. in this case, you can define `i=0`, add `i=i+1` at the beginning of the loop's content, and select and run the loop's content 9 times
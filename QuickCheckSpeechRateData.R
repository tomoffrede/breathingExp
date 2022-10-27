# quick code to check the speech (and articulation) rate data after extracting it in the first chunk of the Preprocessing.R script
# (you just have to run Preprocessing.R until line 154, i.e. until the end of the for loop that creates `srf`)

srf <- srf %>% mutate(srDiff = speechRateIPU - syllSpeechRate,
                      arDiff = articRateIPU - syllArticRate,
                      cond = substr(file, 1, 1))

hist(srf$arDiff)
hist(srf$srDiff)

lm(arDiff ~ cond, srf)
lm(srDiff ~ cond, srf)

cor.test(srf$articRateIPU, srf$syllArticRate)
plot(srf$articRateIPU, srf$syllArticRate)
cor.test(srf$speechRateIPU, srf$syllSpeechRate)
plot(srf$speechRateIPU, srf$syllSpeechRate)

ggplot(srf, aes(cond, syllSpeechRate))+
  geom_boxplot()+
  ggtitle("Speech Rate - Syllables")
ggplot(srf, aes(cond, speechRateIPU))+
  geom_boxplot()+
  ggtitle("Speech Rate - Amplitude Env")
ggplot(srf, aes(cond, syllArticRate))+
  geom_boxplot()+
  ggtitle("Articulation Rate - Syllables")
ggplot(srf, aes(cond, articRateIPU))+
  geom_boxplot()+
  ggtitle("Aritculation Rate - Amplitude End")



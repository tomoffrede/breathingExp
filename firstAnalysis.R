md = read.csv("C:/Users/tomof/Documents/1.Humboldt-Universität_zu_Berlin/Experiment1/Data/metadata.csv", fileEncoding="UTF-8-BOM") # md for MetaData
str(md)
names(md)

md$Height = md$Height / 100

md$BMI = md$Weight / (md$Height)^2

md$BMI = as.numeric(md$BMI)

range(md$BMI, na.rm=TRUE) # from 18.07 to 26.45

md$TMF.F = md$TMF.F1 + md$TMF.F2 + md$TMF.F3 + md$TMF.F4 + md$TMF.F5 + md$TMF.F6
md$TMF.M = md$TMF.M1 + md$TMF.M2 + md$TMF.M3 + md$TMF.M4 + md$TMF.M5 + md$TMF.M6

# Reversed scores:
cols = c("GEPAQ.F2", "GEPAQ.M4")
md[,cols] = 8 - md[,cols] # it's a 7-point scale, hence 8 - x

# participant CBE didn't answer GEPAQ.M5, so it's NA. I'll get the mean of the other GEPAQ.M answers of this participant and use that as GEPAQ.M5
md$GEPAQ.M5[md$Participant=="CBE"] = (md$GEPAQ.M1[md$Participant=="CBE"] + md$GEPAQ.M2[md$Participant=="CBE"] + md$GEPAQ.M3[md$Participant=="CBE"] + md$GEPAQ.M4[md$Participant=="CBE"] + md$GEPAQ.M6[md$Participant=="CBE"] + md$GEPAQ.M7[md$Participant=="CBE"] + md$GEPAQ.M8[md$Participant=="CBE"]) / 7

md$GEPAQ.F = md$GEPAQ.F1 + md$GEPAQ.F2 + md$GEPAQ.F3 + md$GEPAQ.F4 + md$GEPAQ.F5 + md$GEPAQ.F6  + md$GEPAQ.F7 + md$GEPAQ.F8
md$GEPAQ.M = md$GEPAQ.M1 + md$GEPAQ.M2 + md$GEPAQ.M3 + md$GEPAQ.M4 + md$GEPAQ.M5 + md$GEPAQ.M6 + md$GEPAQ.M7 + md$GEPAQ.M8

md = md[,-c(3,4, 8:35)]

# TMF

range(md$TMF.F) # 23 - 41 (the min possible would be 6; max possible would be 42)
range(md$TMF.M) # 11 - 27

hist(md$TMF.F)
hist(md$TMF.M)

# GEPAQ

range(md$GEPAQ.F) # 33 - 50 (the min possible would be 8; max possible would be 56)
range(md$GEPAQ.M) # 24 - 44

hist(md$GEPAQ.F)
hist(md$GEPAQ.M)

png(file="genderHistograms.png")
par(mfrow=c(2,2))
hist(md$TMF.F, main="TMF- Fem", xlab="Scores (TMF)")
hist(md$TMF.M, main="TMF - Masc", xlab="Scores (TMF)")
hist(md$GEPAQ.F, main="Attributes - Fem", xlab="Scores (GEPAQ)")
hist(md$GEPAQ.M, main="Attributes - Masc", xlab="Scores (GEPAQ)")
dev.off()

png(file="genderBoxplots.png", res=5000)
par(mfrow=c(2,2))
boxplot(md$TMF.F, ylim=c(10,42), main="TMF - Fem", xlab="Range: 23-41")
boxplot(md$TMF.M, ylim=c(10,42), main="TMF - Masc", xlab="Range: 11-27")
boxplot(md$GEPAQ.F, ylim=c(23,56), main="Attributes - Fem", xlab="Range: 33-50")
boxplot(md$GEPAQ.M, ylim=c(23,56), main="Attributes - Masc", xlab="Range: 24-44")
dev.off()

cor.test(md$TMF.F, md$GEPAQ.F, method="pearson") # 0.35, p = 0.1
cor.test(md$TMF.M, md$GEPAQ.M, method="pearson") # -0.13, p = 0.56


setwd("F:/THESIS")
cond=read.csv("Condition.csv", header=T, na.strings="Z")

colnames(cond)[colnames(cond)=="SH..mm."] <- "SH"
colnames(cond)[colnames(cond)=="ï..Site"] <- "Site"
str(cond)

AB=subset(cond, Site=="AB")
LX=subset(cond, Site=="LX")
SL=subset(cond, Site=="SL")
LW=subset(cond, Site=="LW")
TB=subset(cond, Site=="TB")
CR=subset(cond, Site=="CR")

#kruskal wallis condition by site
kruskal.test(cond$Condition.Index ~ cond$Site)

CT = dunnTest(Condition.Index ~ Site,
              data = cond,
              method = "bonferroni")
CT

#kruskal wallis shell height by site
kruskal.test(cond$SH ~ cond$Site)

#logistic so use glm
modelC<-lm(cond$Condition.Index~cond$Sal)
summary(modelC)
#significant
plot(cond$Sal, cond$Condition.Index)
abline(modelC)
abline(modelC, col="red", lwd=3)
#positive trendline

model20 <- lm(Condition.Index~Temp, data=AB)
summary(model20)
plot(model20)


aov_residuals20 <- residuals(object = model20)
shapiro.test(aov_residuals20)
#residuals deviate from normal 
#try - log transform and then linear regression

##check for interaction between temp and salinity
#logistic on intensity
#data was log transformed
condition <- glm(Condition.Index ~ Temp + Sal + Site, data = cond, 
               family = binomial())
summary(condition)
#not working??????
#even after log transformed there were a small number of 
#original CI values that were over 10 so after the log transform
#they were over 1







#######################PREVIOUS###############
#Test Normality
library(car)
qqPlot(cond$Condition.Index) #not normal

qqPlot(cond$SH) #not normal

#Check Skewness
library(e1071)
skewness(cond$Condition.Index, na.rm = TRUE) #skewed towards left aka positive skew
#1.234128
hist(cond$Condition.Index, freq = FALSE)

skewness(cond$SH, na.rm = TRUE) #skewed towards left aka positive skew
#1.163319
hist(cond$SH, freq = FALSE)

#Check kurtosis
kurtosis(cond$Condition.Index, na.rm = TRUE) #fat tailed data distribution
#3.093517

kurtosis(cond$SH, na.rm = TRUE) #fat tailed data distribution
#1.798404

##testing for homoscedasticity
library(lmtest)
modelC=lm(Condition.Index ~ Site, data = cond)
bptest(modelC)
#p = 4.416e-10
#there is not constant variance or homoscedasticity in residual
#heteroscedasticity (unequal variance)

library(lmtest)
modelS=lm(SH ~ Site, data = cond)
bptest(modelS)
#p = 2.2e-16
#there is not constant variance or homoscedasticity in residual
#heteroscedasticity (unequal variance)

#Test Anova
library(tidyverse)
install.packages("tidyverse", dependencies= TRUE)

CI.1=aov(Condition.Index ~ Site,data = cond)
summary(CI.1)

#Test Welch Anova
CI.2=oneway.test(Condition.Index ~ Site,data = cond)
CI.2

#So far same story - now for post hoc test
TukeyHSD(CI.1)

with(cond ,oneway(x=Site,y=Condition.Index ,posthoc="games-howell"))



#######################################################################33
colnames(cond)[colnames(cond)=="Condition.Index"] <- "CI"
str(cond)

model1<-lm(CI~Temp, data = cond)
summary(model1)

attributes (model1)

model1$coefficients

plot(Temp, CI, data = cond)
abline(model1) #adds the regression model line
abline(model1, col="red", lwd=3)



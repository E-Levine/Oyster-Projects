#setwd("F:/THESIS")
dermo=read.csv("Data/Dermo.csv", header=T, na.strings = "Z")

AB=subset(dermo, Site=="AB")
LX=subset(dermo, Site=="LX")
SL=subset(dermo, Site=="SL")
LW=subset(dermo, Site=="LW")
TB=subset(dermo, Site=="TB")
CR=subset(dermo, Site=="CR")

hist(dermo$PresenceBoth)
#count data

hist(dermo$Intensity)
#staged 0-5
hist(dermo$LogInt)

#kruskal wallis presence between site
kruskal.test(dermo$PresenceBoth ~ dermo$Site)

DT = dunnTest(PresenceBoth ~ Site,
              data = dermo,
              method = "bonferroni")
DT

#kruskal wallis intensity between site
kruskal.test(dermo$Intensity ~ dermo$Site)

IT = dunnTest(Intensity ~ Site,
              data = dermo,
              method = "bonferroni")
IT

#kruskal wallis intensity between seasons
kruskal.test(dermo$Intensity ~ dermo$Season)
#no significant difference

#kruskal wallis prevalence between seasons
kruskal.test(dermo$PresenceBoth ~ dermo$Season)
#I don't think I'm doing this right



###env parameters on dermo intensity in each site
#biologically: higher salinity = more dermo
modelD<-lm(dermo$Intensity~dermo$Sal)
summary(modelD)
#significant
plot(dermo$Sal, dermo$Intensity)
abline(modelD)
abline(modelD, col="red", lwd=3)
#positive trendline

#biologically: highter temps = more dermo
modelI<-lm(dermo$Intensity~dermo$Temp)
summary(modelI)
#not significant
plot(dermo$Temp, dermo$Intensity)
abline(modelI)
abline(modelI, col="red", lwd=3)
#negative trendline - barely

model00 <- lm(Intensity~Temp, data=AB)
summary(model00)
plot(model00)
#residuals look normal??

aov_residuals00 <- residuals(object = model00)
shapiro.test(aov_residuals00)
#residuals deviate from normal 
#try - log transform and then linear regression

model01 <- lm(Intensity~Sal, data = AB)
summary(model01)
plot(model01)

aov_residuals01 <- residuals(object = model01)
shapiro.test(aov_residuals01)
#residuals deviate from normal 
#try - log transform and then linear regression

###env parameters on dermo prevalence in each site
##count data - poisson
modelP<-lm(dermo$PresenceBoth~dermo$Sal)
summary(modelP)
#significant
plot(dermo$Sal, dermo$PresenceBoth)
abline(modelP)
abline(modelP, col="red", lwd=3)
#positive trendline

#biologically: highter temps = more dermo
modelB<-lm(dermo$PresenceBoth~dermo$Temp)
summary(modelB)
#significant
plot(dermo$Temp, dermo$PresenceBoth)
abline(modelB)
abline(modelB, col="red", lwd=3)
#negative trendline

model10 <- lm(PresenceBoth~Temp, data=AB)
summary(model10)
#not significant
plot(model10)

aov_residuals10 <- residuals(object = model10)
shapiro.test(aov_residuals10)
#residuals deviate from normal 

model11 <- lm(PresenceBoth~Sal, data = AB)
summary(model11)
#significant
plot(model11)

aov_residuals11 <- residuals(object = model11)
shapiro.test(aov_residuals01)
#residuals deviate from normal 

##check for interaction between temp and salinity
#poisson on prevalence
prev <- glm(PresenceBoth ~ Temp + Sal + Site, data=dermo, 
                family=poisson())
summary(prev)
#break up by site?

library(pscl)
pR2(prev)

##check for interaction between temp and salinity
#logistic on intensity
#data was log transformed
intense <- glm(LogInt ~ Temp + Sal + Temp:Sal, data = dermo, 
               family = binomial())
summary(intense)
#there is an interaction between salintiy and temp on whole dermo set
#break up by site?
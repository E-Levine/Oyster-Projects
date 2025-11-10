#setwd("F:/THESIS")
repro=read.csv("Data/Repro.csv", header=T, na.strings = "Z")

colnames(repro)[colnames(repro)=="?..Site"] <- "Site"

AB=subset(repro, Site=="AB")
LX=subset(repro, Site=="LX")
SL=subset(repro, Site=="SL")
LW=subset(repro, Site=="LW")
TB=subset(repro, Site=="TB")
CR=subset(repro, Site=="CR")

########################################################################
##GLM

library(AICcmodavg)
library(lmtest)
attach(repro)

lin.mod <- lm(GonadPercent ~ Temp + Sal, data = repro)
summary(lin.mod)

plot(lin.mod) 

shapiro.test(residuals(lin.mod))
bptest(lin.mod) 
#Since the p-value is not less than 0.05, we fail to reject the null 
#hypothesis. We do not have sufficient evidence to say that 
#heteroscedasticity is present in the regression model.
#proceed to interpret the output of the original regression.

model1<-lm(GonadPercent~Temp)
summary(model1)
plot(Sal, GonadPercent)
abline(model1) #adds the regression model line
abline(model1, col="red", lwd=3)
#positive trendline

model2<-lm(GonadPercent~Sal)
summary(model2)
plot(Sal, GonadPercent)
abline(model2)
abline(model2, col="red", lwd=3)
#negative trendline
#inverse relationship between salinity and temperature on gonad percent

##mixed effects
install.packages("packagename")
library(lme4)
m <- glmer(PercentDiv ~ Temp + Sal + (1 | Site), data = repro, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)
summary(m)


# print the mod results without correlations among fixed effects
print(m, corr = FALSE)

se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 *
                se))
exp(tab)



#####GLM neg binomial on gonad percent
##AB x Gonad x Temp and Sal
lin.modAB <- glm(PercentDiv ~ Temp + Sal, family=binomial(), data = AB)

summary(lin.modAB)
#there is an interaction between temp and sal so drop other two and
#use interaction

#####
##TB x Gonad x Temp and Sal
lin.modTB <- glm(PercentDiv ~ Temp + Sal + Site, family=binomial(), 
                 data = repro)

summary(lin.modTB) 
#only sig diff is between AB and CR with temp and salinity as effects
#as temp increases so does gonad percent
#CR significantly smaller gonad occupied by gametes than AB

library(pscl)
pR2(lin.modTB)

library(car)
vif(lin.modTB)

shellh <- lm(Height ~ Temp + Sal + Site, data = repro)
summary(shellh)

#####
##SL x Gonad x Temp and Sal
lin.modSL <- glm(PercentDiv ~ Temp + Sal, family=binomial(), data = SL)

summary(lin.modSL)

#####
##LX x Gonad x Temp and Sal
lin.modLX <- glm(PercentDiv ~ Temp + Sal, family=binomial(), data = LX)

summary(lin.modLX)

#####
##CR x Gonad x Temp and Sal
lin.modCR <- glm(PercentDiv ~ Temp + Sal, family=binomial(), data = CR)

summary(lin.modCR)

###
##LW x Gonad x Temp and sal
lin.modLW <- glm(PercentDiv ~ Temp + Sal, family=binomial(), data = LW)

summary(lin.modLW)

##########kruskal wallis month and site
#use bonferroni as adjustment
kruskal.test(repro$OocyteDiam ~ repro$Site)
# no sig diff
#0.189
kruskal.test(repro$GonadPercent ~ repro$Site)
# sig diff

GT = dunnTest(GonadPercent ~ Site,
              data = repro,
              method = "bonferroni")
GT

kruskal.test(repro$OocyteDiam ~ repro$Month)
#sig diff

GT = dunnTest(OocyteDiam ~ Site,
              data = repro,
              method = "bonferroni")
GT

kruskal.test(repro$PercentDiv ~ repro$Month)
#significant

################Buceph
##Count data - poisson
model3<-lm(Parasite~Sal)
summary(model2)
plot(Sal, GonadPercent)
abline(model2)
abline(model2, col="red", lwd=3)

parasite <- glm(Parasite ~ Temp + Sal + Temp:Sal, data=repro, 
                family=poisson())
summary(parasite)
#temp and sal are significant


##########Oocyte Diam
model6<-lm(OocyteDiam~Temp)
summary(model6)
plot(Sal, OocyteDiam)
abline(model6) #adds the regression model line
abline(model6, col="red", lwd=3)
#variable lengths differ
#can't run

model7<-lm(OocyteDiam~Sal)
summary(model7)
plot(Sal, OocyteDiam)
abline(model7)
abline(model7, col="red", lwd=3)
#variable lengths differ
#can't run

#kruskal wallis oocyte diam by site
kruskal.test(repro$OocyteDiam ~ repro$Site)
#no sig diff

#kruskal wallis oocyte diam by season
kruskal.test(repro$OocyteDiam ~ repro$Season)
#no sig diff
###AM I DOING THIS WRONG

#GLM oocyte Diam by temp and sal
##AB x Oocyte x Temp and Sal
lin.modABO <- glm(OocyteDiam ~ Temp + Sal, family=binomial(), data = AB)

summary(lin.modABO)
#variable lengths
#from Z's?

#####
##TB x Oocyte x Temp and Sal
lin.modTBO <- glm(OocyteDiam ~ Temp + Sal, family=binomial(), data = TB)

summary(lin.modTBO)

#####
##SL x Oocyte x Temp and Sal
lin.modSLO <- glm(OocyteDiam ~ Temp + Sal, family=binomial(), data = SL)

summary(lin.modSLO)

#####
##LX x Oocyte x Temp and Sal
lin.modLXO <- glm(OocyteDiam ~ Temp + Sal, family=binomial(), data = LX)

summary(lin.modLX)

#####
##CR x Oocyte x Temp and Sal
lin.modCRO <- glm(OocyteDiam ~ Temp + Sal, family=binomial(), data = CR)

summary(lin.modCRO)

###
##LW x Oocyte x Temp and sal
lin.modLWO <- glm(OocyteDiam ~ Temp + Sal, family=binomial(), data = LW)

summary(lin.modLWO)



#######################PREVIOUS###########################
#simple linear regression - Gonad Percent
attach(repro)
str(repro)
plot(Temp, GonadPercent, main = "Scatterplot of Temp vs. GonadPercent")


cor(Temp, GonadPercent, method="pearson")
#NA

model1<-lm(GonadPercent~Temp)
summary(model1)

attributes (model1)

model1$coefficients

plot(Temp, GonadPercent, main = "Scatterplot of Sal vs. GonadPercent")
abline(model1) #adds the regression model line
abline(model1, col="red", lwd=3)
#positive trendline

plot(model1)

library(dunn.test)
GT = dunnTest(GonadPercent ~ Site,
              data = repro,
              method = "bonferroni")
GT


cor(Temp, Sal, method="pearson")
#=-.306

##########################################################################
#simple linear regression
#####Model building#####
model3 <- lm(OocyteDiam~Temp, data=AB)
summary(model3)
plot(model3)


#Assumption 3: Normal distribution of model residuals
plot(model3,2)

aov_residuals <- residuals(object = model3)
shapiro.test(aov_residuals)
#0.04

#Assumption 4: Homogeneity of variance
plot(model3,3)


########################################################################
plot(lin.modAB) 

shapiro.test(residuals(lin.modAB))
#p value is significant
bptest(lin.modAB)
#Since the p-value is not less than 0.05, we fail to reject the null 
#hypothesis. We do not have sufficient evidence to say that 
#heteroscedasticity is present in the regression model.
##proceed to interpret the output of the original regression.

lin.mod3 <- glm(OocyteDiam ~ Temp + Sal, data = AB)
summary(lin.mod3)

plot(lin.mod3) 

shapiro.test(residuals(lin.mod3)) #normal?
bptest(lin.mod3) #p value is less than 0.05 so we reject the null 
#hypothesis. heteroscedasticity is present in the data. In this case, 
#the standard errors that are shown in the output table of the regression 
#may be unreliable
#transform response variable?

#AIC
lin.mod1 <- glm(GonadPercent ~ Temp + Sal, data = AB)
lin.mod2 <- glm(GonadPercent ~ Temp, data = AB)
lin.mod3 <- glm(GonadPercent ~ Sal, data = AB)
lin.mod4 <- glm(GonadPercent ~ 1, data = AB)
aictab(list(lin.mod1,lin.mod2,lin.mod3,lin.mod4)) 
# Which model is best? 

#####################################################################
#Test Normality
library(car)
qqPlot(repro$GonadPercent) #not normal

qqPlot(repro$OocyteDiam) #not normal

#Check Skewness
library(e1071)
skewness(repro$OocyteDiam, na.rm = TRUE) #skewed towards right 
#-1.537594
hist(repro$OocyteDiam, freq = FALSE)

skewness(repro$GonadPercent, na.rm = TRUE) #skewed towards left 
#0.5698278
hist(repro$GonadPercent, freq = FALSE)
mean(repro$GonadPercent, na.rm = TRUE)

#Check kurtosis
kurtosis(repro$OocyteDiam, na.rm = TRUE) #fat tailed data distribution
#1.718336

kurtosis(repro$GonadPercent, na.rm = TRUE) #thin tailed data distribution
#-0.8013699

##testing for homoscedasticity
library(lmtest)
modelY=lm(GonadPercent ~ Site, data = repro)
bptest(modelY)
#p = 0.002001
#there is not constant variance or homoscedasticity in residual
#heteroscedasticity (unequal variance)

library(lmtest)
modelT=lm(OocyteDiam ~ Site, data = repro)
bptest(modelT)
#p = 0.1341
#There is constant variance or homoscedasticity in residual
#equal variance
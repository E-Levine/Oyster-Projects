setwd("F:/THESIS")
repro=read.csv("Repro.csv", header=T, na.strings = "Z")
ABLog=read.csv("ABLog.csv", header=T)

wq=read.csv("WQ.csv", header = T, na.strings = "Z")
wq
repro
str(repro)

library(ggplot2)
library(tidyverse)
library(ggpubr)
library(dplyr)
library(RColorBrewer)
library(Hmisc)
library(plyr)
library(doBy)
library(dunn.test)
library(doBy)
library(lme4)
library(car)

str(gonadmean)
colnames(repro)[colnames(repro)=="ï..Site"] <- "Site"
colnames(wq)[colnames(wq)=="ï..Month"] <- "Month"
repro$Month = factor(repro$Month)
repro$Stage = factor(repro$Stage)

summary(wq)
wq
wq$Month = factor(wq$Month, levels = month.abb)
temp=aggregate(Temp ~ Month + Site, wq, mean)
sal=aggregate(Salinity ~ Month + Site, wq, mean)

levels(wq$Site)
levels(wq$Site) <- c("AB", "CR", "LW", "LX", "SL", "TB")
levels(wq$Site)

levels(temp$Site)
levels(temp$Site) <- c("AB", "CR", "LW", "LX", "SL", "TB")
levels(temp$Site)

levels(sal$Site)
levels(sal$Site) <- c("AB", "CR", "LW", "LX", "SL", "TB")
levels(sal$Site)

AB=subset(repro, Site=="AB")
LX=subset(repro, Site=="LX")
SL=subset(repro, Site=="SL")
LW=subset(repro, Site=="LW")
TB=subset(repro, Site=="TB")
CR=subset(repro, Site=="CR")

match(wq$Month, repro$Month)
wq$Temp[match(wq$Month, repro$Month)]
repro$Temp=wq$Temp[match(wq$Month, repro$Month)]
head(repro)


F=subset(repro, Sex=="F")
M=subset(repro, Sex=="M")
Z=subset(repro, Sex=="X")

colors <- c("dodgerblue4", "chocolate", "mediumpurple2", "violetred", 
            "forestgreen", "goldenrod1")
names(colors) <- c("AB", "CR", "LW", "LX", "SL", "TB")
colors

prop.table(with(AB, table(Month, Sex)), 1) 
prop.table(with(AB, table(Month, Stage)),1)

prop.table(with(LX, table(Month, Sex)), 1)
prop.table(with(LX, table(Month, Stage)),1)

prop.table(with(SL, table(Month, Sex)), 1)
prop.table(with(SL, table(Month, Stage)),1)

prop.table(with(LW, table(Month, Sex)), 1)
prop.table(with(LW, table(Month, Stage)),1)

prop.table(with(TB, table(Month, Sex)), 1)
prop.table(with(TB, table(Month, Stage)),1)

prop.table(with(CR, table(Month, Sex)), 1)
prop.table(with(CR, table(Month, Stage)),1)

gonadmean=data.frame(aggregate(GonadPercent ~ Month + Site, repro, mean))
oocytemean=data.frame(aggregate(OocyteDiam ~ Month + Site, repro, mean))
oocytestage=data.frame(aggregate(OocyteDiam ~ Site + Stage, repro, mean))
gonadstage=data.frame(aggregate(GonadPercent ~ Site + Stage, repro, mean))
stagemean=data.frame(aggregate(Stage ~ Site + Month, repro, mean))

#Combine averages of oocyte and gonad to one datafile
match(gonadmean$Month, oocytemean$Month)
oocytemean$OocyteDiam[match(gonadmean$Month, oocytemean$Month)]
gonadmean$OocyteDiam = oocytemean$OocyteDiam[match(gonadmean$Month, 
                                                   oocytemean$Month)]
#Combine temperature into gonad file
match(gonadmean$Month, temp$Month)
Temp$Temp[match(gonadmean$Month, Temp$Month)]
gonadmean$Temp = Temp$Temp[match(gonadmean$Month, Temp$Month)]

#Combine salinity into gonad file
match(gonadmean$Month, sal$Month)
sal$Salinity[match(gonadmean$Month, sal$Month)]
gonadmean$Salinity = sal$Salinity[match(gonadmean$Month, sal$Month)]

#Combine stage into gonad file
match(gonadmean$Month, stagemean$Month)
stagemean$Stage[match(gonadmean$Month, stagemean$Month)]
gonadmean$Stage = stagemean$Stage[match(gonadmean$Month, stagemean$Month)]
gonadmean

###Plotting
###PROPORTIONS##
#APALACH
repro%>%filter(Site=="AB")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Stage),position="fill") +
  ylab(NULL) +
  labs(title = "Apalachicola Bay") +
  scale_x_discrete(limits = month.abb) +
  theme_classic2()+
  scale_fill_brewer(palette="Blues")
repro%>%filter(Site=="AB")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Sex), position="fill") +
  ylab(NULL) +
  labs(title = "Apalachicola Bay") +
  scale_x_discrete(limits = month.abb) +
  theme_classic2()+
  scale_fill_brewer(palette="Greens")
repro%>%filter(Site=="AB")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Parasite), position="fill") +
  ylab(NULL) +
  labs(title = "Apalachicola Bay") +
  scale_x_discrete(limits = month.abb) +
  theme_classic2() +
  scale_fill_brewer(palette="Purples")

#CALOOSAHATCHEE
repro%>%filter(Site=="CR")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Stage),position="fill") +
  ylab(NULL) +
  labs(title = "Caloosahatchee") +
  theme_classic2()+
  scale_x_discrete(limits = month.abb) +
  scale_fill_brewer(palette="Blues")
repro%>%filter(Site=="CR")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Sex), position="fill") +
  ylab(NULL) +
  labs(title = "Caloosahatchee") +
  scale_x_discrete(limits = month.abb) +
  theme_classic2()+
  scale_fill_brewer(palette="Greens")
repro%>%filter(Site=="CR")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Parasite), position="fill") +
  ylab(NULL) +
  labs(title = "Caloosahatchee") +
  scale_x_discrete(limits = month.abb) +
  theme_classic2()+
  scale_fill_brewer(palette="Purples")

#ST LUCIE
repro%>%filter(Site=="SL")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Stage),position="fill") +
  ylab(NULL) +
  labs(title = "St. Lucie") +
  theme_classic2()+
  scale_x_discrete(limits = month.abb) +
  scale_fill_brewer(palette="Blues")
repro%>%filter(Site=="SL")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Sex),position="fill") +
  ylab(NULL) +
  labs(title = "St. Lucie") +
  theme_classic2()+
  scale_x_discrete(limits = month.abb) +
  scale_fill_brewer(palette="Greens")
repro%>%filter(Site=="SL")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Parasite),position="fill") +
  ylab(NULL) +
  labs(title = "St. Lucie") +
  scale_x_discrete(limits = month.abb) +
  theme_classic2()+
  scale_fill_brewer(palette="Purples")

#LOXAHATCHEE
repro%>%filter(Site=="LX")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Stage),position="fill") +
  ylab(NULL) +
  labs(title = "Loxahatchee") +
  scale_x_discrete(limits = month.abb) +
  theme_classic2()+
  scale_fill_brewer(palette="Blues")
repro%>%filter(Site=="LX")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Sex),position="fill") +
  ylab(NULL) +
  labs(title = "Loxahatchee") +
  scale_x_discrete(limits = month.abb) +
  theme_classic2()+
  scale_fill_brewer(palette="Greens")
repro%>%filter(Site=="LX")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Parasite),position="fill") +
  ylab(NULL) +
  labs(title = "Loxahatchee") +
  scale_x_discrete(limits = month.abb) +
  theme_classic2()+
  scale_fill_brewer(palette="Purples")

#LAKE WORTH
repro%>%filter(Site=="LW")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Stage),position="fill") +
  ylab(NULL) +
  labs(title = "Lake Worth") +
  scale_x_discrete(limits = month.abb) +
  theme_classic2()+
  scale_fill_brewer(palette="Blues")
repro%>%filter(Site=="LW")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Sex),position="fill") +
  ylab(NULL) +
  labs(title = "Lake Worth") +
  scale_x_discrete(limits = month.abb) +
  theme_classic2()+
  scale_fill_brewer(palette="Greens")
repro%>%filter(Site=="LW")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Parasite),position="fill") +
  ylab(NULL) +
  labs(title = "Lake Worth") +
  scale_x_discrete(limits = month.abb) +
  theme_classic2()+
  scale_fill_brewer(palette="Purples")

#TAMPA
repro%>%filter(Site=="TB")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Stage),position="fill") +
  ylab(NULL) +
  theme_classic2()+
  labs(title = "Tampa Bay") +
  scale_x_discrete(limits = month.abb) +
  scale_fill_brewer(palette="Blues")
repro%>%filter(Site=="TB")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Sex),position="fill") +
  ylab(NULL) +
  labs(title = "Tampa Bay") +
  scale_x_discrete(limits = month.abb) +
  theme_classic2() +
  scale_fill_brewer(palette="Greens")
repro%>%filter(Site=="TB")%>%
  ggplot()+geom_bar(mapping=aes(x=Month, fill=Parasite),position="fill") +
  ylab(NULL) +
  labs(subtitle = "TB") +
  scale_x_discrete(limits = month.abb) +
  theme_classic() +
  scale_fill_brewer(palette="Purples")

###NUM OF OYSTERS W PARASITES
par <- with(repro, table(Parasite, Site))

ggplot(as.data.frame(par), aes(factor(Parasite), Freq, fill = Site)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y= "Number of Oysters with Parasites", x = "Parasites") +
  scale_y_continuous(limits=c(0,140), breaks = seq(0,140, by = 20)) +
  scale_fill_manual(values = colors) +
  theme_classic2()


####Gonad and Oocyte measurements
###GONAD
gonaddata=data.frame(repro %>%
                    select(Site, Month, GonadPercent))
gonaddatasummary <- ddply(gonaddata, c("Month", "Site"), summarise,
                       N    = length(GonadPercent),
                       mean = mean(GonadPercent, na.rm=TRUE),
                       sd   = sd(GonadPercent, na.rm=TRUE),
                       se   = sd / sqrt(N))
gonaddatasummary

ggplot(gonaddatasummary, aes(x=Month, y=mean, colour=Site, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1) +
  geom_line() +
  geom_point() +
  labs(y= "Average Gonad Percent Area", x = "Month") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,70), breaks = seq(0,70, by = 10)) +
  theme_classic2() +
  scale_colour_manual(values = colors)

##by individual site
gonaddatasummary%>%filter(Site=="AB")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="seagreen") +
  geom_line(color="dodgerblue4") +
  geom_point(color="dodgerblue4") +
  labs(y= "Average Gonad Percent Area", x = "Month") +
  labs(title = "Apalachicola Bay") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,70), breaks = seq(0,70, by = 10)) +
  theme_classic2()

gonaddatasummary%>%filter(Site=="TB")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="goldenrod1") +
  geom_line(color="goldenrod1") +
  geom_point(color="goldenrod1") +
  labs(y= "Average Gonad Percent Area", x = "Month") +
  labs(title = "Tampa Bay") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,70), breaks = seq(0,70, by = 10)) +
  theme_classic2()

gonaddatasummary%>%filter(Site=="CR")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="chocolate") +
  geom_line(color="chocolate") +
  geom_point(color="chocolate") +
  labs(y= "Average Gonad Percent Area", x = "Month") +
  labs(title = "Caloosahatchee") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,70), breaks = seq(0,70, by = 10)) +
  theme_classic2()

gonaddatasummary%>%filter(Site=="LW")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="mediumpurple3") +
  geom_line(color="mediumpurple2") +
  geom_point(color="mediumpurple2") +
  labs(y= "Average Gonad Percent Area", x = "Month") +
  labs(title = "Lake Worth") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,70), breaks = seq(0,70, by = 10)) +
  theme_classic2()

gonaddatasummary%>%filter(Site=="LX")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="violetred") +
  geom_line(color="violetred") +
  geom_point(color="violetred") +
  labs(y= "Average Gonad Percent Area", x = "Month") +
  labs(title = "Loxahatchee") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,70), breaks = seq(0,70, by = 10)) +
  theme_classic2()

gonaddatasummary%>%filter(Site=="SL")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="forestgreen") +
  geom_line(color="forestgreen") +
  geom_point(color="forestgreen") +
  labs(y= "Average Gonad Percent Area", x = "Month") +
  labs(title = "St. Lucie") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,70), breaks = seq(0,70, by = 10)) +
  theme_classic2()

####OOCYTE 
oocytedata=data.frame(repro %>%
                       select(Site, Month, OocyteDiam))
oocytedatasummary <- ddply(oocytedata, c("Month", "Site"), summarise,
                          N    = length(OocyteDiam),
                          mean = mean(OocyteDiam, na.rm=TRUE),
                          sd   = sd(OocyteDiam, na.rm=TRUE),
                          se   = sd / sqrt(N))
oocytedatasummary

ggplot(oocytedatasummary, aes(x=Month, y=mean, colour=Site, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1) +
  geom_line() +
  geom_point() +
  labs(y= "Average Oocyte Diamater (µm)", x = "Month") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,50), breaks = seq(0,50, by = 10)) +
  theme_classic2() +
  scale_colour_manual(values = colors)

##by individual sites
oocytedatasummary%>%filter(Site=="AB")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="seagreen") +
  geom_line(color="dodgerblue4") +
  geom_point(color="dodgerblue4") +
  labs(y= "Average Oocyte Diameter (µm)", x = "Month") +
  labs(title = "Apalachicola Bay") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,50), breaks = seq(0,50, by = 10)) +
  theme_classic2()

oocytedatasummary%>%filter(Site=="TB")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="goldenrod1") +
  geom_line(color="goldenrod1") +
  geom_point(color="goldenrod1") +
  labs(y= "Average Oocyte Diameter (µm)", x = "Month") +
  labs(title = "Tampa Bay") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,50), breaks = seq(0,50, by = 10)) +
  theme_classic2()

oocytedatasummary%>%filter(Site=="CR")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="chocolate") +
  geom_line(color="chocolate") +
  geom_point(color="chocolate") +
  labs(y= "Average Oocyte Diameter (µm)", x = "Month") +
  labs(title = "Caloosahatchee") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,50), breaks = seq(0,50, by = 10)) +
  theme_classic2()

oocytedatasummary%>%filter(Site=="LW")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="mediumpurple3") +
  geom_line(color="mediumpurple2") +
  geom_point(color="mediumpurple2") +
  labs(y= "Average Oocyte Diameter (µm)", x = "Month") +
  labs(title = "Lake Worth") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,50), breaks = seq(0,50, by = 10)) +
  theme_classic2()

oocytedatasummary%>%filter(Site=="LX")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="violetred") +
  geom_line(color="violetred") +
  geom_point(color="violetred") +
  labs(y= "Average Oocyte Diameter (µm)", x = "Month") +
  labs(title = "Loxahatchee") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,50), breaks = seq(0,50, by = 10)) +
  theme_classic2()

oocytedatasummary%>%filter(Site=="SL")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="forestgreen") +
  geom_line(color="forestgreen") +
  geom_point(color="forestgreen") +
  labs(y= "Average Oocyte Diameter (µm)", x = "Month") +
  labs(title = "St. Lucie") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,50), breaks = seq(0,50, by = 10)) +
  theme_classic2()

###SH BY STAGE
summary(repro$SH)
repro<-na.omit(repro)

shellstage=data.frame(repro %>%
                     select(Site, Stage, SH))
shellstagedata <- ddply(oostage, c("Stage", "Site"), summarise,
                     N    = length(SH),
                     mean = mean(SH),
                     sd   = sd(SH),
                     se   = sd / sqrt(N))
shellstagedata

ggplot(data=shellstagedata, aes(x=Stage, y=mean, fill=Site,)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=.2,    # Thinner lines
                width=.2,
                position=position_dodge(.93)) +
  labs(y= "Average Shell Height (mm)", x = "Stage") +
  scale_y_continuous(limits=c(0,120), breaks = seq(0,120, by = 20)) +
  scale_fill_manual(values = colors) +
  theme_classic2()

###OOCYTE BY STAGE
summary(repro$OocyteDiam)
repro<-na.omit(repro)

oostage=data.frame(repro %>%
                     select(Site, Stage, OocyteDiam))
oostagedata <- ddply(oostage, c("Stage", "Site"), summarise,
                     N    = length(OocyteDiam),
                     mean = mean(OocyteDiam),
                     sd   = sd(OocyteDiam),
                     se   = sd / sqrt(N))
oostagedata

ggplot(data=oostagedata, aes(x=Stage, y=mean, fill=Site,)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=.2,    # Thinner lines
                width=.2,
                position=position_dodge(.93)) +
  labs(y= "Average Oocyte Diameter (µm)", x = "Stage") +
  scale_y_continuous(limits=c(0,40), breaks = seq(0,40, by = 10)) +
  scale_fill_manual(values = colors) +
  theme_classic2()

###GONAD BY STAGE
gonadstage=data.frame(repro %>%
                     select(Site, Stage, GonadPercent))
gonadstagedata <- ddply(gonadstage, c("Stage", "Site"), summarise,
                     N    = length(GonadPercent),
                     mean = mean(GonadPercent),
                     sd   = sd(GonadPercent),
                     se   = sd / sqrt(N))
gonadstagedata

ggplot(data=gonadstagedata, aes(x=Stage, y=mean, fill=Site,)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=.2,    # Thinner lines
                width=.2,
                position=position_dodge(.93)) +
  labs(y= "Average Gonad Percent Area", x = "Stage") +
  scale_y_continuous(limits=c(0,70), breaks = seq(0,70, by = 10)) +
  scale_fill_manual(values = colors) +
  theme_classic2()

###Test normality
qqnorm(repro$OocyteDiam)
qqPlot(repro$OocyteDiam)

shapiro.test(repro$OocyteDiam)
hist(repro$OocyteDiam)

qqnorm(repro$GonadPercent)
qqPlot(repro$GonadPercent)
qqline(repro$GonadPercent)

shapiro.test(repro$GonadPercent)
hist(repro$GonadPercent)
#both not normal

shapiro.test(AB$GonadPercent)
hist(AB$GonadPercent)

qqnorm(AB$GonadPercent)
qqPlot(AB$GonadPercent)
qqline(AB$GonadPercent)

shapiro.test(AB$LogGonad)
head(gonadmean)

shapiro.test(ABLog$LogGonad)




#TEST HOMOGENEITY OF VARIANCE
#since data is non normal use levenes test
library(car)
leveneTest(repro$GonadPercent, repro$Site, center = mean)
#variances assumed to not be equal

##testing for homoscedasticity
library(lmtest)
modelY=lm(GonadPercent ~ Site, data = repro)
bptest(modelY)
#there is not constant variance or homoscedasticity in residual
#heterscedasticity (unequal variance)

library(lmtest)
modelT=lm(OocyteDiam ~ Site, data = repro)
bptest(modelT)
#There is constant variance or homoscedasticity in residual
#equal variance

#DUNNTEST FOR OOCYTE
library(FSA)
OT = dunnTest(OocyteDiam ~ Site,
              data = repro,
              method = "bh")
OT

OT = OT$res
OT
library(rcompanion)
cldList(comparison = OT$Comparison,
        p.value = OT$P.adj,
        threshold = 0.05)
ootmp <- dunnTest(OocyteDiam ~ Site, data = repro)
print(ootmp, dunn.test.results=TRUE)
##NO SIG DIFFERENCES

a <- ggplot(oocytemean, aes(x = Site, y = OocyteDiam)) +
  geom_boxplot() +
  labs(y= "Oocyte Diameter (µm)", x = "Site") +
  scale_y_continuous(limits=c(0,40), breaks = seq(0,40, by = 10)) +
  theme_classic() 
a

##DUNNTEST FOR GONAD
GT = dunnTest(GonadPercent ~ Site,
              data = repro,
              method = "bonferroni")
GT

GT = GT$res
GT 
library(rcompanion)
cldList(comparison = GT$Comparison,
        p.value = GT$P.adj,
        threshold = 0.05)
###Group Letter MonoLetter
###1    AB     ab        ab 
###2    CR      a        a  
###3    LW      c          c
###4    LX     bc         bc
###5    SL      c          c
###6    TB      a        a  

Sum = groupwiseMean(GonadPercent ~ Site,
                      data = repro,
                      conf = 0.95,
                      R = 5000,
                      percentile = TRUE,
                      bca = FALSE,
                      digits = 3)
Sum
X = 1:6
Y = Sum$Percentile.upper = 0.2
Label = c("ab", "a", "c", "bc", "c", "a")


b <- ggplot(gonadmean, aes(x = Site, y = GonadPercent)) +
  geom_boxplot() +
  labs(y= "Gonad Percent Area", x = "Site") +
  scale_y_continuous(limits=c(0,60), breaks = seq(0,60, by = 20)) +
  annotate("text", x=1:6, y=60, label=Label) +
  theme_classic()
b

#test for sig diff
kruskal.test(repro$OocyteDiam ~ repro$Site)
# no sig diff

kruskal.test(repro$GonadPercent ~ repro$Month)
#significant

GT = dunnTest(GonadPercent ~ Month,
              data = repro,
              method = "bonferroni")
GT

kruskal.test(repro$GonadPercent ~ repro$Site)
# sig diff

kruskal.test(repro$OocyteDiam ~ repro$Month)
#sig diff
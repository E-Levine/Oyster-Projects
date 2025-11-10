cond=read.csv("Data/Condition.csv", header=T, na.strings="Z")
attach(cond)
head(cond)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(dplyr)
library(RColorBrewer)
library(Hmisc)
library(plyr)
str(cond)

summary(cond)

cond$Month = factor(cond$Month, levels = month.abb)

colors <- c("dodgerblue4", "chocolate", "mediumpurple2", "violetred", 
            "forestgreen", "goldenrod1")
names(colors) <- c("AB", "CR", "LW", "LX", "SL", "TB")
colors

colnames(cond)[colnames(cond)=="SH.mm"] <-"SH"
colnames(cond)[colnames(cond)=="Total.Weight..g."] <-"Total.Weight"
colnames(cond)[colnames(cond)=="?..Site"] <- "Site"

ABS=subset(cond, Site=="AB")
LXN=subset(cond, Site=="LX")
SLC=subset(cond, Site=="SL")
LW=subset(cond, Site=="LW")
TB=subset(cond, Site=="TB")
CR=subset(cond, Site=="CR")

avgcond=data.frame(aggregate(Condition.Index ~ Month + Site, cond, mean))
avgsh=data.frame(aggregate(SH ~ Month + Site, cond, mean))
aggregate(Total.Weight ~ Month + Site, cond, mean)

###PLOT AVERAGE CONDITION INDEX OVER TIME###
##Between site
CIdata=data.frame(cond %>%
                    dplyr::select(Site, Month, Condition.Index))
CIdatasummary <- ddply(CIdata, c("Month", "Site"), summarise,
                    N    = length(Condition.Index),
                    mean = mean(Condition.Index),
                    sd   = sd(Condition.Index),
                    se   = sd / sqrt(N))
CIdatasummary

ggplot(CIdatasummary, aes(x=Month, y=mean, colour=Site, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1) +
  geom_line() +
  geom_point() +
  labs(y= "Average Condition Index", x = "Month") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,6), breaks = seq(0,6, by = 1)) +
  theme_classic2() +
  scale_colour_manual(values = colors)

####EACH SITE INDIVIDUALLY
CIdatasummary%>%filter(Site=="TB")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="goldenrod1") +
  geom_line(color="goldenrod1") +
  geom_point(color="goldenrod1") +
  labs(y= "Average Condition Index", x = "Month") +
  labs(title = "Tampa Bay") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,6), breaks = seq(0,6, by = 1)) +
  theme_classic2()

CIdatasummary%>%filter(Site=="AB")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="seagreen") +
  geom_line(color="dodgerblue4") +
  geom_point(color="dodgerblue4") +
  labs(y= "Average Condition Index", x = "Month") +
  labs(title = "Apalachicola Bay") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,6), breaks = seq(0,6, by = 1)) +
  theme_classic2()

CIdatasummary%>%filter(Site=="CR")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="chocolate") +
  geom_line(color="chocolate") +
  geom_point(color="chocolate") +
  labs(y= "Average Condition Index", x = "Month") +
  labs(title = "Caloosahatchee") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,6), breaks = seq(0,6, by = 1)) +
  theme_classic2()

CIdatasummary%>%filter(Site=="LW")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="mediumpurple3") +
  geom_line(color="mediumpurple2") +
  geom_point(color="mediumpurple2") +
  labs(y= "Average Condition Index", x = "Month") +
  labs(title = "Lake Worth") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,6), breaks = seq(0,6, by = 1)) +
  theme_classic2()

CIdatasummary%>%filter(Site=="LX")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="violetred") +
  geom_line(color="violetred") +
  geom_point(color="violetred") +
  labs(y= "Average Condition Index", x = "Month") +
  labs(title = "Loxahatchee") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,6), breaks = seq(0,6, by = 1)) +
  theme_classic2()

CIdatasummary%>%filter(Site=="SL")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="forestgreen") +
  geom_line(color="forestgreen") +
  geom_point(color="forestgreen") +
  labs(y= "Average Condition Index", x = "Month") +
  labs(title = "St. Lucie") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,6), breaks = seq(0,6, by = 1)) +
  theme_classic2()

###SHELL HEIGHT
SHdata=data.frame(cond %>%
                    dplyr::select(Site, Month, SH))
SHdatasummary <- ddply(SHdata, c("Month", "Site"), summarise,
                       N    = length(SH),
                       mean = mean(SH),
                       sd   = sd(SH),
                       se   = sd / sqrt(N))
SHdatasummary

ggplot(SHdatasummary, aes(x=Month, y=mean, colour=Site, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1) +
  geom_line() +
  geom_point() +
  labs(y= "Average Shell Height (mm)", x = "Month") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100, by = 20)) +
  theme_classic2() +
  scale_colour_manual(values = colors)

####EACH SITE INDIVIDUALLY
SHdatasummary%>%filter(Site=="TB")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="goldenrod1") +
  geom_line(color="goldenrod1") +
  geom_point(color="goldenrod1") +
  labs(y= "Average Shell Height (mm)", x = "Month") +
  labs(subtitle = "TB") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100, by = 20)) +
  theme_classic()

SHdatasummary%>%filter(Site=="AB")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="seagreen") +
  geom_line(color="seagreen") +
  geom_point(color="seagreen") +
  labs(y= "Average Shell Height (mm)", x = "Month") +
  labs(subtitle = "AB") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100, by = 20)) +
  theme_classic()

SHdatasummary%>%filter(Site=="CR")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="chocolate") +
  geom_line(color="chocolate") +
  geom_point(color="chocolate") +
  labs(y= "Average Shell Height (mm)", x = "Month") +
  labs(subtitle = "CR") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100, by = 20)) +
  theme_classic()

SHdatasummary%>%filter(Site=="LW")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="mediumpurple3") +
  geom_line(color="mediumpurple2") +
  geom_point(color="mediumpurple2") +
  labs(y= "Average Shell Height (mm)", x = "Month") +
  labs(subtitle = "LW") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100, by = 20)) +
  theme_classic()

SHdatasummary%>%filter(Site=="LX")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="violetred") +
  geom_line(color="violetred") +
  geom_point(color="violetred") +
  labs(y= "Average Shell Height (mm)", x = "Month") +
  labs(subtitle = "LX") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100, by = 20)) +
  theme_classic()

SHdatasummary%>%filter(Site=="SL")%>%
  ggplot(aes(x=Month, y=mean, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1, color="forestgreen") +
  geom_line(color="forestgreen") +
  geom_point(color="forestgreen") +
  labs(y= "Average Shell Height (mm)", x = "Month") +
  labs(subtitle = "SL") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100, by = 20)) +
  theme_classic()

####TOTAL WEIGHT
TWdata=data.frame(cond %>%
                    select(Site, Month, Total.Weight))
TWdatasummary <- ddply(TWdata, c("Month", "Site"), summarise,
                       N    = length(Total.Weight),
                       mean = mean(Total.Weight),
                       sd   = sd(Total.Weight),
                       se   = sd / sqrt(N))
TWdatasummary

ggplot(TWdatasummary, aes(x=Month, y=mean, colour=Site, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.1) +
  geom_line() +
  geom_point() +
  labs(y= "Average Total Weight (g)", x = "Month") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,180), breaks = seq(0,180, by = 20)) +
  theme_classic() +
  scale_color_brewer(palette = "Dark2")

##TEST NORMALITY##
qqnorm(cond$Condition.Index)
qqPlot(cond$Condition.Index)
#skewed to the right

shapiro.test(cond$Condition.Index)
hist(cond$Condition.Index)
#not normal??

qqnorm(cond$SH)
qqPlot(cond$SH)

shapiro.test(cond$SH)
hist(cond$SH)
#not normal

shapiro.test(cond$Total.Weight)
hist(cond$Total.Weight)
#not normal

#TEST HOMOGENEITY OF VARIANCE
bartlett.test(Condition.Index~Site, data=cond)
#at least one sample has a sig diff variance

bartlett.test(SH~Site, data=cond)
#at least one sample has a sig diff variance
#variances are not equal - can't use kruskal

bartlett.test(Total.Weight~Site, data=cond)
#at least one sample has a sig diff variance
#variances are not equal - can't use kruskal

bartlett.test(Condition.Index~Month, data=cond)
#at least one sample has a sig diff variance
#variances are not equal - can't use kruskal

##testing for homoscedasticity
library(lmtest)
modelC=lm(Condition.Index ~ Site, data = cond)
bptest(modelC)
#there is not constant variance or homoscedasticity in residual
#heterscedasticity (unequal variance)


#DUNNTEST FOR CI
library(FSA)
CT = dunnTest(Condition.Index ~ Site,
              data = cond,
              method = "bh")
CT

CT = CT$res
CT
library(rcompanion)
cldList(comparison = CT$Comparison,
        p.value = CT$P.adj,
        threshold = 0.05)
#  Group Letter MonoLetter
#1    AB      a      a    
#2    CR      b       b   
#3    LW      c        c  
#4    LX      d         d 
#5    SL     ab      ab   
#6    TB      e          e

#DUNNTEST FOR SH
#library(FSA)
ST = dunnTest(SH ~ Site,
              data = cond,
              method = "bh")
ST

ST = ST$res
ST
#library(rcompanion)
cldList(comparison = ST$Comparison,
        p.value = ST$P.adj,
        threshold = 0.05)
#  Group Letter MonoLetter
#1    AB      a      a    
#2    CR      b       b   
#3    LW      c        c  
#4    LX      c        c  
#5    SL      d         d 
#6    TB      e          e

#BOXPLOT
condlabel = c("a", "b", "c", "d", "ab", "e")

cibox <- ggplot(avgcond, aes(x = Site, y = Condition.Index)) +
  geom_boxplot() +
  labs(y= "Condition Index", x = "Site") +
  scale_y_continuous(limits=c(0,7), breaks = seq(0,7, by = 2)) +
  annotate("text", x=1:6, y=6.5, label=condlabel) +
  theme_classic()
cibox


cishlabel = c("a", "b", "c", "c", "d", "e")

cishbox <- ggplot(avgsh, aes(x = Site, y = SH)) +
  geom_boxplot() +
  labs(y= "Shell Height (mm)", x = "Site") +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100, by = 20)) +
  annotate("text", x=1:6, y=99, label=cishlabel) +
  theme_classic()
cishbox


# 
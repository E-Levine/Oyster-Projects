wq=read.csv("WQ.csv", header = T, na.strings = "Z")
wq
colnames(wq)[colnames(wq)=="ï..Month"] <- "Month"
library(MASS)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(dplyr)
library(scales)
library(wesanderson)
library(fansi)

summary(wq)
wq
wq$Month = factor(wq$Month, levels = month.abb)
temp=aggregate(Temp ~ Month + Site, wq, mean)
sal=aggregate(Salinity ~ Month + Site, wq, mean)

AB=subset(wq, Site=="AB")
LX=subset(wq, Site=="LX")
SL=subset(wq, Site=="SL")
LW=subset(wq, Site=="LW")
TB=subset(wq, Site=="TB")
CR=subset(wq, Site=="CR")

levels(wq$Site)
levels(wq$Site) <- c("AB", "CR", "LW", "LX", "SL", "TB")
levels(wq$Site)

levels(temp$Site)
levels(temp$Site) <- c("AB", "CR", "LW", "LX", "SL", "TB")
levels(temp$Site)

levels(sal$Site)
levels(sal$Site) <- c("AB", "CR", "LW", "LX", "SL", "TB")
levels(sal$Site)

colors <- c("dodgerblue4", "chocolate", "mediumpurple2", "violetred", 
            "forestgreen", "goldenrod1")
names(colors) <- c("AB", "CR", "LW", "LX", "SL", "TB")
colors

#PLOT
##All sites
temperature = ggplot(temp, aes(Month, Temp, color=Site))
temperature + 
  stat_summary(fun = mean,
               geom = "point") +
  stat_summary(fun = mean,
               geom = "line",
               aes(group = Site)) +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,35), breaks = seq(0,35, by = 5)) +
  labs(y= "Average Temperature (°C)", x = "Month") +
  theme_classic2() + 
  scale_colour_manual(values = colors)

salinity = ggplot(sal, aes(Month, Salinity, color=Site))
salinity + 
  stat_summary(fun.y = mean,
               geom = "point") +
  stat_summary(fun.y = mean,
               geom = "line",
               aes(group = Site)) +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,35), breaks = seq(0,35, by = 5)) +
  labs(y= "Average Salinity", x = "Month") +
  theme_classic2() + 
  scale_color_manual(values = colors)

#break up by sites
means=data.frame(wq%>%group_by(Site, Month)%>%summarize(
  meantemp=mean(Temp),
  meansal=mean(Salinity)))

means%>%filter(Site=="TB")%>%ggplot(aes(x=Month)) +
  geom_line(aes(y=meantemp, colour="Temperature (°C)")) +
  geom_line(aes(y=meansal, colour="Salinity (ppt)")) + 
  geom_point(aes(y=meantemp, colour="Temperature (°C)"), shape = 18, size =2) +
  geom_point(aes(y=meansal, colour="Salinity (ppt)"), shape = 18, size =2) + 
  scale_x_continuous(breaks = seq(1,12, by = 1)) +
  scale_y_continuous(limits=c(0,35), breaks = seq(0,35, by = 5)) +
  ylab(NULL) +
  theme_classic2() +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.85, 0.15)) +
  theme(plot.title = element_text())

means%>%filter(Site=="ABS")%>%ggplot(aes(x=Month)) +
  geom_line(aes(y=meantemp, colour="Temperature (°C)")) +
  geom_line(aes(y=meansal, colour="Salinity (ppt)")) + 
  geom_point(aes(y=meantemp, colour="Temperature (°C)"), shape = 18, size =2) +
  geom_point(aes(y=meansal, colour="Salinity (ppt)"), shape = 18, size =2) + 
  scale_x_discrete(breaks = seq(1,12, by = 1)) +
  scale_y_continuous(limits=c(0,35), breaks = seq(0,35, by = 5)) +
  ylab(NULL) +
  theme_classic2() +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.85, 0.15))

means%>%filter(Site=="SL-C")%>%ggplot(aes(x=Month)) +
  geom_line(aes(y=meantemp, colour="Temperature (°C)")) +
  geom_line(aes(y=meansal, colour="Salinity (ppt)")) + 
  geom_point(aes(y=meantemp, colour="Temperature (°C)"), shape = 18, size =2) +
  geom_point(aes(y=meansal, colour="Salinity (ppt)"), shape = 18, size =2) + 
  scale_x_continuous(breaks = seq(1,12, by = 1)) +
  scale_y_continuous(limits=c(0,35), breaks = seq(0,35, by = 5)) +
  ylab(NULL) +
  theme_classic2() +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.89, 0.15))

means%>%filter(Site=="LX-N")%>%ggplot(aes(x=Month)) +
  geom_line(aes(y=meantemp, colour="Temperature (°C)")) +
  geom_line(aes(y=meansal, colour="Salinity (ppt)")) + 
  geom_point(aes(y=meantemp, colour="Temperature (°C)"), shape = 18, size =2) +
  geom_point(aes(y=meansal, colour="Salinity (ppt)"), shape = 18, size =2) + 
  scale_x_continuous(breaks = seq(1,12, by = 1)) +
  scale_y_continuous(limits=c(0,35), breaks = seq(0,35, by = 5)) +
  ylab(NULL) +
  theme_classic2() +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.88, 0.15))

means%>%filter(Site=="LW")%>%ggplot(aes(x=Month)) +
  geom_line(aes(y=meantemp, colour="Temperature (°C)")) +
  geom_line(aes(y=meansal, colour="Salinity (ppt)")) + 
  geom_point(aes(y=meantemp, colour="Temperature (°C)"), shape = 18, size =2) +
  geom_point(aes(y=meansal, colour="Salinity (ppt)"), shape = 18, size =2) + 
  scale_x_continuous(breaks = seq(1,12, by = 1)) +
  scale_y_continuous(limits=c(0,35), breaks = seq(0,35, by = 5)) +
  ylab(NULL) +
  theme_classic2() +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.85, 0.15))

means%>%filter(Site=="CR")%>%ggplot(aes(x=Month)) +
  geom_line(aes(y=meantemp, colour="Temperature (°C)")) +
  geom_line(aes(y=meansal, colour="Salinity (ppt)")) + 
  geom_point(aes(y=meantemp, colour="Temperature (°C)"), shape = 18, size =2) +
  geom_point(aes(y=meansal, colour="Salinity (ppt)"), shape = 18, size =2) + 
  scale_x_continuous(breaks = seq(1,12, by = 1)) +
  scale_y_continuous(limits=c(0,35), breaks = seq(0,35, by = 5)) +
  ylab(NULL) +
  theme_classic2() +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.85, 0.15))

#TEST NORMALITY
shapiro.test(wq$Temp)
#not normal

shapiro.test(wq$Salinity)
#not normal

#TEST HOMOGENEITY OF VARIANCE
bartlett.test(Temp~Month, data=wq)
#at least one sample has a sig diff variance
bartlett.test(Salinity~Month, data=wq)
#at least one sample has a sig diff variance

#stats
kruskal.test(wq$Temp ~ wq$Site)
#less than 0.05 therefore there are significant differences between sites
kruskal.test(wq$Salinity ~ wq$Site)
#less than 0.05 therefore there are significant differences between sites

pairwise.wilcox.test(wq$Temp, wq$Site, p.adjust.method = "bonferroni")
pairwise.wilcox.test(wq$Sal, wq$Site, p.adjust.method = "bonferroni")

#DUNNTEST
library(FSA)
TT = dunnTest(Temp ~ Site,
              data = wq,
              method = "bh")
TT

TT = TT$res
TT
library(rcompanion)
cldList(comparison = TT$Comparison,
        p.value = TT$P.adj,
        threshold = 0.05)
#  Group Letter MonoLetter
#1    AB      a         a 
#2    CR     ab         ab
#3    LW      b          b
#4    LX      b          b
#5    SL     ab         ab
#6    TB     ab         ab
WQlabel = c("a", "b", "b", "b", "ab", "ab")

c <- ggplot(wq, aes(x = Site, y = Temp)) +
  geom_boxplot() +
  labs(y= "Temperature (°C)", x = "Site") +
  scale_y_continuous(limits=c(0,40), breaks = seq(0,40, by = 10)) +
  annotate("text", x=1:6, y=37, label=WQlabel) +
  theme_classic()
c

library(FSA)
ST = dunnTest(Salinity ~ Site,
              data = wq,
              method = "bh")
ST

ST = ST$res
ST
library(rcompanion)
cldList(comparison = ST$Comparison,
        p.value = ST$P.adj,
        threshold = 0.05)
#  Group Letter MonoLetter
#1    AB     ab        ab 
#2    CR     ab        ab 
#3    LW      a        a  
#4    LX      b         b 
#5    SL      b         b 
#6    TB      c          c

Sallabel = c("ab", "ab", "a", "b", "b", "c")

d <- ggplot(wq, aes(x = Site, y = Salinity)) +
  geom_boxplot() +
  labs(y= "Salinity", x = "Site") +
  scale_y_continuous(limits=c(0,45), breaks = seq(0,45, by = 10)) +
  annotate("text", x=1:6, y=42, label=Sallabel) +
  theme_classic()
d

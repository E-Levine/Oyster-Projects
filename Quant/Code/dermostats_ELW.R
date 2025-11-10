dermo=read.csv("Data/Dermo.csv", header=T, na.strings="Z")
dermo

library(Hmisc)
library(plyr)
library(tidyverse)
library(ggpubr)

summary(dermo)
dermo$Month <- factor(dermo$Month)
dermo$Month = factor(dermo$Month, levels = month.abb)

AB=subset(dermo, Site=="AB")
LX=subset(dermo, Site=="LX")
SL=subset(dermo, Site=="SL")
LW=subset(dermo, Site=="LW")
TB=subset(dermo, Site=="TB")
CR=subset(dermo, Site=="CR")

colnames(dermo)[colnames(dermo)=="?..Site"] <- "Site"
colnames(dermo)[colnames(dermo)=="X"] <- "Month"
colnames(dermo)[colnames(dermo)=="Dermo.Gill"] <- "Gill"
colnames(dermo)[colnames(dermo)=="Dermo.Mantle"] <- "Mantle"
dermo

colors <- c("dodgerblue4", "chocolate", "mediumpurple2", "violetred", 
            "forestgreen", "goldenrod1")
names(colors) <- c("AB", "CR", "LW", "LX", "SL", "TB")
colors

###### plot 
height=data.frame(dermo %>%
                     select(Site, Month, SH))
heightdata <- ddply(height, c("Month", "Site"), summarise,
                     N    = length(SH),
                     mean = mean(SH),
                     sd   = sd(SH),
                     se   = sd / sqrt(N))
heightdata 

ggplot(data=heightdata, aes(x=Month, y=mean, fill=Site,)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=.2,    # Thinner lines
                width=.2,
                position=position_dodge(.93)) +
  labs(y= "Average Shell Height (mm)", x = "Month") +
  scale_x_discrete(limits = month.abb) +
  #scale_y_continuous(limits=c(0,5), breaks = seq(0,5, by = 1)) +
  scale_fill_manual(values = colors) +
  theme_classic2()

#dermo3 not found...
ggline(dermo3,x="Month",y="SH",color="Site", xlab="Month", 
       ylab="Average Shell Height (mm)") + 
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100, by = 10)) +
  theme_classic() +
  scale_color_brewer(palette = "Dark2")


###INTENSITY FIGURES
intense=data.frame(dermo %>%
  select(Site, Month, Intensity))
intensedata <- ddply(intense, c("Month", "Site"), summarise,
               N    = length(Intensity),
               mean = mean(Intensity, na.rm=TRUE),
               sd   = sd(Intensity, na.rm=TRUE),
               se   = sd / sqrt(N))
intensedata

ggplot(data=intensedata, aes(x=Month, y=mean, fill=Site,)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=.2,    # Thinner lines
                width=.2,
                position=position_dodge(.93)) +
  labs(y= "Average Intensity of Infection", x = "Month") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,5), breaks = seq(0,5, by = 1)) +
  scale_fill_manual(values = colors) +
  theme_classic2()

intensedata%>%filter(Site=="TB")%>%
  ggplot() + geom_bar(aes(x=Month, y=mean), 
                      stat="identity", 
                      fill="goldenrod1",
                      position=position_dodge()) +
  geom_errorbar(aes(x=Month, ymin=mean-se, ymax=mean+se),
                size=.2,    # Thinner lines
                width=.2,
                position=position_dodge(.93)) +
  labs(y= "Average Intensity of Infection", x = "Month") +
  labs(title = "Tampa Bay") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,5), breaks = seq(0,5, by = 1)) +
  theme_classic2()

intensedata%>%filter(Site=="AB")%>%
  ggplot() + geom_bar(aes(x=Month, y=mean), 
                      stat="identity", 
                      fill="dodgerblue4",
                      position=position_dodge()) +
  geom_errorbar(aes(x=Month, ymin=mean-se, ymax=mean+se),
                size=.2,    # Thinner lines
                width=.2,
                position=position_dodge(.93)) +
  labs(y= "Average Intensity of Infection", x = "Month") +
  labs(title = "Apalachicola Bay") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,5), breaks = seq(0,5, by = 1)) +
  theme_classic2()  

intensedata%>%filter(Site=="LW")%>%
  ggplot() + geom_bar(aes(x=Month, y=mean), 
                      stat="identity", 
                      fill="mediumpurple2",
                      position=position_dodge()) +
  geom_errorbar(aes(x=Month, ymin=mean-se, ymax=mean+se),
                size=.2,    # Thinner lines
                width=.2,
                position=position_dodge(.93)) +
  labs(y= "Average Intensity of Infection", x = "Month") +
  labs(title = "Lake Worth") +
  scale_x_discrete(limits = month.abb) + 
  scale_y_continuous(limits=c(0,5), breaks = seq(0,5, by = 1)) +
  theme_classic2()

intensedata%>%filter(Site=="LX")%>%
  ggplot() + geom_bar(aes(x=Month, y=mean), 
                      stat="identity", 
                      fill="violetred",
                      position=position_dodge()) +
  geom_errorbar(aes(x=Month, ymin=mean-se, ymax=mean+se),
                size=.2,    # Thinner lines
                width=.2,
                position=position_dodge(.93)) +
  labs(y= "Average Intensity of Infection", x = "Month") +
  labs(title = "Loxahatchee") +
  scale_x_discrete(limits = month.abb) + 
  scale_y_continuous(limits=c(0,5), breaks = seq(0,5, by = 1)) +
  theme_classic2()

intensedata%>%filter(Site=="CR")%>%
  ggplot() + geom_bar(aes(x=Month, y=mean), 
                      stat="identity", 
                      fill="chocolate",
                      position=position_dodge()) +
  geom_errorbar(aes(x=Month, ymin=mean-se, ymax=mean+se),
                size=.2,    # Thinner lines
                width=.2,
                position=position_dodge(.93)) +
  labs(y= "Average Intensity of Infection", x = "Month") +
  labs(title = "Caloosahatchee") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,5), breaks = seq(0,5, by = 1)) +
  theme_classic2()

intensedata%>%filter(Site=="SL")%>%
  ggplot() + geom_bar(aes(x=Month, y=mean), 
                      stat="identity", 
                      fill="forestgreen",
                      position=position_dodge()) +
  geom_errorbar(aes(x=Month, ymin=mean-se, ymax=mean+se),
                size=.2,    # Thinner lines
                width=.2,
                position=position_dodge(.93)) +
  labs(y= "Average Intensity of Infection", x = "Month") +
  labs(title = "St. Lucie") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,5), breaks = seq(0,5, by = 1)) +
  theme_classic2()

intensemean=data.frame(aggregate(Intensity ~ Month + Site, dermo, mean))
intensemean %>% pivot_wider(names_from = "Site", values_from = "Intensity") %>%
  mutate(Month = month.name[match(Month, month.abb)]) %>%
  arrange(factor(Month, levels = month.name))

#Prevalence
presencemean=data.frame(aggregate(PresenceBoth ~ Month + Site, dermo, mean))

###PREVALENCE BY SITE
presencemean%>%filter(Site=="TB")%>%
  ggplot()+geom_bar(aes(x=Month, y=PresenceBoth), stat="identity", 
                    fill="goldenrod1") +
  labs(y= "Prevalence of Dermo", x = "Month") +
  labs(title = "Tampa Bay") +
  scale_x_discrete(limits = month.abb) + 
  scale_y_continuous(limits=c(0,1), breaks = seq(0,1, by = .25)) +
  theme_classic2()

presencemean%>%filter(Site=="AB")%>%
  ggplot()+geom_bar(aes(x=Month, y=PresenceBoth), stat="identity", 
                    fill="dodgerblue4") +
  labs(y= "Prevalence of Dermo", x = "Month") +
  labs(title = "Apalachicola Bay") +
  scale_x_discrete(limits = month.abb) + 
  scale_y_continuous(limits=c(0,1), breaks = seq(0,1, by = .25)) +
  theme_classic2() 

presencemean%>%filter(Site=="LW")%>%
  ggplot()+geom_bar(aes(x=Month, y=PresenceBoth), stat="identity", 
                    fill="mediumpurple2") +
  labs(y= "Prevalence of Dermo", x = "Month") +
  labs(title = "Lake Worth") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits=c(0,1), breaks = seq(0,1, by = .25)) +
  theme_classic2() 

presencemean%>%filter(Site=="LX")%>%
  ggplot()+geom_bar(aes(x=Month, y=PresenceBoth), stat="identity", 
                    fill="violetred") +
  labs(y= "Prevalence of Dermo", x = "Month") +
  labs(title = "Loxahatchee") +
  scale_x_discrete(limits = month.abb) + 
  scale_y_continuous(limits=c(0,1), breaks = seq(0,1, by = .25)) +
  theme_classic2() 

presencemean%>%filter(Site=="CR")%>%
  ggplot()+geom_bar(aes(x=Month, y=PresenceBoth), stat="identity", 
                    fill="chocolate") +
  labs(y= "Prevalence of Dermo", x = "Month") +
  labs(title = "Caloosahatchee") +
  scale_x_discrete(limits = month.abb) + 
  scale_y_continuous(limits=c(0,1), breaks = seq(0,1, by = .25)) +
  theme_classic2() 

presencemean%>%filter(Site=="SL")%>%
  ggplot()+geom_bar(aes(x=Month, y=PresenceBoth), stat="identity",
                    fill="forestgreen") +
  labs(y= "Prevalence of Dermo", x = "Month") +
  labs(title = "St. Lucie") +
  scale_x_discrete(limits = month.abb) + 
  scale_y_continuous(limits=c(0,1), breaks = seq(0,1, by = .25)) +
  theme_classic2() 

ggplot(data=presencemean, aes(x=Month, y=PresenceBoth, fill=Site,)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y= "Prevalence of Dermo", x = "Month") +
  scale_x_discrete(limits = month.abb) + 
  scale_fill_manual(values = colors) +
  theme_classic2()
#the better prevalence figure!!!

##TEST NORMALITY##
shapiro.test(dermo$PresenceBoth)
hist(dermo$PresenceBoth)
#not normal

shapiro.test(dermo$SH)
hist(dermo$SH)
#not normal

shapiro.test(dermo$TotalWeight)
hist(dermo$TotalWeight)
#not normal

#TEST HOMOGENEITY OF VARIANCE
library(car)
leveneTest(dermo$PresenceBoth, dermo$Site, center = mean)


bartlett.test(PresenceBoth~Site, data=dermo)
#at least one sample has a sig diff variance
bartlett.test(SH~Site, data=dermo)
#at least one sample has a sig diff variance
bartlett.test(Intensity~Site, data=dermo)
#at least one sample has a sig diff variance


#DUNNTEST DERMO PRESENCE
library(FSA)
DT = dunnTest(PresenceBoth ~ Site,
              data = dermo,
              method = "bh")
DT

DT = DT$res
DT
library(rcompanion)
cldList(comparison = DT$Comparison,
        p.value = DT$P.adj,
        threshold = 0.05)

# Group Letter MonoLetter
#1    AB      a       a   
#2    CR      b        b  
#3    LW      c         c 
#4    LX      b        b  
#5    SL      d          d
#6    TB      a       a   

#DUNNTEST DERMO INTENSITY
#library(FSA)
IT = dunnTest(Intensity ~ Site,
              data = dermo,
              method = "bh")
IT

IT = IT$res
IT
#library(rcompanion)
cldList(comparison = IT$Comparison,
        p.value = IT$P.adj,
        threshold = 0.05)
#  Group Letter MonoLetter
#1    AB      a       a   
#2    CR      b        b  
#3    LW      c         c 
#4    LX      b        b  
#5    SL      d          d
#6    TB      a       a   

#BOXPLOT
Intlabel = c("a", "b", "c", "b", "d", "a")

int <- ggplot(intensemean, aes(x = Site, y = Intensity)) +
  geom_boxplot() +
  labs(y= "Dermo Intensity", x = "Site") +
  scale_y_continuous(limits=c(0,3), breaks = seq(0,3, by = 1)) +
  annotate("text", x=1:6, y=2.70, label=Intlabel) +
  theme_classic()
int

Preslabel = c("a", "b", "c", "b", "d", "a")

pres <- ggplot(presencemean, aes(x = Site, y = PresenceBoth)) +
  geom_boxplot() +
  labs(y= "Dermo Prevalence", x = "Site") +
  scale_y_continuous(limits=c(0,1.3), breaks = seq(0,1.3, by = 0.5)) +
  annotate("text", x=1:6, y=1.2, label=Preslabel) +
  theme_classic()
pres

#PAIRWISE WILCOX
pairwise.wilcox.test(dermo$SH, dermo$Site, p.adjust.method = "BH")
pairwise.wilcox.test(dermo$PresenceBoth, dermo$Site, p.adjust.method = "BH")
pairwise.wilcox.test(dermo$Intensity, dermo$Site, p.adjust.method = "BH")


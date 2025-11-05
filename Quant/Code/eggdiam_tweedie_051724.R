#### Load packages
library(tidyverse)
library(RColorBrewer)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(AICcmodavg)

#### Read in repro data
repro <- read.csv("Repro.csv", header=T, na.strings="Z")

#### Create variable SiteMonth to use as grouping variable for random intercept
repro$SiteMonth <- interaction(repro$Site, repro$Month) %>% droplevels(.)

#### Subset data and filter for females since we're modeling egg diameter
repro <- repro %>% dplyr::select(Site, Height, Length, Total, Sex, Stage, OocyteDiam, Parasite, Month, Temp, Sal, Season, SiteMonth) %>% filter(Sex=="F") %>% filter(complete.cases(.))

#### These models don't fit well when fitting data for all sites together, so I've broken it down by site here unlike the condition index and dermo models

#### Filter for site AB, fit a tweedie regression, and check goodness of fit
reproAB <- repro %>% filter(Site == "AB")
mAB1 <- glmmTMB(OocyteDiam ~ Season*Length, dispformula = ~Season*Length, family = tweedie(link = "log"), data = reproAB)
simulateResiduals(mAB1, n = 1000, plot = T)

#### Now do the same for CR
reproCR <- repro %>% filter(Site == "CR")
mCR1 <- glmmTMB(OocyteDiam ~ Season*Length, dispformula = ~Season*Length, family = tweedie(link = "log"), data = reproCR)
simulateResiduals(mCR1, n = 1000, plot = T)

#### Now do the same for LW
reproLW <- repro %>% filter(Site == "LW")
mLW1 <- glmmTMB(OocyteDiam ~ Season*Length, dispformula = ~Season*Length, family = tweedie(link = "log"), data = reproLW)
simulateResiduals(mLW1, n = 1000, plot = T)

#### Now do the same for LX
reproLX <- repro %>% filter(Site == "LX")
mLX1 <- glmmTMB(OocyteDiam ~ Season*Length, dispformula = ~Season*Length, family = tweedie(link = "log"), data = reproLX)
simulateResiduals(mLX1, n = 1000, plot = T)

#### Now do the same for SL
reproSL <- repro %>% filter(Site == "SL")
mSL1 <- glmmTMB(OocyteDiam ~ Season*Length, dispformula = ~Season*Length, family = tweedie(link = "log"), data = reproSL)
simulateResiduals(mSL1, n = 1000, plot = T)

#### Now do the same for TB
reproTB <- repro %>% filter(Site == "TB")
mTB1 <- glmmTMB(OocyteDiam ~ Season*Length, dispformula = ~Season*Length, family = tweedie(link = "log"), data = reproTB)
simulateResiduals(mTB1, n = 1000, plot = T)

#### Predictions for AB (model 1 is best)
newDatAB <- expand.grid(Season = unique(reproAB$Season), Length = seq(min(reproAB$Length), max(reproAB$Length), length.out = 100), SiteStationMonth = NA)
predsAB <- predict(mAB1, newdata = newDatAB, type = "link", se.fit = T)
preddsAB <- data.frame(newDatAB, fit = predsAB$fit, se = predsAB$se.fit)
preddsAB$mean <- exp(preddsAB$fit)
preddsAB$lwr <- exp(preddsAB$fit - 1.96*preddsAB$se)
preddsAB$upr <- exp(preddsAB$fit + 1.96*preddsAB$se)
preddsAB$Season <- factor(preddsAB$Season, levels = c("Dry", "Wet"))

## Just checking the range of Lengths in the Wet and Dry seasons and they're in the same ballpark so no need to subset the predictions for the figure 
range(reproAB$Length[reproAB$Site=="AB" & reproAB$Season=="Dry"])
range(reproAB$Length[reproAB$Site=="AB" & reproAB$Season=="Wet"])

(EDPlot_AB <- ggplot(preddsAB, aes(x = Length, y = mean, group = Season, color = Season)) + geom_line(stat = "identity") + geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25, color = NA) + theme_bw() + scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10), expand = expansion(add = c(0,0))) + scale_x_continuous(limits = c(4,24), breaks = seq(4,24,2), expand = expansion(add = c(0,0))) + labs(x = "Length (cm)", y = "Mean egg diameter (µm)") + theme(axis.text = element_text(color = "black"), panel.grid.major.y = element_line(color = "grey90"), panel.grid.minor.y = element_line(color = "grey90", linetype = "dashed")) + theme(legend.position = "bottom"))
ggsave("EDPlot_AB.png", EDPlot_AB, dpi = 600, height = 6, width = 8)

#### Predictions for CR (model 1 is best)
newDatCR <- expand.grid(Season = unique(reproCR$Season), Length = seq(min(reproCR$Length), max(reproCR$Length), length.out = 100), SiteStationMonth = NA)
predsCR <- predict(mCR1, newdata = newDatCR, type = "link", se.fit = T)
preddsCR <- data.frame(newDatCR, fit = predsCR$fit, se = predsCR$se.fit)
preddsCR$mean <- exp(preddsCR$fit)
preddsCR$lwr <- exp(preddsCR$fit - 1.96*preddsCR$se)
preddsCR$upr <- exp(preddsCR$fit + 1.96*preddsCR$se)
preddsCR$Season <- factor(preddsCR$Season, levels = c("Dry", "Wet"))

## Just checking the range of Lengths in the Wet and Dry seasons and they're in the same ballpark so no need to subset the predictions for the figure 
range(reproCR$Length[reproCR$Site=="CR" & reproCR$Season=="Dry"])
range(reproCR$Length[reproCR$Site=="CR" & reproCR$Season=="Wet"])

(EDPlot_CR <- ggplot(preddsCR, aes(x = Length, y = mean, group = Season, color = Season)) + geom_line(stat = "identity") + geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25, color = NA) + theme_bw() + scale_y_continuous(limits = c(0,80), breaks = seq(0,80,10), expand = expansion(add = c(0,0))) + scale_x_continuous(limits = c(3,12), breaks = seq(3,12,1), expand = expansion(add = c(0,0))) + labs(x = "Length (cm)", y = "Mean egg diameter (µm)") + theme(axis.text = element_text(color = "black"), panel.grid.major.y = element_line(color = "grey90"), panel.grid.minor.y = element_line(color = "grey90", linetype = "dashed")) + theme(legend.position = "bottom"))
ggsave("EDPlot_CR.png", EDPlot_CR, dpi = 600, height = 6, width = 8)

#### Predictions for LW 
newDatLW <- expand.grid(Season = unique(reproLW$Season), Length = seq(min(reproLW$Length), max(reproLW$Length), length.out = 100), SiteStationMonth = NA)
predsLW <- predict(mLW1, newdata = newDatLW, type = "link", se.fit = T)
preddsLW <- data.frame(newDatLW, fit = predsLW$fit, se = predsLW$se.fit)
preddsLW$mean <- exp(preddsLW$fit)
preddsLW$lwr <- exp(preddsLW$fit - 1.96*preddsLW$se)
preddsLW$upr <- exp(preddsLW$fit + 1.96*preddsLW$se)
preddsLW$Season <- factor(preddsLW$Season, levels = c("Dry", "Wet"))

## Just checking the range of Lengths in the Wet and Dry seasons and they're in the same ballpark so no need to subset the predictions for the figure 
range(reproLW$Length[reproLW$Site=="LW" & reproLW$Season=="Dry"])
range(reproLW$Length[reproLW$Site=="LW" & reproLW$Season=="Wet"])

(EDPlot_LW <- ggplot(preddsLW, aes(x = Length, y = mean, group = Season, color = Season)) + geom_line(stat = "identity") + geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25, color = NA) + theme_bw() + scale_y_continuous(limits = c(0,40), breaks = seq(0,40,5), expand = expansion(add = c(0,0))) + scale_x_continuous(limits = c(4,14), breaks = seq(4,14,1), expand = expansion(add = c(0,0))) + labs(x = "Length (cm)", y = "Mean egg diameter (µm)") + theme(axis.text = element_text(color = "black"), panel.grid.major.y = element_line(color = "grey90"), panel.grid.minor.y = element_line(color = "grey90", linetype = "dashed")) + theme(legend.position = "bottom"))
ggsave("EDPlot_LW.png", EDPlot_LW, dpi = 600, height = 6, width = 8)

#### Predictions for LX 
newDatLX <- expand.grid(Season = unique(reproLX$Season), Length = seq(min(reproLX$Length), max(reproLX$Length), length.out = 100), SiteStationMonth = NA)
predsLX <- predict(mLX1, newdata = newDatLX, type = "link", se.fit = T)
preddsLX <- data.frame(newDatLX, fit = predsLX$fit, se = predsLX$se.fit)
preddsLX$mean <- exp(preddsLX$fit)
preddsLX$LXr <- exp(preddsLX$fit - 1.96*preddsLX$se)
preddsLX$upr <- exp(preddsLX$fit + 1.96*preddsLX$se)
preddsLX$Season <- factor(preddsLX$Season, levels = c("Dry", "Wet"))

## Just checking the range of Lengths in the Wet and Dry seasons and they're in the same ballpark so no need to subset the predictions for the figure 
range(reproLX$Length[reproLX$Site=="LX" & reproLX$Season=="Dry"])
range(reproLX$Length[reproLX$Site=="LX" & reproLX$Season=="Wet"])

(EDPlot_LX <- ggplot(preddsLX, aes(x = Length, y = mean, group = Season, color = Season)) + geom_line(stat = "identity") + geom_ribbon(aes(ymin = LXr, ymax = upr), alpha = 0.25, color = NA) + theme_bw() + scale_y_continuous(limits = c(0,45), breaks = seq(0,45,5), expand = expansion(add = c(0,0))) + scale_x_continuous(limits = c(4,14), breaks = seq(4,14,1), expand = expansion(add = c(0,0))) + labs(x = "Length (cm)", y = "Mean egg diameter (µm)") + theme(axis.text = element_text(color = "black"), panel.grid.major.y = element_line(color = "grey90"), panel.grid.minor.y = element_line(color = "grey90", linetype = "dashed")) + theme(legend.position = "bottom"))
ggsave("EDPlot_LX.png", EDPlot_LX, dpi = 600, height = 6, width = 8)

#### Predictions for SL 
newDatSL <- expand.grid(Season = unique(reproSL$Season), Length = seq(min(reproSL$Length), max(reproSL$Length), length.out = 100), SiteStationMonth = NA)
predsSL <- predict(mSL1, newdata = newDatSL, type = "link", se.fit = T)
preddsSL <- data.frame(newDatSL, fit = predsSL$fit, se = predsSL$se.fit)
preddsSL$mean <- exp(preddsSL$fit)
preddsSL$SLr <- exp(preddsSL$fit - 1.96*preddsSL$se)
preddsSL$upr <- exp(preddsSL$fit + 1.96*preddsSL$se)
preddsSL$Season <- factor(preddsSL$Season, levels = c("Dry", "Wet"))

## Just checking the range of Lengths in the Wet and Dry seasons and they're in the same ballpark so no need to subset the predictions for the figure 
range(reproSL$Length[reproSL$Site=="SL" & reproSL$Season=="Dry"])
range(reproSL$Length[reproSL$Site=="SL" & reproSL$Season=="Wet"])

(EDPlot_SL <- ggplot(preddsSL, aes(x = Length, y = mean, group = Season, color = Season)) + geom_line(stat = "identity") + geom_ribbon(aes(ymin = SLr, ymax = upr), alpha = 0.25, color = NA) + theme_bw() + scale_y_continuous(limits = c(0,50), breaks = seq(0,50,5), expand = expansion(add = c(0,0))) + scale_x_continuous(limits = c(3,9), breaks = seq(3,9,1), expand = expansion(add = c(0,0))) + labs(x = "Length (cm)", y = "Mean egg diameter (µm)") + theme(axis.text = element_text(color = "black"), panel.grid.major.y = element_line(color = "grey90"), panel.grid.minor.y = element_line(color = "grey90", linetype = "dashed")) + theme(legend.position = "bottom"))
ggsave("EDPlot_SL.png", EDPlot_SL, dpi = 600, height = 6, width = 8)

#### Predictions for TB
newDatTB <- expand.grid(Season = unique(reproTB$Season), Length = seq(min(reproTB$Length), max(reproTB$Length), length.out = 100), SiteStationMonth = NA)
predsTB <- predict(mTB1, newdata = newDatTB, type = "link", se.fit = T)
preddsTB <- data.frame(newDatTB, fit = predsTB$fit, se = predsTB$se.fit)
preddsTB$mean <- exp(preddsTB$fit)
preddsTB$TBr <- exp(preddsTB$fit - 1.96*preddsTB$se)
preddsTB$upr <- exp(preddsTB$fit + 1.96*preddsTB$se)
preddsTB$Season <- factor(preddsTB$Season, levels = c("Dry", "Wet"))

## Just checking the range of Lengths in the Wet and Dry seasons and they're in the same ballpark so no need to subset the predictions for the figure 
range(reproTB$Length[reproTB$Site=="TB" & reproTB$Season=="Dry"])
range(reproTB$Length[reproTB$Site=="TB" & reproTB$Season=="Wet"])

(EDPlot_TB <- ggplot(preddsTB, aes(x = Length, y = mean, group = Season, color = Season)) + geom_line(stat = "identity") + geom_ribbon(aes(ymin = TBr, ymax = upr), alpha = 0.25, color = NA) + theme_bw() + scale_y_continuous(limits = c(0,45), breaks = seq(0,45,5), expand = expansion(add = c(0,0))) + scale_x_continuous(limits = c(4,16), breaks = seq(4,16,1), expand = expansion(add = c(0,0))) + labs(x = "Length (cm)", y = "Mean egg diameter (µm)") + theme(axis.text = element_text(color = "black"), panel.grid.major.y = element_line(color = "grey90"), panel.grid.minor.y = element_line(color = "grey90", linetype = "dashed")) + theme(legend.position = "bottom"))
ggsave("EDPlot_TB.png", EDPlot_TB, dpi = 600, height = 6, width = 8)
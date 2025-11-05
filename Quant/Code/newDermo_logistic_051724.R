# ---
# title: "Dermo Presence"
# author: "Colin Shea (FWC/FWRI ~ colin.shea@myfwc.com)"
# date: "`r Sys.Date()`"
# output: html_document
# ---
  
#### Load packages
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(rms)
library(emmeans)
library(AICcmodavg)

#### Read in dermo data
dermo <- read.csv("Dermo.csv", header=T, na.strings = "Z")

#### Create a variable "PB" (Presence Both) = 1 if dermo present on gill AND mantle and 0 otherwise
dermo$PB <- ifelse(dermo$Presence.Gill==1 & dermo$Presence.Mantle==1, 1, 0)

#### Note that dermo was never observed at SL so the estimates will be weird for that site (doesn't affect the other sites' estimates)
table(dermo$PB, dermo$Site)

#### Create variable SiteMonth to use as a grouping variable for a random intercept
dermo$SiteMonth <- interaction(dermo$Site, dermo$Month)

#### Fit a few alternative logistic regression models with SiteMonth as a random effect
mD1 <- glmmTMB(PB ~ Season*Site + (1|SiteMonth), family = binomial, data = dermo)

#### Goodness of fit for the top 3 models (they all look OK)
simulateResiduals(mD1, n = 1000, plot = T)

#### Predictive performance (Higher AUC = better and lower Brier = better; these all look fine)
val.prob(p = predict(mD1, type="response"), y = dermo$PB, smooth = FALSE)[c("Brier", "C (ROC)")]

#### Marginal means and post-hoc contrasts with false discovery rate p-value adjustment. Here, we are just comparing among seasons separately for each site. Note that any means and contrasts involving SL will look weird (it's OK though)
emmeans(mD1, pairwise ~ Season, by = "Site", type = "link")$emmeans %>% summary(by = NULL, adjust = "none")
emmeans(mD1, pairwise ~ Season, by = "Site", type = "link")$contrasts %>% summary(by = NULL, adjust = "fdr")

#### Make some predictions from the top model to new data
newDat <- expand.grid(Site = unique(dermo$Site), Season = unique(dermo$Season), Date = NA, SiteMonth = NA)
preds <- predict(mD1, newdata = newDat, type = "link", se.fit = T)
predds <- data.frame(newDat, fit = preds$fit, se = preds$se.fit)
predds$mean <- plogis(predds$fit)
predds$lwr <- plogis(predds$fit - 1.96*predds$se)
predds$upr <- plogis(predds$fit + 1.96*predds$se)
predds$Site <- factor(predds$Site, levels = c("AB", "CR", "LX", "LW", "SL", "TB"))
pd <- position_dodge(width = 0.9, preserve = "total")

#### For plotting purposes setting upper and lower 95% CLs to 0
predds$upr[predds$Site=="SL"] <- 0
predds$lwr[predds$Site=="SL"] <- 0

#### Plot the predicted means along with 95% confidence intervals
(dermoProbPlot <- ggplot(predds, aes(x = Season, y = mean, group = Site, color = Site, fill = Site)) + geom_bar(stat = "identity", position = pd, width = 0.9, color = "black") + geom_errorbar(aes(ymin = lwr, ymax = upr), position = pd, color = "black", width = 0.3) + theme_bw() + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1), expand = expansion(add = c(0,0))) + labs(x = "Season", y = "Mean probability dermo on both mantle and gill") + theme(axis.text = element_text(color = "black"), panel.grid.major.y = element_line(color = "grey90"), panel.grid.minor.y = element_line(color = "grey90", linetype = "dashed")) + theme(legend.position = "bottom") + scale_fill_brewer(palette = "Dark2"))
ggsave("dermoProbPlot.png", dermoProbPlot, dpi = 600, height = 6, width = 8)

#### Plot temperature and salinity by site and season
dermoPlot <- dermo %>% mutate(Site = factor(Site, levels = c("AB", "CR", "LX", "LW", "SL", "TB")), Season = factor(Season, levels = c("Dry", "Wet")))

#### First temperature
(temperaturePlot <- ggplot(dermoPlot, aes(x = Season, y = Temp, group = interaction(Site, Season), fill = Site)) + geom_boxplot(position = pd) + theme_bw() + scale_y_continuous(limits = c(12,34), breaks = seq(12,34,2), expand = expansion(add = c(0,0))) + labs(x = "Season", y = "Temperature (C)") + theme(axis.text = element_text(color = "black"), panel.grid.major.y = element_line(color = "grey90"), panel.grid.minor.y = element_line(color = "grey90", linetype = "dashed")) + theme(legend.position = "bottom") + scale_fill_brewer(palette = "Dark2"))
ggsave("temperaturePlot.png", temperaturePlot, dpi = 600, height = 6, width = 8)

#### Then salinity
(salinityPlot <- ggplot(dermoPlot, aes(x = Season, y = Sal, group = interaction(Site, Season), fill = Site)) + geom_boxplot(position = pd) + theme_bw() + scale_y_continuous(limits = c(0,36), breaks = seq(0,36,2), expand = expansion(add = c(0,0))) + labs(x = "Season", y = "Salinity (PPT)") + theme(axis.text = element_text(color = "black"), panel.grid.major.y = element_line(color = "grey90"), panel.grid.minor.y = element_line(color = "grey90", linetype = "dashed")) + theme(legend.position = "bottom") + scale_fill_brewer(palette = "Dark2"))
ggsave("salinityPlot.png", salinityPlot, dpi = 600, height = 6, width = 8)
# ---
# title: "Condition Index"
# author: "Colin Shea (FWC/FWRI ~ colin.shea@myfwc.com)"
# date: "`r Sys.Date()`"
# output: html_document
# ---
  
#### Load packages
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(AICcmodavg)

#### Read in condition index data
cond <- read.csv("Condition.csv", header=T, na.strings="Z")

#### Create a variable SiteStationMonth to use as grouping variable for a random intercept
cond$SiteStationMonth <- interaction(cond$Site, cond$Station, cond$Month) %>% droplevels(.)

#### Model condition index as a function of site and season. Note inclusion of Site*Season in dispersion model to account for differing variances among groups
mCI1 <- glmmTMB(Condition.Index ~ Site*Season + (1|SiteStationMonth), dispformula = ~Site*Season, family = Gamma(link = "log"), data = cond)

#### Goodness of fit for the top model (looks OK)
simulateResiduals(mCI1, n = 1000, plot = T)

#### Marginal means and post-hoc contrasts with false discovery rate p-value adjustment. Here, we are just comparing among seasons separately for each site. 
emmeans(mCI1, pairwise ~ Season, by = "Site", type = "link")$emmeans %>% summary(by = NULL, adjust = "none")
emmeans(mCI1, pairwise ~ Season, by = "Site", type = "link")$contrasts %>% summary(by = NULL, adjust = "fdr")

#### New data set for making predictions and plotting them
newDatCI <- expand.grid(Site = unique(cond$Site), Season = unique(cond$Season), SiteStationMonth = NA)
predsCI <- predict(mCI1, newdata = newDatCI, type = "link", se.fit = T)
preddsCI <- data.frame(newDatCI, fit = predsCI$fit, se = predsCI$se.fit)
preddsCI$mean <- exp(preddsCI$fit)
preddsCI$lwr <- exp(preddsCI$fit - 1.96*preddsCI$se)
preddsCI$upr <- exp(preddsCI$fit + 1.96*preddsCI$se)
preddsCI$Site <- factor(preddsCI$Site, levels = c("AB", "CR", "LX", "LW", "SL", "TB"))
preddsCI$Season <- factor(preddsCI$Season, levels = c("Dry", "Wet"))

#### Plot predicted CIs for each site and season
(CIPlot <- ggplot(preddsCI, aes(x = Site, y = mean, fill = Site)) + geom_bar(stat = "identity", color = "black") + geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) + theme_bw() + scale_y_continuous(limits = c(0,5), breaks = seq(0,5,0.25), expand = expansion(add = c(0,0))) + scale_x_discrete(expand = expansion(add = c(0.5,0.5))) + labs(x = NULL, y = "Mean condition") + theme(axis.text = element_text(color = "black"), panel.grid.major.y = element_line(color = "grey90"), panel.grid.minor.y = element_line(color = "grey90", linetype = "dashed")) + facet_wrap(~Season) + theme(legend.position = "none") + scale_fill_brewer(palette = "Dark2"))
ggsave("CIPlot.png", CIPlot, dpi = 600, height = 6, width = 8)
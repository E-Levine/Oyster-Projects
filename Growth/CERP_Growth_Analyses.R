##CERP Growth Comparisons
#
#CERP Cage Data 2015-current
#
#
#Load packages, install as needed
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, 
               ggpubr, scales,
               rstatix, #Summary stats
               zoo, lubridate, forecast, #Dates and times
               readxl, #Reading excel files
               car, emmeans, multcomp, #Basic analyses
               lmPerm,
               install = TRUE)
#
#
#
#
####Load Files####
#Reading in Excel files, adding station information to dfs.
#
##Station information
Locations_raw <- read_excel("Growth_database_2024_09_25.xlsx", sheet = "FixedLocations", #File name and sheet name
                          skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                          na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                          .name_repair = "unique")
head(Locations_raw)
(Locations <- Locations_raw %>% mutate(Site = as.factor(paste0(Estuary, SectionName))))
#
###Water quality
Cage_WQ_raw <- read_excel("Growth_database_2024_09_25.xlsx", sheet = "SampleEventWQ", #File name and sheet name
                     skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                     na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                     .name_repair = "unique")
#Check data and column names
head(Cage_WQ_raw)
#Remove unneeded columns and add in station info
(Cage_WQ <- Cage_WQ_raw %>% 
    dplyr::select(SampleEventWQID:DissolvedOxygen, PercentDissolvedOxygen, pH:TurbidityYSI, CollectionTime, Comments) %>%
    mutate(MonYr = as.yearmon(as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d")),
           FixedLocationID = substring(SampleEventID, 19, 22)) %>%
    left_join(Locations))
#
###Cage Counts
Cage_counts_raw <- read_excel("Growth_database_2024_09_25.xlsx", sheet = "CageCount_Dead", #File name and sheet name
                          skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                          na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                          .name_repair = "unique")
head(Cage_counts_raw)
(Cage_counts <- Cage_counts_raw %>% dplyr::select(CageCountID, CageColor, DataType, TotalCount) %>%
    #Get deployed and retrieved counts
    mutate(CageCountID = substr(CageCountID, 1, 22),
           MonYr = as.yearmon(as.Date(substring(CageCountID, 8, 15), format = "%Y%m%d")),
           FixedLocationID = substring(CageCountID, 19, 22)) %>% 
    spread(DataType, TotalCount) %>% rename(DepCount = Deployed, LiveCount = Retrieved) %>%
    #Determine dead counts per cage
    left_join(Cage_counts_raw %>% dplyr::select(CageCountID, CageColor, DataType, Dead) %>%
                mutate(CageCountID = substr(CageCountID, 1, 22)) %>% 
                spread(DataType, Dead) %>% rename("DeadCount" = Retrieved) %>%
                dplyr::select(-Deployed)) %>%
    mutate(RetTotal = LiveCount + DeadCount,
           MissCount = DepCount - RetTotal,
           Pct_Dead = 100-((LiveCount/DepCount)*100),
           Pct_DeadCounts = DeadCount/DepCount*100) %>% left_join(Locations))
#
###Cage SHS
Cage_SH_raw <- read_excel("Growth_database_2024_09_25.xlsx", sheet = "CageSH", #File name and sheet name
                      skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                      na = c("", "Z", "z", "NA"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                      .name_repair = "unique")
head(Cage_SH_raw)
(Cage_SH <- Cage_SH_raw %>% dplyr::select(ShellHeightID:ShellHeight, Comments) %>%
  mutate(ShellHeight = as.numeric(ShellHeight),
         DataType = case_when(substr(CageCountID, 26, 26) == "D" ~ "Dep",
                              substr(CageCountID, 26, 26) == "R" ~ "Ret",
                              TRUE ~ NA),
         CageColor = case_when(substr(CageCountID, 28, 28) == "R" ~ "Red",
                               substr(CageCountID, 28, 28) == "B" ~ "Brown",
                               substr(CageCountID, 28, 28) == "Y" ~ "Yellow",
                               TRUE ~ NA),
         MonYr = as.yearmon(as.Date(substring(CageCountID, 8, 15), format = "%Y%m%d")),
         FixedLocationID = substring(CageCountID, 19, 22),
         CageCountID = substr(CageCountID, 1, 22)) %>%
  left_join(Locations))
#
#END OF SECTION
#
#
#
#
####Figure formatting for consistent figures#####
#
#Basic background to work with
basetheme <- theme_bw()+
  theme(axis.title.x = element_text(size = 12, face = "bold", color = "black"), axis.text.x = element_text(size = 11, margin = unit(c(0.5, 0.5, 0, 0.5), "cm")),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"), axis.text.y = element_text(size = 11, margin = unit(c(0, 0.5, 0, 0), "cm")),
        panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(color = "black"),
        axis.ticks.length = unit(-0.15, "cm"))
#
axistheme <- theme(axis.ticks.length = unit(-0.15, "cm"), 
                   axis.text.x = element_text(color = "black", margin = unit(c(0.5, 0.5, 0, 0.5), "cm")), 
                   axis.text.y = element_text(color = "black", margin = unit(c(0, 0.5, 0, 0), "cm")))
##Colors to sites
SiteColor <- c("#56B4E9", "#009E73", "#E69F00", "#CC79A7")
names(SiteColor) <- levels(Locations$Site)
#
#
#
#
####Cage data####
#
#Average monthly/all number live, dead, and missing retrieved 
(Counts_monthly <- Cage_counts %>% group_by(FixedLocationID, Site, MonYr, CageCountID) %>% 
  summarise(Live = mean(LiveCount, na.rm = T),
            Dead = mean(DeadCount, na.rm = T),
            Missing = mean(MissCount, na.rm = T)))
Counts_monthly %>% group_by(Site) %>% 
  summarise(MeanLive = mean(Live, na.rm = T),
            MeanDead = mean(Dead, na.rm = T),
            MeanMissing = mean(Missing, na.rm = T))
#
#Live, dead, and missing monthly counts by Site
ggarrange(
#Live counts
Counts_monthly %>% 
  ggplot(aes(MonYr, Live, fill = Site)) +
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = SiteColor)+
  basetheme + axistheme,
#Dead counts
Counts_monthly %>% 
  ggplot(aes(MonYr, Dead, fill = Site)) +
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = SiteColor)+
  basetheme + axistheme,
#Missing counts
Counts_monthly %>% 
  ggplot(aes(MonYr, Missing, fill = Site)) +
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = SiteColor)+
  basetheme + axistheme,
nrow = 3, ncol = 1)
#
#Live ret count monthly
Counts_monthly %>% 
  ggplot(aes(MonYr, Live, color = Site))+
  geom_point()+
  geom_line(aes(group = Site))+
  lemon::facet_rep_grid(Site~.)+
  geom_smooth()+
  scale_y_continuous("Live Ret Count", limits = c(0,30), expand = c(0,0))+
  basetheme + axistheme
#
#Dead ret counts monthly
Counts_monthly %>% 
  ggplot(aes(MonYr, Dead, color = Site))+
  geom_point()+
  geom_line(aes(group = Site))+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme
#
#Missing ret counts monthly
Counts_monthly %>% 
  ggplot(aes(MonYr, Missing, color = Site))+
  geom_point()+
  geom_line(aes(group = Site))+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme
#
#Dead and live monthly counts
Counts_monthly %>% dplyr::select(-Missing) %>% #Missing is inverse of Live
  gather(Type, Count, -FixedLocationID, -Site, -MonYr, -CageCountID) %>%
  ggplot(aes(MonYr, Count, color = Type))+
  geom_point()+
  geom_line(aes(group = Type))+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme
#
#Percentage live, pct mortality, pct unknown
(Counts_summ <- Cage_counts %>% group_by(MonYr, Site) %>% 
  summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)))
#
#Pct Mortality (Dead: report calc) and Pct dead (Counts: only dead count) monthly
#Pct mortatlity larger driven by unknown fates. 
Counts_summ %>%
  ggplot(aes(MonYr, Pct_Dead_mean, group = 1))+
  geom_line()+
  geom_line(aes(MonYr, Pct_DeadCounts_mean, group = 1), color = "red")+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme
#
#
#
####Cage Shell Heights####
#
(ShellHeights <- Cage_SH %>% 
  group_by(MonYr, CageCountID, DataType, Site, CageColor) %>%
  summarise(MinSH = min(ShellHeight),
            MaxSH = max(ShellHeight),
            MeanSH = mean(ShellHeight, na.rm = TRUE)) %>% 
  mutate(across(all_of(c("MinSH", "MaxSH")), ~ replace(., . == Inf | . == -Inf, NA))) %>% gather(Meas, Size, -MonYr, -CageCountID, -Site, -DataType, -CageColor) %>% mutate(Measurement = paste(DataType, Meas, sep = "_")) %>%
  ungroup() %>% dplyr::select(-DataType, -Meas) %>%
  spread(Measurement, Size) %>%
   mutate(Min_growth = Ret_MinSH - Dep_MinSH,
          Max_growth = Ret_MaxSH - Dep_MaxSH, 
          Mean_growth = Ret_MeanSH - Dep_MeanSH))
#
(SH_summ <- ShellHeights %>% group_by(MonYr, Site, CageCountID) %>%  summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)))
(SH_Site_summ <- ShellHeights %>% group_by(Site) %>%  summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)))
#
#
#
### - differences in size among sites
#
ShellHeights %>% group_by(Site) %>%
  ggplot(aes(Site, Dep_MeanSH))+
  geom_point()+
  geom_boxplot()+
  scale_y_continuous(expand = c(0,0), limits = c(0,85))+
  basetheme + axistheme
#
#Compare deployed shell heights among Sites
set.seed(54321)
Site_Dep_SHs <- aovp(Dep_MeanSH_mean  ~ Site, data = SH_summ, perm = "", nperm = 10000)
(Site_Dep_SH_summ <- summary(Site_Dep_SHs))
Site_Dep_SH_tidy <- tidy(Site_Dep_SHs)
names(Site_Dep_SH_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
Site_Dep_SH_tidy
#
##Pairwise comparisons - Sites deployed
(Site_dep_pair <- as.data.frame(SH_summ) %>% pairwise_t_test(Dep_MeanSH_mean ~ Site, p.adjust.method = "holm"))
Site_dep_tab <- dplyr::select(Site_dep_pair, c("group1", "group2", "p", "p.adj")) %>%
  mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
  dplyr::select("Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj)    #Move 'Comparison' to front and drop grp1 & grp2
Site_dep_letters <- biostat::make_cld(Site_dep_tab) %>% dplyr::select(-c("spaced_cld")) %>% rename(Site = group, Letters = cld)
(Site_dep_comps <- merge(SH_summ %>% group_by(Site) %>% rstatix::get_summary_stats(Dep_MeanSH_mean , type = "mean_sd") %>% 
                dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd), Site_dep_letters, by = "Site"))
#Significant differences among all sites... maybe due to current data being used
#
#Compare retrieved shell heights among Sites
set.seed(54321)
Site_Ret_SHs <- aovp(Ret_MeanSH_mean  ~ Site, data = SH_summ, perm = "", nperm = 10000)
(Site_Ret_SH_summ <- summary(Site_Ret_SHs))
Site_Ret_SH_tidy <- tidy(Site_Ret_SHs)
names(Site_Ret_SH_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
Site_Ret_SH_tidy
#
##Pairwise comparisons - Sites deployed
(Site_Ret_pair <- as.data.frame(SH_summ) %>% pairwise_t_test(Ret_MeanSH_mean ~ Site, p.adjust.method = "holm"))
Site_Ret_tab <- dplyr::select(Site_Ret_pair, c("group1", "group2", "p", "p.adj")) %>%
  mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
  dplyr::select("Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj)    #Move 'Comparison' to front and drop grp1 & grp2
Site_Ret_letters <- biostat::make_cld(Site_Ret_tab) %>% dplyr::select(-c("spaced_cld")) %>% rename(Site = group, Letters = cld)
(Site_Ret_comps <- merge(SH_summ %>% group_by(Site) %>% rstatix::get_summary_stats(Ret_MeanSH_mean , type = "mean_sd") %>% 
                           dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd), Site_Ret_letters, by = "Site"))
#
SH_summ %>% ungroup %>% dplyr::select(Site, Dep_MeanSH_mean, Ret_MeanSH_mean) %>% rename(Dep = "Dep_MeanSH_mean", Ret = Ret_MeanSH_mean) %>% gather(Type, SH, -Site) %>% 
  ggplot(aes(Site, SH)) +
  geom_jitter(alpha = 0.6, width = 0.25)+
  geom_point(data = SH_summ %>% group_by(Site) %>% summarise(Dep = mean(Dep_MeanSH_mean, na.rm = T), Ret = mean(Ret_MeanSH_mean, na.rm = T)) %>% gather(Type, SH, -Site),
             color = "red", size = 5)+
  lemon::facet_rep_grid(.~Type)+
  basetheme + axistheme
#
#
#
#
#
###Difference within Site among years
#

#
#
#
#
####Comparison of pct mortality - minimal progress####
#
(Counts_cages <- Cage_counts %>% group_by(MonYr, Site) %>% 
   summarise(MeanPctDead = mean(Pct_Dead, na.rm = T), MeanPctCounts = mean(Pct_DeadCounts, na.rm = T)) %>%
   mutate(Year = as.factor(format(MonYr, "%Y")), Month = as.factor(format(MonYr, "%m")),
          MeanPctDead_s = scale(MeanPctDead)))
##Comparing within Site since different time frames for CRE/CRW and LX/SL
library(zoib)
SL_model <- zoib::zoib(MeanPctDead ~ Year + Month, data = Counts_cages %>% subset(Site == "SLC"), random = 1,
                       zero.inflated = T, one.inflation  = T)
#
#
#
####Comparison of growth####
#
#mm/day growth by CageColor - "raw replicate data"
(Cage_growth_raw <- left_join(ShellHeights %>% dplyr::select(MonYr:CageColor, Dep_MeanSH, Ret_MeanSH, Mean_growth),
          Cage_counts_raw %>% mutate(CageCountID = substr(CageCountID, 1, 22)) %>% 
            filter(DataType == "Retrieved") %>% dplyr::select(CageCountID, CageColor, TotalCount, DaysDeployed)) %>%
  mutate(Year = as.factor(format(MonYr, "%Y")), Month = as.factor(format(MonYr, "%m")), 
         Site = as.factor(Site), mm_day = Mean_growth/DaysDeployed))
(Cage_growth <- Cage_growth_raw %>% group_by(MonYr, Year, Month, CageCountID, Site) %>%
  summarise(MeanDep = mean(Dep_MeanSH, na.rm = T), MeanRet = mean(Ret_MeanSH, na.rm = T), 
            MeanCount = mean(TotalCount, na.rm = T), MeanGrowth = mean(Mean_growth, na.rm = T), DaysDeployed = mean(DaysDeployed),
            MeanDaily = MeanGrowth/DaysDeployed))
#
#Visualize 
ggarrange(
  Cage_growth %>% group_by(MonYr, Site) %>% summarise(Mean_growth = mean(MeanGrowth, na.rm = T)) %>%
  ggplot(aes(MonYr, Mean_growth, group = 1))+
  geom_line()+
  geom_smooth()+
    geom_hline(yintercept = 0, linetype = "dashed")+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme,
  Cage_growth %>% group_by(Year, Site) %>% summarise(Mean_growth = mean(MeanGrowth, na.rm = T)) %>%
    ggplot(aes(Year, Mean_growth, group = 1))+
    geom_line()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
Cage_growth %>% group_by(Month, Site) %>% summarise(Mean_growth = mean(MeanGrowth, na.rm = T)) %>%
  ggplot(aes(Month, Mean_growth, group = 1))+
  geom_line()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_smooth()+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme,
nrow = 1, ncol=3)
#
###Does each site have different variability among Months?
ggarrange(Growth_final %>% ggplot(aes(x = MeanGrowth)) + geom_histogram(), #normal but negative so add 10 to all values to make non-negative, continuous, normal dist
          Growth_final %>% mutate(Growth1 = MeanGrowth + 10) %>% ggplot(aes(x = Growth1)) + geom_histogram(), nrow = 2)
(Growth_final$Growth_1 <- Growth_final$MeanGrowth + 10)
#
##Permutation based ANOVA - Month, Site##
set.seed(54321)
Growth_mon <- aovp(Growth_1 ~ Month * Site, data = Growth_final, perm = "",  nperm = 10000)
(Growth_mon_summ <- summary(Growth_mon))
Growth_mon_tidy <- tidy(Growth_mon)
names(Growth_mon_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
Growth_mon_tidy
#Significant difference among Months within Sites - detrend each Site by month -- additive since pattern is theoretically the same each time period (i.e. year)
#
Growth_final %>% group_by(Month, Site) %>% summarise(meanGrowth = mean(MeanGrowth, na.rm = T), se = sd(MeanGrowth, na.rm = T)/sqrt(length(MeanGrowth))) %>%
  ggplot(aes(Month, meanGrowth, group = 1, color = Site)) + 
  geom_line() +
  geom_errorbar(aes(ymin = meanGrowth - se, ymax = meanGrowth+se), width = 0.25)+
  geom_hline(data = Growth_final %>% group_by(Site) %>% summarise(Mean = mean(MeanGrowth)), aes(yintercept = Mean), linetype = "dashed")+
  lemon::facet_rep_grid(Site~.)+
  scale_y_continuous(limits = c(0, 15), expand = c(0,0))+
  basetheme + axistheme
#
###Detrend each parameter - additive - function "detrending"
detrending <- function(df, param){
  temp <- df %>% ungroup() %>% dplyr::select(c("MonYr", all_of(param)))
  #temp$MonYr <- as.yearmon(temp$MonYr, format = "%m/%Y")
  temp <- na.interp(as.ts(read.zoo(temp, FUN = as.yearmon)))
  temp %>% decompose("additive") -> decompTemp
  tempAdj <- temp-decompTemp$seasonal #Removes seasonal component, leaves trend and random components in final output values
  return(tempAdj)
}
#
(LXN_de <- detrending(Cage_growth %>% filter(Site == "LXN"), "MeanGrowth"))
(SLC_de <- detrending(Cage_growth %>% filter(Site == "SLC"), "MeanGrowth"))
(CRE_de <- detrending(Cage_growth %>% filter(Site == "CRE"), "MeanGrowth"))
(CRW_de <- detrending(Cage_growth %>% filter(Site == "CRW"), "MeanGrowth"))
(LXN_da <- detrending(Cage_growth %>% filter(Site == "LXN"), "MeanDaily"))
(SLC_da <- detrending(Cage_growth %>% filter(Site == "SLC"), "MeanDaily"))
(CRE_da <- detrending(Cage_growth %>% filter(Site == "CRE"), "MeanDaily"))
(CRW_da <- detrending(Cage_growth %>% filter(Site == "CRW"), "MeanDaily"))
#
#Get dataframe of detrended data and add to growth data for final data frame
(Growth_detrended <- left_join(
  left_join(data.frame(MonYr = as.yearmon(time(LXN_de)), LXN_de, SLC_de),
            data.frame(MonYr = as.yearmon(time(CRE_de)), CRE_de)),
  data.frame(MonYr = as.yearmon(time(CRW_de)), CRW_de)) %>% rename(LXN = LXN_de, SLC = SLC_de, CRE = CRE_de, CRW = CRW_de) %>%
    gather("Site", "Growth_de", -MonYr))
(Daily_detrended <- left_join(
  left_join(data.frame(MonYr = as.yearmon(time(LXN_de)), LXN_da, SLC_da),
            data.frame(MonYr = as.yearmon(time(CRE_de)), CRE_da)),
  data.frame(MonYr = as.yearmon(time(CRW_da)), CRW_da)) %>% rename(LXN = LXN_da, SLC = SLC_da, CRE = CRE_da, CRW = CRW_da) %>%
    gather("Site", "Growth_da", -MonYr))
(Growth_final <- left_join(Cage_growth, Growth_detrended) %>% left_join(Daily_detrended) %>% mutate(Site = as.factor(Site)))
#
ggarrange(
  Growth_final %>% 
    ggplot(aes(MonYr, MeanGrowth, group = 1))+
    geom_line()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  Growth_final %>% 
  ggplot(aes(MonYr, Growth_de, group = 1))+
  geom_line()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_smooth()+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme,
  Growth_final %>% 
    ggplot(aes(MonYr, Growth_da, group = 1))+
    geom_line()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  nrow = 1, ncol = 3)
#
ggarrange(
  Growth_final %>% 
    ggplot(aes(MonYr, Growth_de, group = 1))+
    geom_line()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  Growth_final %>% group_by(Site, Year) %>% summarise(MeanDe = mean(Growth_de, na.rm = T)) %>%
    ggplot(aes(Year, MeanDe, group = 1))+
    geom_line()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  Growth_final %>% group_by(Site, Month) %>% summarise(MeanDe = mean(Growth_de, na.rm = T)) %>% 
    ggplot(aes(Month, MeanDe, group = 1))+
    geom_line()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  nrow = 1, ncol = 3)
#
(Growth_final$Growth_de1 <- Growth_final$Growth_de + 10)
#
#
##Permutation based ANOVA - Year for each site mean growth
set.seed(54321)
Growth_LXN <- aovp(Growth_de1 ~ Year, data = Growth_final %>% filter(Site == "LXN"), perm = "",  nperm = 10000)
Growth_SLC <- aovp(Growth_de1 ~ Year, data = Growth_final %>% filter(Site == "SLC"), perm = "",  nperm = 10000)
Growth_CRE <- aovp(Growth_de1 ~ Year, data = Growth_final %>% filter(Site == "CRE"), perm = "",  nperm = 10000)
Growth_CRW <- aovp(Growth_de1 ~ Year, data = Growth_final %>% filter(Site == "CRW"), perm = "",  nperm = 10000)

(Growth_summ <- rbind(rbind(rbind(tidy(Growth_LXN) %>% mutate(Site = "LXN"), tidy(Growth_SLC) %>% mutate(Site = "SLC")),
                          tidy(Growth_CRE) %>% mutate(Site = "CRE")), tidy(Growth_CRW) %>% mutate(Site = "CRW")) %>%
    dplyr::select(Site, everything())) 
names(Growth_summ) <- c("Site", "Factors", "df", "SS", "MS", "F", "Pr"); Growth_summ
#Significant difference among Year in LXN and SLC
#
#P values for  pairwise comparisons
(LXN_gro_p <- pairwise_t_test(Growth_de1 ~ Year, data = ungroup(Growth_final) %>% filter(Site == "LXN"), p.adjust.method = "BH") %>%
  dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
  dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj) %>% mutate(Site = "LXN", .before = "Comparison"))()

###Not giving any results due to incomplete data - Proof all data, regather data then continue working. 
SLC_gro_p <- pairwise.t.test(Growth_de1 ~ Year, data = ungroup(Growth_final) %>% filter(Site == "SLC"), p.adjust.method = "holm") %>%
  dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
  dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj) %>% mutate(Site = "SLC", .before = "Comparison"))

(Growth_year_p <- rbind(
  pairwise_t_test(Growth_de1 ~ Year, data = ungroup(Growth_final) %>% filter(Site == "SLC"), p.adjust.method = "BH") %>%
    dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
    dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj) %>% mutate(Site = "SLC", .before = "Comparison")))
#Table of summary stats per Site each Year and Letters
(Growth_year_tab <- rbind(rbind(rbind(merge(Growth_final %>% filter(Site == "LXN") %>% group_by(Year) %>% get_summary_stats(MeanGrowth, show = c("n", "mean", "sd", "min", "max")) %>% 
        dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
      biostat::make_cld(pairwise_t_test(Growth_de1 ~ Year, data = ungroup(Growth_final) %>% filter(Site == "LXN"), p.adjust.method = "BH") %>%
                          dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
                          dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj)) %>% dplyr::select(-c(spaced_cld)) %>% rename(Year = group, Letters = cld)) %>% 
  mutate(Site = "LXN", .before = "Year"),
  merge(Growth_final %>% filter(Site == "SLC") %>% group_by(Year) %>% get_summary_stats(MeanGrowth, show = c("n", "mean", "sd", "min", "max")) %>% 
          dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
        biostat::make_cld(pairwise_t_test(Growth_de1 ~ Year, data = ungroup(Growth_final) %>% filter(Site == "SLC"), p.adjust.method = "BH") %>%
                            dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
                            dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj)) %>% dplyr::select(-c(spaced_cld)) %>% rename(Year = group, Letters = cld)) %>% 
    mutate(Site = "SLC", .before = "Year")),
  Growth_final %>% filter(Site == "CRE") %>% group_by(Year) %>% get_summary_stats(MeanGrowth, show = c("n", "mean", "sd", "se", "min", "max")) %>% 
    mutate(Site = "CRE", .before = "Year") %>% mutate(lower = mean-se, upper = mean+se, Letters = NA) %>% dplyr::select(-variable, - se)),
  Growth_final %>% filter(Site == "CRW") %>% group_by(Year) %>% get_summary_stats(MeanGrowth, show = c("n", "mean", "sd", "se", "min", "max")) %>% 
    mutate(Site = "CRW", .before = "Year") %>% mutate(lower = mean-se, upper = mean+se, Letters = NA) %>% dplyr::select(-variable, -se)))
#
#Plot of mean Growth per year for each Site with pairwise letter differences.
Growth_year_tab %>%
  ggplot(aes(Year, mean))+
  geom_point()+
  geom_line(aes(group = 1))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25)+
  geom_hline(aes(yintercept = 0), linetype = "dashed")+
  geom_text(aes(y = upper+2, label = Letters))+
  lemon::facet_rep_grid(Site~.)+
  scale_y_continuous(expand = c(0,0), limits = c(-5, 20))+
  basetheme + axistheme
#
#
#
#
##Permutation based ANOVA - Year for each site mean daily growth
set.seed(54321)
Daily_LXN <- aovp(Growth_da ~ Year, data = Growth_final %>% filter(Site == "LXN"), perm = "",  nperm = 10000)
Daily_SLC <- aovp(Growth_da ~ Year, data = Growth_final %>% filter(Site == "SLC"), perm = "",  nperm = 10000)
Daily_CRE <- aovp(Growth_da ~ Year, data = Growth_final %>% filter(Site == "CRE"), perm = "",  nperm = 10000)
Daily_CRW <- aovp(Growth_da ~ Year, data = Growth_final %>% filter(Site == "CRW"), perm = "",  nperm = 10000)

(Daily_summ <- rbind(rbind(rbind(tidy(Daily_LXN) %>% mutate(Site = "LXN"), tidy(Daily_SLC) %>% mutate(Site = "SLC")),
                            tidy(Daily_CRE) %>% mutate(Site = "CRE")), tidy(Daily_CRW) %>% mutate(Site = "CRW")) %>%
    dplyr::select(Site, everything())) 
names(Daily_summ) <- c("Site", "Factors", "df", "SS", "MS", "F", "Pr"); Daily_summ
#Significant difference among Year in LXN and SLC
#
#P values for  pairwise comparisons
(Daily_year_p <- rbind(pairwise_t_test(Growth_da ~ Year, data = ungroup(Growth_final) %>% filter(Site == "LXN"), p.adjust.method = "BH") %>%
                          dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
                          dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj) %>% mutate(Site = "LXN", .before = "Comparison"),
                        pairwise_t_test(Growth_da ~ Year, data = ungroup(Growth_final) %>% filter(Site == "SLC"), p.adjust.method = "BH") %>%
                          dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
                          dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj) %>% mutate(Site = "SLC", .before = "Comparison")))
#Table of summary stats per Site each Year and Letters
(Daily_year_tab <- rbind(rbind(rbind(merge(Growth_final %>% filter(Site == "LXN") %>% group_by(Year) %>% get_summary_stats(Growth_da, show = c("n", "mean", "sd", "min", "max")) %>% 
                                              dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                                            biostat::make_cld(LXN_year_p) %>% dplyr::select(-c(spaced_cld)) %>% rename(Year = group, Letters = cld)) %>% 
                                        mutate(Site = "LXN", .before = "Year"),
                                      merge(Growth_final %>% filter(Site == "SLC") %>% group_by(Year) %>% get_summary_stats(MeanGrowth, show = c("n", "mean", "sd", "min", "max")) %>% 
                                              dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                                            biostat::make_cld(LXN_year_p) %>% dplyr::select(-c(spaced_cld)) %>% rename(Year = group, Letters = cld)) %>% 
                                        mutate(Site = "SLC", .before = "Year")),
                                Growth_final %>% filter(Site == "CRE") %>% group_by(Year) %>% get_summary_stats(MeanGrowth, show = c("n", "mean", "sd", "se", "min", "max")) %>% 
                                  mutate(Site = "CRE", .before = "Year") %>% mutate(lower = mean-se, upper = mean+se, Letters = NA) %>% dplyr::select(-variable, - se)),
                          Growth_final %>% filter(Site == "CRW") %>% group_by(Year) %>% get_summary_stats(MeanGrowth, show = c("n", "mean", "sd", "se", "min", "max")) %>% 
                            mutate(Site = "CRW", .before = "Year") %>% mutate(lower = mean-se, upper = mean+se, Letters = NA) %>% dplyr::select(-variable, -se)))
#
#Plot of mean Growth per year for each Site with pairwise letter differences.
Growth_year_tab %>%
  ggplot(aes(Year, mean))+
  geom_point()+
  geom_line(aes(group = 1))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25)+
  geom_hline(aes(yintercept = 0), linetype = "dashed")+
  geom_text(aes(y = upper+2, label = Letters))+
  lemon::facet_rep_grid(Site~.)+
  scale_y_continuous(expand = c(0,0), limits = c(-5, 20))+
  basetheme + axistheme

#
####Extra code####
#From Cage Shell Heights
SH_summ %>%
  ggplot(aes(MonYr, Mean_growth_mean, group = 1))+
  geom_line()+
  geom_line(aes(MonYr, Min_growth_mean, group = 1), color = "red")+
  geom_line(aes(MonYr, Max_growth_mean, group = 1), color = "blue")+
  geom_hline(yintercept = 0, linetype = "dotted")+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme
SH_summ %>% 
  ggplot(aes(MonYr, Dep_MeanSH_mean, group = 1))+
  geom_line()+
  geom_line(aes(MonYr, Dep_MinSH_mean, group = 1), color = "red")+
  geom_line(aes(MonYr, Dep_MaxSH_mean, group = 1), color = "blue")+
  geom_hline(data = SH_Site_summ, aes(yintercept = Dep_MeanSH_mean), linetype = "dashed")+
  geom_hline(data = SH_Site_summ, aes(yintercept = Dep_MinSH_mean), linetype = "dashed", color = "red")+
  geom_hline(data = SH_Site_summ, aes(yintercept = Dep_MaxSH_mean), linetype = "dashed", color = "blue")+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme
##Data analysis and visualizations for Small SF

# SETUP ----
source("0-clean.R") # data cleaning script, produces cleaned data for study 1 and 2
# Load cleaned data - 2 dfs
rm(list = ls())
load("../Data/all_data_study1.RData")
load("../Data/all_data_study2.RData")

## load libraries
library(tidyverse)
library(magrittr)
library(langcog)
library(lme4)
library(stringr)    
library(RColorBrewer)
library(ggthemes)
library(broom.mixed)

'%!in%' <- function(x,y)!('%in%'(x,y))

# classify SF and NN as within or outside count range
all.data.study1 %<>%
  mutate(kl.num = ifelse(Knower_level == "CP", 6, as.numeric(as.character(Knower_level))), 
         count_range = factor(ifelse(Task_item <= kl.num, "Within", "Outside")), 
         CP_subset = factor(ifelse(Knower_level == "CP", "CP", "Subset")))

all.data.study2 %<>%
  mutate(kl.num = ifelse(Knower_level == "CP", 6, as.numeric(as.character(Knower_level))), 
         count_range = factor(ifelse(Task_item <= kl.num, "Within", "Outside")), 
         CP_subset = factor(ifelse(Knower_level == "CP", "CP", "Subset")))

#global theme set
theme_set(theme_bw() + theme(text = element_text(size=9), 
                             axis.title=element_text(size=8),
                             strip.text = element_text(margin=margin(2,0,2,0)), 
                             panel.grid = element_blank()))


# Study 1 ----

# ...demographics ----
##Overall
all.data.study1 %>% 
  distinct(SID, Age)%>%
  summarise_at('Age', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~median(., na.rm=T),
                    ~min(., na.rm=T),
                    ~max(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(n, mean, sd, median, min, max)

#sex
all.data.study1 %>%
  distinct(SID, Sex)%>%
  group_by(Sex)%>%
  summarise(n = n())

##CP_subset
all.data.study1 %>% 
  distinct(SID, CP_subset, Knower_level, Age)%>%
  group_by(CP_subset)%>%
  summarise_at('Age', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~median(., na.rm=T),
                    ~min(., na.rm=T),
                    ~max(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(CP_subset, n, mean, sd, median, min, max)

##KLs
all.data.study1 %>% 
  distinct(SID, Knower_level, Age)%>%
  group_by(Knower_level)%>%
  summarise_at('Age', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~median(., na.rm=T),
                    ~min(., na.rm=T),
                    ~max(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(Knower_level, n, mean, sd, median, min, max)

# ...analysis: Do subset-knowers do better on this task than chance (glmers)? ----
hc.only.df <- all.data.study1 %>%
  filter(!is.na(highest_count))
##Test 1: Subset-knowers, all trials
subset.chance.overall <- glmer(Correct ~ 1 + (1|SID), family = "binomial", 
                               data = subset(all.data.study1, CP_subset == "Subset")) #null model, testing intercept
summary(subset.chance.overall) #Z = 3.47, p = .0005

##Test 2: Subset-knowers, within-range trials
subset.chance.within <- glmer(Correct ~ 1 + (1|SID), family = "binomial", 
                               data = subset(all.data.study1, CP_subset == "Subset" & count_range == "Within")) #null model, testing intercept
summary(subset.chance.within) #Z = 5.28, p < .0001

##Test 3: Does adding highest count improve: 
###3a: Overall performance model? 
subset.overall.hc <- glmer(Correct ~ highest_count + (1|SID), 
                   family = "binomial", data = subset(all.data.study1, CP_subset == "Subset"))
anova(subset.chance.overall, subset.overall.hc, test= 'LRT') #NS, Chisq = 0.45, p = .50

###3b: Within-range performance model? 
subset.within.hc <- glmer(Correct ~ highest_count + (1|SID), 
                           family = "binomial", data = subset(all.data.study1, CP_subset == "Subset" & count_range == "Within"))
anova(subset.chance.within, subset.within.hc, test= 'LRT') #NS, Chisq = 0.51, p = 0.48

##Test 4: Do CP-knowers have greater performance relative to subset knowers overall? (Removing kid without HC)
#construct base model with age
cp.subset.base <- glmer(Correct ~ age.c + (1|SID), 
                        family = "binomial", data = hc.only.df)

#add knower level - does this explain additional variance?
cp.subset.kl <- glmer(Correct ~ CP_subset + age.c + (1|SID), 
                      family = "binomial", data = hc.only.df)

#compare - does KL improve the fit of the base model? 
anova(cp.subset.base, cp.subset.kl, test = 'LRT') #Chisq = 7.71, p = .005, AIC = 984.66
summary(cp.subset.kl)

##Test 4b: Does adding highest count improve this model more than knower level?
cp.hc <- glmer(Correct ~ highest_count.c + age.c + (1|SID), 
               family = "binomial", data = all.data.study1)

#compare
anova(cp.subset.base, cp.hc, test = 'LRT') #Chisq = 6.59, p = .01, AIC = 985.78
##Now add highest count to a model containing CP_subset, test to see if it improves the model
cp.and.hc <- cp.hc <- glmer(Correct ~ highest_count.c + CP_subset + age.c + (1|SID), 
                            family = "binomial", data = all.data.study1)
anova(cp.subset.kl, cp.and.hc, test = 'LRT') #NS, Chisq = 2.67, p = .10

##Test 6: Does highest count predict performance for CP-knowers? 
cp.hc.only.cp.base <- glmer(Correct ~ age.c + (1|SID), 
                       family = "binomial", data = subset(hc.only.df, CP_subset == "CP"))
cp.hc.only.cp <- glmer(Correct ~ highest_count.c + age.c + (1|SID), 
                       family = "binomial", data = subset(hc.only.df, CP_subset == "CP"))
anova(cp.hc.only.cp.base, cp.hc.only.cp, test = 'LRT') #NS, Chisq = 1.6, p = .21

# ... Exploratory: t-tests against chance for each knower level and each item 
subset.ms <- all.data.study1 %>%
  group_by(SID, Knower_level, Task_item)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

##1-knowers
t.test(subset(subset.ms, Knower_level == "1" & Task_item == 1)$mean, mu = .5, var.equal = TRUE) ##NS; t(17) = -0.29, p = 0.77
t.test(subset(subset.ms, Knower_level == "1" & Task_item == 2)$mean, mu = .5, var.equal = TRUE) ##NS; t(17) = 0, p = 1
t.test(subset(subset.ms, Knower_level == "1" & Task_item == 3)$mean, mu = .5, var.equal = TRUE) ##NS; t(17) = 0.37, p = 0.72
t.test(subset(subset.ms, Knower_level == "1" & Task_item == 4)$mean, mu = .5, var.equal = TRUE) ##NS; t(17) = -0.70, p = 0.50
t.test(subset(subset.ms, Knower_level == "1" & Task_item == 5)$mean, mu = .5, var.equal = TRUE) ##NS; t(17) = 0, p = 1

##2-knowers
t.test(subset(subset.ms, Knower_level == "2" & Task_item == 1)$mean, mu = .5, var.equal = TRUE) ##Significant, t(17) = 7.71, p < .0001
t.test(subset(subset.ms, Knower_level == "2" & Task_item == 2)$mean, mu = .5, var.equal = TRUE) ##NS; t(17) = 0.44, p = 0.67
t.test(subset(subset.ms, Knower_level == "2" & Task_item == 3)$mean, mu = .5, var.equal = TRUE) ##NS; t(17) = -0.37, p = 0.72
t.test(subset(subset.ms, Knower_level == "2" & Task_item == 4)$mean, mu = .5, var.equal = TRUE) ##NS; t(17) = 1.46, p = 0.16
t.test(subset(subset.ms, Knower_level == "2" & Task_item == 5)$mean, mu = .5, var.equal = TRUE) ##NS; t(17) = -0.37, p = 0.72

#3-knowers
t.test(subset(subset.ms, Knower_level == "3" & Task_item == 1)$mean, mu = .5, var.equal = TRUE) ##Significant, t(13) = 8.83, p < .0001
t.test(subset(subset.ms, Knower_level == "3" & Task_item == 2)$mean, mu = .5, var.equal = TRUE) ##Significant, t(13) = 3.80, p = .002
t.test(subset(subset.ms, Knower_level == "3" & Task_item == 3)$mean, mu = .5, var.equal = TRUE) ##Marginal, t(13) = 2.12, p = .05
t.test(subset(subset.ms, Knower_level == "3" & Task_item == 4)$mean, mu = .5, var.equal = TRUE) ##NS; t(13) = 0, p = 1
t.test(subset(subset.ms, Knower_level == "3" & Task_item == 5)$mean, mu = .5, var.equal = TRUE) ##NS; t(13) = -0.62, p = 0.55

#CP-knowers
t.test(subset(subset.ms, Knower_level == "CP" & Task_item == 1)$mean, mu = .5, var.equal = TRUE) ##Significant, t(22) = 10.72, p < .0001
t.test(subset(subset.ms, Knower_level == "CP" & Task_item == 2)$mean, mu = .5, var.equal = TRUE) ##Significant, t(22) = 3.82, p= .0008
t.test(subset(subset.ms, Knower_level == "CP" & Task_item == 3)$mean, mu = .5, var.equal = TRUE) ##Significant, t(22) = 4.03, p = .0005
t.test(subset(subset.ms, Knower_level == "CP" & Task_item == 4)$mean, mu = .5, var.equal = TRUE) ##Significant, t(22) = 2.33, p = .03
t.test(subset(subset.ms, Knower_level == "CP" & Task_item == 5)$mean, mu = .5, var.equal = TRUE) ##NS, t(22) = -0.72, p = 0.48

# ...visualization of SF performance ---- 
##Subset-knowers only
all.data.study1 %>%
  filter(Knower_level == "1" | 
           Knower_level == "2" | 
           Knower_level == "3" | 
           Knower_level == "4")%>%
  mutate(Task_item = factor(Task_item), 
         Knower_level.combined = ifelse((Knower_level == "4" | Knower_level == "3"), "3- & 4-knowers", as.character(Knower_level)),
         Knower_level.combined = factor(Knower_level.combined, levels= c("1", "2", "3- & 4-knowers"), 
                                        labels = c("1-knowers", "2-knowers", "3- & 4-knowers")))%>%
  group_by(Task_item, Knower_level.combined)%>%
  langcog::multi_boot_standard("Correct", na.rm = TRUE) %>%
  ggplot(aes(x = Task_item, y = mean, colour = Knower_level.combined, group= Knower_level.combined)) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "grey", size = .9) +
  geom_point(size = 2.5, 
             show.legend = FALSE) + 
  geom_line(size = .7, 
            show.legend = FALSE) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0, size = .5, 
                show.legend = FALSE) +
  theme_bw(base_size = 10) + 
  theme(legend.position = "none", 
        panel.grid = element_blank()) + 
  facet_wrap(~Knower_level.combined, ncol = 4) + 
  langcog::scale_color_solarized("Knower level") +
  labs(x = "Starting set size", y = "Mean Unit Task performance")
ggsave("Figures/study1_nKL_Unit.png", width = 6, height = 3)

##CP and subset knowers
all.data.study1 %>%
  mutate(Task_item = factor(Task_item), 
         Correct = as.numeric(as.character(Correct)), 
         CP_subset = factor(CP_subset, levels = c("Subset", "CP"), 
                            labels = c("Subset-knower", "CP-knower")))%>%
  group_by(Task_item, CP_subset)%>%
  langcog::multi_boot_standard("Correct", na.rm = TRUE) %>%
  ggplot(aes(x = Task_item, y = mean, colour = CP_subset, group= CP_subset)) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "grey", size = .9) +
  geom_point(size = 2.5) + 
  geom_line(size = .7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0, size = .5) +
  theme_bw(base_size = 10) + 
  theme(panel.grid = element_blank(), 
        legend.position = "right", 
        legend.title = element_blank()) + 
  scale_y_continuous(breaks = seq(0,1,.25), limits = c(0, 1)) + 
  scale_color_brewer(palette = "Paired") + 
  labs(fill = "Knower level", 
       x = "Starting set size", y = "Mean Unit Task performance")
ggsave("Figures/study1_CP_subset_Unit.png", width = 6, height = 4)


# Study 2 ----

# ...demographics ----
##Overall
all.data.study2 %>% 
  distinct(SID, Age)%>%
  summarise_at('Age', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~median(., na.rm=T),
                    ~min(., na.rm=T),
                    ~max(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(n, mean, sd, median, min, max)

#sex
all.data.study2 %>%
  distinct(SID, Sex)%>%
  group_by(Sex)%>%
  summarise(n = n())

##CP_subset
all.data.study2 %>% 
  distinct(SID, CP_subset, Knower_level, Age)%>%
  group_by(CP_subset)%>%
  summarise_at('Age', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~median(., na.rm=T),
                    ~min(., na.rm=T),
                    ~max(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(CP_subset, n, mean, sd, median, min, max)

##KLs
all.data.study2 %>% 
  distinct(SID, Knower_level, Age)%>%
  group_by(Knower_level)%>%
  summarise_at('Age', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~median(., na.rm=T),
                    ~min(., na.rm=T),
                    ~max(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(Knower_level, n, mean, sd, median, min, max)


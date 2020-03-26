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

#custom palettes
task.pal <- c("#cb4b16", "#2aa198")


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
##Descriptives of mean performance
#overall
all.data.study1 %>%
  group_by(CP_subset)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

#count_range
all.data.study1 %>%
  group_by(CP_subset, count_range)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

#create a dataframe for highest count analyses
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
                            labels = c("Subset-knowers", "CP-knowers")))%>%
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

# ...analysis: SF and NN comparison
##Descriptives of mean performance
#overall
all.data.study2 %>%
  group_by(CP_subset, Task)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

#count range
all.data.study2 %>%
  group_by(CP_subset, Task, count_range)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

##Test 1: Do subset-knowers perform better on NN in comparison to SF overall?
#remove NA highest count so we can do comparisons
model.df.2 <- all.data.study2 %>%
  filter(!is.na(highest_count))
#make base
nn.v.sf.base <- glmer(Correct ~ 1 + (1|SID), 
                      family = 'binomial', data = subset(model.df.2, CP_subset == "Subset"))
#add task
nn.v.sf.task <- glmer(Correct ~ Task + (1|SID), 
                      family = 'binomial', data = subset(model.df.2, CP_subset == "Subset"))
#compare
anova(nn.v.sf.base, nn.v.sf.task, test= 'LRT')# Chisq = 66.55, p < .0001
summary(nn.v.sf.task)

##Test 2: Do subset-knowers perform better on NN in comparison to SF for numbers within knower level?
#make base
nn.v.sf.base.within <- glmer(Correct ~ 1 + (1|SID), 
                      family = 'binomial', data = subset(model.df.2, CP_subset == "Subset" & count_range == "Within"))
#add task
nn.v.sf.task.within <- glmer(Correct ~ Task + (1|SID), 
                      family = 'binomial', data = subset(model.df.2, CP_subset == "Subset" & count_range == "Within"))
#compare
anova(nn.v.sf.base.within, nn.v.sf.task.within, test= 'LRT')# Chisq = 36.45, p < .0001
summary(nn.v.sf.task.within)

##Test 3: Do CP-knowers perform better on NN in comparison to SF overall?
#make base
nn.v.sf.base.cp <- glmer(Correct ~ 1 + (1|SID), 
                      family = 'binomial', data = subset(model.df.2, CP_subset == "CP"))
#add task
nn.v.sf.task.cp <- glmer(Correct ~ Task + (1|SID), 
                      family = 'binomial', data = subset(model.df.2, CP_subset == "CP"))
#compare
anova(nn.v.sf.base.cp, nn.v.sf.task.cp, test= 'LRT')# Chisq = 1.55, p = 0.21
summary(nn.v.sf.task.cp)

##Test 4: Do CP-knowers outperform subset knowers on NN? 
nn.cp.v.subset.base <- glmer(Correct ~ age.c + (1|SID), 
                             family = binomial, data = subset(model.df.2, Task == "Next_number"))
nn.cp.v.subset.kl <- glmer(Correct ~ CP_subset + age.c + (1|SID), 
                             family = binomial, data = subset(model.df.2, Task == "Next_number"))
anova(nn.cp.v.subset.base, nn.cp.v.subset.kl, test = 'lrt')
summary(nn.cp.v.subset.kl)

# ...analysis: SF performance
##Test 1: Do subset-knowers perform above chance on SF overall?
subset.sf.chance <- glmer(Correct ~ 1 + (1|SID), 
                          family = 'binomial', data = subset(model.df.2, CP_subset == "Subset" & Task == "SF"))

summary(subset.sf.chance) #Z 2.50, p = .01

##Test 2: Do subset-knowers perform above chance on SF within range?
subset.sf.within <- glmer(Correct ~ 1 + (1|SID), 
                          family = 'binomial', data = subset(model.df.2, CP_subset == "Subset" & Task == "SF" & count_range == "Within"))

summary(subset.sf.within) #Z 3.76, p = .0002

##Test 3: Do CP-knowers perform better than subset knowers overall?
cp.subset.sf.overall.base <- glmer(Correct ~ age.c + (1|SID), 
                          family = 'binomial', data = subset(model.df.2, Task == "SF"))
cp.subset.sf.overall.kl <- glmer(Correct ~ CP_subset + age.c + (1|SID), 
                                   family = 'binomial', data = subset(model.df.2, Task == "SF"))
anova(cp.subset.sf.overall.base, cp.subset.sf.overall.kl, test = 'lrt') #Chisq = 4.22, p = .04
summary(cp.subset.sf.overall.kl)#beta = 0.48, p = .04


##Test 4a: Does highest count predict performance for subset knowers? 
subset.sf.hc.base <- glmer(Correct ~ 1 + (1|SID), 
                                   family = 'binomial', data = subset(model.df.2, Task == "SF" & CP_subset == "Subset"))
subset.sf.hc <- glmer(Correct ~ highest_count.c + (1|SID), 
                                 family = 'binomial', data = subset(model.df.2, Task == "SF" & CP_subset == "Subset"))
anova(subset.sf.hc.base, subset.sf.hc, test = 'lrt') #Chisq = 0.56, p = 0.45

##Test 4b: Does highest count predict performance for CP knowers? 
cp.sf.hc.base <- glmer(Correct ~ 1 + (1|SID), 
                           family = 'binomial', data = subset(model.df.2, Task == "SF" & CP_subset == "CP"))
cp.sf.hc <- glmer(Correct ~ highest_count.c + (1|SID), 
                      family = 'binomial', data = subset(model.df.2, Task == "SF" & CP_subset == "CP"))
anova(cp.sf.hc.base, cp.sf.hc, test = 'lrt') #Chisq = 1.31, p = 0.27

# ...Exploratory: t-tests against chance for item by knower level ----
study2.ms <- all.data.study2 %>%
  filter(Task == "SF")%>%
  group_by(SID, Knower_level, Task_item)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

#1-knowers
t.test(subset(study2.ms, Knower_level == "1" & Task_item == "1")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = -0.69, p = 0.5
t.test(subset(study2.ms, Knower_level == "1" & Task_item == "2")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = 1.15, p = 0.27
t.test(subset(study2.ms, Knower_level == "1" & Task_item == "3")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = 0.56, p = 0.58
t.test(subset(study2.ms, Knower_level == "1" & Task_item == "4")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = 0, p = 1
t.test(subset(study2.ms, Knower_level == "1" & Task_item == "5")$mean, mu = .5, var.equal = TRUE) #ns t(13) = 0.56, p = 0.58


#2-knowers
t.test(subset(study2.ms, Knower_level == "2" & Task_item == "1")$mean, mu = .5, var.equal = TRUE) #sig, t(17) = 3.34, p = .004
t.test(subset(study2.ms, Knower_level == "2" & Task_item == "2")$mean, mu = .5, var.equal = TRUE) #ns; t(17) = -0.32, p = 0.75
t.test(subset(study2.ms, Knower_level == "2" & Task_item == "3")$mean, mu = .5, var.equal = TRUE) #ns; t(17) = -0.32, P = 0.75
t.test(subset(study2.ms, Knower_level == "2" & Task_item == "4")$mean, mu = .5, var.equal = TRUE) #ns; t(17) = 0.37, p = 0.72
t.test(subset(study2.ms, Knower_level == "2" & Task_item == "5")$mean, mu = .5, var.equal = TRUE) #ns; t(16) = 0.37, p = 0.72

#3-knowers
t.test(subset(study2.ms, Knower_level == "3" & Task_item == "1")$mean, mu = .5, var.equal = TRUE) #sig, t(7) = 2.65, p = .03
t.test(subset(study2.ms, Knower_level == "3" & Task_item == "2")$mean, mu = .5, var.equal = TRUE) #ns; t(7) = 1.87, p= .10
t.test(subset(study2.ms, Knower_level == "3" & Task_item == "3")$mean, mu = .5, var.equal = TRUE) #ns; t(7) = 1, p= 0.35
t.test(subset(study2.ms, Knower_level == "3" & Task_item == "4")$mean, mu = .5, var.equal = TRUE) #ns; t(7) = 0.55, p = 0.60
t.test(subset(study2.ms, Knower_level == "3" & Task_item == "5")$mean, mu = .5, var.equal = TRUE) #ns; t(7) = -1.43, p = 0.20

#CP-knowers
t.test(subset(study2.ms, Knower_level == "CP" & Task_item == "1")$mean, mu = .5, var.equal = TRUE) #sig, t(27) = 11.15, p <.0001
t.test(subset(study2.ms, Knower_level == "CP" & Task_item == "2")$mean, mu = .5, var.equal = TRUE) #sig; t(27) = 8.22, p= < .0001
t.test(subset(study2.ms, Knower_level == "CP" & Task_item == "3")$mean, mu = .5, var.equal = TRUE) #sig; t(27) = 3.86, p= 0.0006
t.test(subset(study2.ms, Knower_level == "CP" & Task_item == "4")$mean, mu = .5, var.equal = TRUE) #ns; t(27) = 0.72, p = 0.48
t.test(subset(study2.ms, Knower_level == "CP" & Task_item == "5")$mean, mu = .5, var.equal = TRUE) #ns; t(27) = 1.22, p = 0.23

# ...visualizations ----
##Subset-knowers; SF vs. NN
all.data.study2 %>%
  filter(Knower_level != "CP")%>%
  mutate(Task_item = factor(Task_item), 
         Correct = as.numeric(as.character(Correct)), 
         Knower_level.combined = ifelse((Knower_level == "4" | Knower_level == "3" | Knower_level == "5"), "3-, 4-, & 5-knowers", as.character(Knower_level)),
         Knower_level.combined = factor(Knower_level.combined, levels= c("1", "2", "3-, 4-, & 5-knowers"), 
                                        labels = c("1-knowers", "2-knowers", "3-, 4-, & 5-knowers")), 
         Task = factor(Task, levels = c("Next_number", "SF"), 
                       labels = c("Next Number", "Unit Task")))%>%
  group_by(Knower_level.combined, Task, Task_item)%>%
  langcog::multi_boot_standard("Correct", na.rm = TRUE) %>%
  ggplot(aes(x = Task_item, y = mean, colour = Task, group= Task)) +
  geom_point(size = 2.5) + 
  geom_line(size = .7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0, size = .5) +
  theme_bw(base_size = 10) + 
  theme(legend.position = "right", 
        panel.grid = element_blank(), 
        legend.title = element_blank()) + 
  facet_wrap(~Knower_level.combined) + 
  scale_y_continuous(breaks = seq(0,1,.25), limits = c(0, 1)) + 
  scale_color_manual(values = task.pal)+ 
  labs(x = "Number queried", y = "Mean task performance")
ggsave("Figures/study2_nKL_NNandUnit.png", width = 7.5, height = 3)

##SF and NN by CP/Subset
all.data.study2 %>%
  mutate(Task_item = factor(Task_item), 
         Correct = as.numeric(as.character(Correct)), 
         Task = factor(Task, levels = c("Next_number", "SF"), 
                       labels = c("Next Number", "Unit Task")), 
         CP_subset = factor(CP_subset, levels = c("Subset", "CP"), 
                            labels = c("Subset-knowers", "CP-knowers")))%>%
  group_by(CP_subset, Task, Task_item)%>%
  langcog::multi_boot_standard("Correct", na.rm = TRUE) %>%
  ggplot(aes(x = Task_item, y = mean, colour = Task, group= Task)) +
  geom_point(size = 2.5) + 
  geom_line(size = .7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0, size = .5) +
  theme_bw(base_size = 10) + 
  theme(
    panel.grid = element_blank(), 
    legend.position = "right", 
    legend.title = element_blank()) + 
  facet_wrap(~CP_subset) + 
  scale_y_continuous(breaks = seq(0,1,.25), limits = c(0, 1)) + 
  scale_color_manual(values = task.pal)+ 
  labs(x = "Number queried", y = "Mean task performance")
ggsave("Figures/study2_CPSubset_NNandUnit.png", width = 6.5, height = 3)


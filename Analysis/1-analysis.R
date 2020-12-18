##Data analysis and visualizations for Small SF

# SETUP ----
source("0-clean.R") # data cleaning script, produces cleaned data for study 1 and 2
# Load cleaned data - 2 dfs
rm(list = ls())
load(here::here("../Data/all_data_study1.RData"))
load(here::here("../Data/all_data_study2.RData"))

## load libraries
library(tidyverse)
library(magrittr)
library(langcog)
library(lme4)
library(stringr)    
library(RColorBrewer)
library(ggthemes)
library(broom.mixed)
library(performance)

'%!in%' <- function(x,y)!('%in%'(x,y))

# Leftover data wrangling/manipulations ----

# classify SF and NN as within or outside count range
all.data.study1 %<>%
  mutate(kl.num = ifelse(Knower_level == "CP", 6, as.numeric(as.character(Knower_level))), 
         count_range = factor(ifelse(Task_item <= kl.num, "Within", "Outside")), 
         CP_subset = factor(ifelse(Knower_level == "CP", "CP", "Subset")))

all.data.study2 %<>%
  mutate(kl.num = ifelse(Knower_level == "CP", 6, as.numeric(as.character(Knower_level))), 
         count_range = factor(ifelse(Task_item <= kl.num, "Within", "Outside")), 
         CP_subset = factor(ifelse(Knower_level == "CP", "CP", "Subset")))

# Global settings ----

#global theme set
theme_set(theme_bw() + theme(strip.text = element_text(margin=margin(2,0,2,0)), 
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
  distinct(SID, CP_subset, Sex)%>%
  group_by(CP_subset, Sex)%>%
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


# ...analyses ----
##Descriptives of mean performance
#overall - by CP knower status
all.data.study1 %>%
  group_by(CP_subset)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

#count_range - by CP knower status
all.data.study1 %>%
  group_by(CP_subset, count_range)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

# how many IDKs in Exp. 1? 
all.data.study1 %>%
  filter(Task == "SF")%>%
  mutate(IDK = ifelse(Response == -1000, "IDK", "Not"))%>%
  group_by(IDK)%>%
  summarise(n = n())

## highest count descriptives
all.data.study1 %>%
  distinct(SID, CP_subset, highest_count)%>%
  summarise(mean = mean(highest_count, na.rm = TRUE), 
            sd = sd(highest_count, na.rm = TRUE), 
            median = median(highest_count, na.rm = TRUE), 
            min = min(highest_count, na.rm = TRUE), 
            max = max(highest_count, na.rm = TRUE))

## Test 1: Do CP-knowers outperform subset knowers? 
### Make the base model 
cp.subset.overall.base <- glmer(Correct ~ age.c + (1|SID), family = "binomial", 
                                data = all.data.study1)
### Add a CP term
cp.subset.overall.kl <- glmer(Correct ~ CP_subset + age.c + (1|SID), family = "binomial", 
                                data = all.data.study1)
### summary
summary(cp.subset.overall.kl)

#compare 
anova(cp.subset.overall.base, cp.subset.overall.kl, test = 'lrt') #chisq(1) = 8.29, p = .004

### Post-hoc test: t-tests against chance for sets of 4 and 5 for CP-knowers
subset.ms <- all.data.study1 %>%
  group_by(SID, Knower_level, Task_item)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

#CP-knowers
t.test(subset(subset.ms, Knower_level == "CP" & Task_item == 1)$mean, mu = .5, var.equal = TRUE) ##Significant, t(23) = 10.72, p < .0001
t.test(subset(subset.ms, Knower_level == "CP" & Task_item == 2)$mean, mu = .5, var.equal = TRUE) ##Significant, t(23) = 3.82, p= .0008
t.test(subset(subset.ms, Knower_level == "CP" & Task_item == 3)$mean, mu = .5, var.equal = TRUE) ##Significant, t(23) = 4.03, p = .0005
t.test(subset(subset.ms, Knower_level == "CP" & Task_item == 4)$mean, mu = .5, var.equal = TRUE) ##Significant, t(23) = 2.33, p = .03
t.test(subset(subset.ms, Knower_level == "CP" & Task_item == 5)$mean, mu = .5, var.equal = TRUE) ##NS, t(23) = -0.72, p = 0.48

# ...visualization of CP/subset-knowers for Exp. 1 ----
all.data.study1 %>%
  mutate(Task_item = factor(Task_item), 
         Correct = as.numeric(as.character(Correct)), 
         CP_subset = factor(CP_subset, levels = c("Subset", "CP"), 
                            labels = c("Subset-knowers", "CP-knowers")))%>%
  group_by(Task_item, CP_subset)%>%
  langcog::multi_boot_standard("Correct", na.rm = TRUE) %>%
  ggplot(aes(x = Task_item, y = mean, colour = CP_subset, group= CP_subset, shape = CP_subset)) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "grey", size = .35) +
  geom_point(size = 1.5) + 
  geom_line(size = .35) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0, size = .5) +
  theme_bw(base_size = 8) + 
  theme(panel.grid = element_blank(), 
        legend.position = "right", 
        legend.title = element_blank()) + 
  scale_y_continuous(breaks = seq(0,1,.25), limits = c(0, 1)) + 
  scale_color_brewer(palette = "Paired") + 
  labs(fill = "Knower level", 
       x = "Starting set size", y = "Mean Unit Task performance")
ggsave("Figures/study1_CP_subset_Unit.png", units = "in", width = 3.5, height = 2.25)

## Test 2: Do subset knowers perform better than chance on this task? 
### First, overall
subset.chance.overall <- glmer(Correct ~ 1 + (1|SID), 
                               family = "binomial", 
                               data = subset(all.data.study1, CP_subset == "Subset"))
summary(subset.chance.overall)

### Next, only within number range
subset.chance.within <- glmer(Correct ~ 1 + (1|SID), 
                               family = "binomial", 
                               data = subset(all.data.study1, CP_subset == "Subset" & count_range == "Within"))
summary(subset.chance.within)

## follow-up: Does CP-knower advantage go away when we consider only items within knower level?
subset.cp.within <- glmer(Correct ~ CP_subset + (1|SID), 
                              family = "binomial", 
                              data = subset(all.data.study1, count_range == "Within"))
summary(subset.cp.within)

### follow up with SF within
all.data.study1 %<>%
  mutate(count_range_sf = ifelse(Task_item + 1 <= kl.num, "Within", "Outside"))
subset.chance.within.sf <- glmer(Correct ~ 1 + (1|SID), 
                              family = "binomial", 
                              data = subset(all.data.study1, CP_subset == "Subset" & count_range_sf == "Within"))
summary(subset.chance.within.sf)


## descriptive for mean within.sf 
all.data.study1 %>%
  group_by(CP_subset, count_range_sf)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

### Follow-up tests: t-tests against chance for individual items
##1-knowers
t.test(subset(subset.ms, Knower_level == "1" & Task_item == 1)$mean, mu = .5, var.equal = TRUE) ##NS; t(17) = -0.29, p = 0.77
t.test(subset(subset.ms, Knower_level == "1" & Task_item == 2)$mean, mu = .5, var.equal = TRUE) ##NS; t(17) = 0, p = 1
t.test(subset(subset.ms, Knower_level == "1" & Task_item == 3)$mean, mu = .5, var.equal = TRUE) ##NS; t(17) = 0.37, p = 0.72
t.test(subset(subset.ms, Knower_level == "1" & Task_item == 4)$mean, mu = .5, var.equal = TRUE) ##NS; t(17) = -0.70, p = 0.50
t.test(subset(subset.ms, Knower_level == "1" & Task_item == 5)$mean, mu = .5, var.equal = TRUE) ##NS; t(17) = 0, p = 1

##2-knowers
t.test(subset(subset.ms, Knower_level == "2" & Task_item == 1)$mean, mu = .5, var.equal = TRUE) ##Significant, t(17) = 6.65, p < .0001
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

# ...visualization of subset knower performance for exp 1 ----
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
  geom_hline(yintercept = .5, linetype = "dashed", color = "grey", size = .35) +
  geom_point(size = 1.5, 
             show.legend = FALSE) + 
  geom_line(size = .35, 
            show.legend = FALSE) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0, size = .35, 
                show.legend = FALSE) +
  theme_bw(base_size = 8) + 
  theme(legend.position = "none", 
        panel.grid = element_blank()) + 
  facet_wrap(~Knower_level.combined, ncol = 4) + 
  langcog::scale_color_solarized("Knower level") +
  labs(x = "Starting set size", y = "Mean Unit Task performance")
ggsave("Figures/study1_nKL_Unit.png", units = "in", width = 5, height = 2)

## Test 3: Does highest count significantly predict performance? 
### descriptives of highest count
all.data.study1 %>%
  distinct(SID, CP_subset, highest_count)%>%
  group_by(CP_subset)%>%
  summarise(mean_hc = mean(highest_count, na.rm = TRUE), 
            sd_hc = sd(highest_count, na.rm = TRUE), 
            median_hc = median(highest_count, na.rm = TRUE))

### First, subset knowers base
subset.hc.base <- glmer(Correct ~ age.c + (1|SID), 
                   family = "binomial", data = subset(all.data.study1, CP_subset == "Subset"))
### Now add highest count
subset.hc.hc <- glmer(Correct ~ highest_count.c + age.c + (1|SID), 
                        family = "binomial", data = subset(all.data.study1, CP_subset == "Subset"))
#compare - does highest count do anything?
anova(subset.hc.base, subset.hc.hc, test = 'lrt')

### Now, CP knowers
## need to exclude CP-knower without highest count
cp.hc <- all.data.study1 %>%
  filter(!is.na(highest_count))

cp.hc.base <- glmer(Correct ~ age.c + (1|SID), 
                        family = "binomial", data = subset(cp.hc, CP_subset == "CP"))
### Now add highest count
cp.hc.hc <- glmer(Correct ~ highest_count.c + age.c + (1|SID), 
                      family = "binomial", data = subset(cp.hc, CP_subset == "CP"))
#compare - does highest count do anything?
anova(cp.hc.base, cp.hc.hc, test = 'lrt')

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

## ...Follow up - are subset-knowers older than CP-knowers ----
age.data.1 <- all.data.study1 %>%
  distinct(SID, Age, CP_subset, Knower_level)

##checking whether 2-knowers are older than CP-knowers
tmp <- age.data.1 %>%
  filter(Knower_level == "2" | 
           Knower_level == "CP")%>%
  droplevels()
boxplot(Age ~ Knower_level, data = tmp)
t.test(subset(tmp, Knower_level == "CP")$Age, 
       subset(tmp, Knower_level == "2")$Age, var.equal = TRUE) #no significant difference in mean age for exp. 1

## now for study 2
age.data.2 <- all.data.study2 %>%
  distinct(SID, Age, CP_subset, Knower_level)

tmp <- age.data.2 %>%
  filter(Knower_level == "2" | 
           Knower_level == "CP")%>%
  droplevels()
boxplot(Age ~ Knower_level, data = tmp)
t.test(subset(tmp, Knower_level == "CP")$Age, 
       subset(tmp, Knower_level == "2")$Age, var.equal = TRUE) #significant difference for exp. 2

## collapse over all data
age.data <- bind_rows(age.data.1, age.data.2)

t.test(subset(age.data, CP_subset == "CP")$Age, 
       subset(age.data, CP_subset == "Subset")$Age, var.equal = TRUE)

boxplot(Age ~ Knower_level, data = age.data)

## are 2-knowers older than CP-knowers?
t.test(subset(age.data, CP_subset == "CP")$Age, 
       subset(age.data, Knower_level == "2")$Age, var.equal = TRUE) #over both experiments, CP-knowers are older than 2-knowers

tmp <- age.data %>%
  filter(Knower_level == "2" | 
           Knower_level == "CP")%>%
  droplevels()
boxplot(Age ~ Knower_level, data = tmp)

t.test(subset(tmp, Knower_level == "CP")$Age, 
       subset(tmp, Knower_level == "2")$Age, var.equal = TRUE)

##CP_subset
all.data.study2 %>% 
  distinct(SID, CP_subset, Sex)%>%
  group_by(CP_subset, Sex)%>%
  summarise(n = n())

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

# ...analyses ----
## Descriptives of mean performance for SF and NN
#overall
all.data.study2 %>%
  group_by(CP_subset, Task)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

#count range
all.data.study2 %>%
  group_by(CP_subset, Task, count_range)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

## Test 1: Replication of SF difference between CP and subset
### Make base with age 
sf.cp.base <- glmer(Correct ~ age.c + (1|SID), 
                    family = "binomial", 
                    data = subset(all.data.study2, Task == "SF"))
### Add CP 
sf.cp.kl <- glmer(Correct ~ CP_subset + age.c + (1|SID), 
                    family = "binomial", 
                    data = subset(all.data.study2, Task == "SF"))
#compare 
anova(sf.cp.base, sf.cp.kl, test = 'lrt')
summary(sf.cp.kl)

# ...visualization of CP v. subset on SF, study 2 ----
all.data.study2 %>%
  filter(Task == "SF")%>%
  mutate(Task_item = factor(Task_item), 
         Correct = as.numeric(as.character(Correct)), 
         CP_subset = factor(CP_subset, levels = c("Subset", "CP"), 
                            labels = c("Subset-knowers", "CP-knowers")))%>%
  group_by(Task_item, CP_subset)%>%
  langcog::multi_boot_standard("Correct", na.rm = TRUE) %>%
  ggplot(aes(x = Task_item, y = mean, colour = CP_subset, group= CP_subset, shape = CP_subset)) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "grey", size = .35) +
  geom_point(size = 1.5) + 
  geom_line(size = .35) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0, size = .5) +
  theme_bw(base_size = 8) + 
  theme(panel.grid = element_blank(), 
        legend.position = "right", 
        legend.title = element_blank()) + 
  scale_y_continuous(breaks = seq(0,1,.25), limits = c(0, 1)) + 
  scale_color_brewer(palette = "Paired") + 
  labs(fill = "Knower level", 
       x = "Starting set size", y = "Mean Unit Task performance")
ggsave("Figures/study2_CP_subset_Unit.png", units = "in", width = 3.5, height = 2.25)

## Test 2: Do subset knowers perform above chance overall? 
subset.chance.overall.2 <- glmer(Correct ~ 1 + (1|SID), 
                                 family = "binomial", 
                                 data = subset(all.data.study2, CP_subset == "Subset" & 
                                                 Task == "SF"))
summary(subset.chance.overall.2)

## Test 3: Do subset knowers perform above chance for known numbers?
subset.chance.known.2 <- glmer(Correct ~ 1 + (1|SID), 
                                 family = "binomial", 
                                 data = subset(all.data.study2, CP_subset == "Subset" & 
                                                 Task == "SF" & count_range == "Within"))
summary(subset.chance.known.2)

# ...visualization of SF subset knower and CP knower performance for exp 2 ----
##Subset-knowers only
all.data.study2 %>%
  filter(Task == "SF")%>%
  mutate(Task_item = factor(Task_item), 
         Knower_level.combined = ifelse((Knower_level == "4" | Knower_level == "3" | Knower_level == "5"), "3-, 4-, & 5-knowers", 
                                        ifelse(Knower_level == "CP", "CP knowers", as.character(Knower_level))),
         Knower_level.combined = factor(Knower_level.combined, levels= c("1", "2", "3-, 4-, & 5-knowers", "CP knowers"), 
                                        labels = c("1-knowers", "2-knowers", "3-, 4-, & 5-knowers", "CP knowers")))%>%
  group_by(Task_item, Knower_level.combined)%>%
  langcog::multi_boot_standard("Correct", na.rm = TRUE) %>%
  ggplot(aes(x = Task_item, y = mean, colour = Knower_level.combined, group= Knower_level.combined)) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "grey", size = .35) +
  geom_point(size = 1.5, 
             show.legend = FALSE) + 
  geom_line(size = .35, 
            show.legend = FALSE) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0, size = .35, 
                show.legend = FALSE) +
  theme_bw(base_size = 8) + 
  theme(legend.position = "none", 
        panel.grid = element_blank()) + 
  facet_wrap(~Knower_level.combined, ncol = 4) + 
  langcog::scale_color_solarized("Knower level") +
  labs(x = "Starting set size", y = "Mean Unit Task performance")
ggsave("Figures/study2_nKL_Unit.png", units = "in", width = 5, height = 2)

## Follow up: t-tests against chance for knower levels
study2.ms <- all.data.study2 %>%
  filter(Task == "SF")%>%
  group_by(SID, Knower_level, Task_item)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

#1-knowers - at chance for all items
t.test(subset(study2.ms, Knower_level == "1" & Task_item == "1")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = -0.69, p = 0.5
t.test(subset(study2.ms, Knower_level == "1" & Task_item == "2")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = 1.15, p = 0.27
t.test(subset(study2.ms, Knower_level == "1" & Task_item == "3")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = 0.56, p = 0.58
t.test(subset(study2.ms, Knower_level == "1" & Task_item == "4")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = 0, p = 1
t.test(subset(study2.ms, Knower_level == "1" & Task_item == "5")$mean, mu = .5, var.equal = TRUE) #ns t(13) = 0.56, p = 0.58

#2-knowers - above chance for sets of 1
t.test(subset(study2.ms, Knower_level == "2" & Task_item == "1")$mean, mu = .5, var.equal = TRUE) #sig, t(17) = 3.34, p = .004
t.test(subset(study2.ms, Knower_level == "2" & Task_item == "2")$mean, mu = .5, var.equal = TRUE) #ns; t(17) = -0.32, p = 0.75
t.test(subset(study2.ms, Knower_level == "2" & Task_item == "3")$mean, mu = .5, var.equal = TRUE) #ns; t(17) = -0.32, P = 0.75
t.test(subset(study2.ms, Knower_level == "2" & Task_item == "4")$mean, mu = .5, var.equal = TRUE) #ns; t(17) = 0.37, p = 0.72
t.test(subset(study2.ms, Knower_level == "2" & Task_item == "5")$mean, mu = .5, var.equal = TRUE) #ns; t(16) = 0.37, p = 0.72

#3-knowers - only for sets of 1 
t.test(subset(study2.ms, Knower_level == "3" & Task_item == "1")$mean, mu = .5, var.equal = TRUE) #sig, t(7) = 2.65, p = .03
t.test(subset(study2.ms, Knower_level == "3" & Task_item == "2")$mean, mu = .5, var.equal = TRUE) #ns; t(7) = 1.87, p= .10
t.test(subset(study2.ms, Knower_level == "3" & Task_item == "3")$mean, mu = .5, var.equal = TRUE) #ns; t(7) = 1, p= 0.35
t.test(subset(study2.ms, Knower_level == "3" & Task_item == "4")$mean, mu = .5, var.equal = TRUE) #ns; t(7) = 0.55, p = 0.60
t.test(subset(study2.ms, Knower_level == "3" & Task_item == "5")$mean, mu = .5, var.equal = TRUE) #ns; t(7) = -1.43, p = 0.20

#CP-knowers - only for sets of 1-3
t.test(subset(study2.ms, Knower_level == "CP" & Task_item == "1")$mean, mu = .5, var.equal = TRUE) #sig, t(27) = 11.15, p <.0001
t.test(subset(study2.ms, Knower_level == "CP" & Task_item == "2")$mean, mu = .5, var.equal = TRUE) #sig; t(27) = 8.22, p= < .0001
t.test(subset(study2.ms, Knower_level == "CP" & Task_item == "3")$mean, mu = .5, var.equal = TRUE) #sig; t(27) = 3.86, p= 0.0006
t.test(subset(study2.ms, Knower_level == "CP" & Task_item == "4")$mean, mu = .5, var.equal = TRUE) #ns; t(27) = 0.72, p = 0.48
t.test(subset(study2.ms, Knower_level == "CP" & Task_item == "5")$mean, mu = .5, var.equal = TRUE) #ns; t(27) = 1.22, p = 0.23


## Test 4: Does highest count predict SF for CP or subset knowers? 
###subset knowers first - make base
subset.sf.hc.base <- glmer(Correct ~ age.c + (1|SID), 
                           family = "binomial", 
                           data = subset(all.data.study2, Task == "SF" & CP_subset == "Subset"))
#### add hc
subset.sf.hc.hc <- glmer(Correct ~ highest_count.c + age.c + (1|SID), 
                           family = "binomial", 
                           data = subset(all.data.study2, Task == "SF" & CP_subset == "Subset"))
#### compare
anova(subset.sf.hc.base, subset.sf.hc.hc, test = 'lrt')

###now CP knowers - make base
hc.cp <- all.data.study2 %>%
  filter(!is.na(highest_count))

cp.sf.hc.base <- glmer(Correct ~ age.c + (1|SID), 
                           family = "binomial", 
                           data = subset(hc.cp, Task == "SF" & CP_subset == "CP"))
#### add hc
cp.sf.hc.hc <- glmer(Correct ~ highest_count.c + age.c + (1|SID), 
                         family = "binomial", 
                         data = subset(hc.cp, Task == "SF" & CP_subset == "CP"))
#### compare
anova(cp.sf.hc.base, cp.sf.hc.hc, test = 'lrt')

## Test 5: Do subset-knowers have lower performance on NN than SF overall?
subset.sf.nn.comparison <- glmer(Correct ~ Task + age.c + (1|SID), 
                                 family = "binomial",
                                   data = subset(all.data.study2, CP_subset == "Subset"))
summary(subset.sf.nn.comparison)

## Test 6: Do subset-knowers have lower performance on NN than SF for numbers within their known range?
subset.sf.nn.comparison.within <- glmer(Correct ~ Task + age.c + (1|SID), 
                                 family = "binomial",
                                 data = subset(all.data.study2, CP_subset == "Subset" & count_range == "Within"))
summary(subset.sf.nn.comparison.within)

## Test 7: Do CP-knowers outperform subset knowers on NN?
cp.subset.nn.comparison.base <- glmer(Correct ~ age.c + (1|SID), 
                                 family = "binomial",
                                 data = subset(all.data.study2, Task == "Next_number"))
cp.subset.nn.comparison <- glmer(Correct ~ CP_subset + age.c + (1|SID), 
                             family = "binomial",
                             data = subset(all.data.study2, Task == "Next_number"))
###compare 
anova(cp.subset.nn.comparison.base, cp.subset.nn.comparison, test= 'lrt')
summary(cp.subset.nn.comparison)

## Test 7: Do CP-knowers have lower performance on NN than SF overall?
cp.sf.nn.comparison <- glmer(Correct ~ Task + age.c + (1|SID), 
                                 family = "binomial",
                                 data = subset(all.data.study2, CP_subset == "CP"))
summary(cp.sf.nn.comparison)

##t.test for difference
ms.nn.cp <- all.data.study2 %>%
  filter(CP_subset == "CP")%>%
  group_by(SID, Task)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

effsize::cohen.d(subset(ms.nn.cp, Task == "SF")$mean, 
                 subset(ms.nn.cp, Task == "Next_number")$mean)


## Test 8: Does What comes next predict Unit Task performance? 
### Create a data frame with SF correct and NN
nn.df <- all.data.study2 %>%
  filter(Task == "Next_number")%>%
  select(SID, Task_item, Trial_number, Correct)%>%
  dplyr::rename("nn.correct" = "Correct")

sf.df <- all.data.study2 %>%
  filter(Task == "SF")%>%
  select(SID, Task, Task_item, Trial_number, Correct, highest_count.c, age.c, Knower_level, CP_subset, count_range)%>%
  filter(Trial_number != "1") #we're filtering out the first trial with 1 because we only have 1 trial for 1 in the NN data
#It feels less weird to remove one trial of 1 from Unit rather than duplicating one trial with 1 in NN

#add the Next Number correct data frame to the Unit Task data frame
##this data frame has, for each subject, whether they got an item correct on Unit and Next number
all.df <- left_join(sf.df, nn.df, by = c("SID", "Trial_number"))%>%
  filter(!is.na(nn.correct))

## So now we can do our analyses: Does performance on Next Number predict success for the corresponding item on Unit Task?
#subset-knowers first - build a base predicting from age
nn.sf.predict.subset.base <- glmer(Correct ~ age.c + (1|SID), 
                              family = "binomial", 
                              data = subset(all.df, CP_subset == "Subset"))
# now add nn.correct
nn.sf.predict.subset <- glmer(Correct ~ nn.correct + age.c + (1|SID), 
                                   family = "binomial", 
                                   data = subset(all.df, CP_subset == "Subset"))
#now compare
anova(nn.sf.predict.subset.base, nn.sf.predict.subset, test = 'lrt') #next number does not significantly predict Unit; chisq = 2.57, p = .11
summary(nn.sf.predict.subset)

#CP-knowers next - build a base predicting from age
nn.sf.predict.cp.base <- glmer(Correct ~ age.c + (1|SID), 
                                   family = "binomial", 
                                   data = subset(all.df, CP_subset == "CP"))
# now add nn.correct
nn.sf.predict.cp <- glmer(Correct ~ nn.correct + age.c + (1|SID), 
                              family = "binomial", 
                              data = subset(all.df, CP_subset == "CP"))
#now compare
anova(nn.sf.predict.cp.base, nn.sf.predict.cp, test = 'lrt') #next number does not significantly predict Unit; chisq = 1.57, p = .21
summary(nn.sf.predict.subset)
  
  
  



## follow up: do subset knowers have better performance on NN for numbers within known range? 
sf.within.nn <- glmer(Correct ~ 1 + (1|SID), 
                      family = "binomial", 
                      data = subset(all.data.study2, Task == "Next_number" & 
                                      count_range == "Within" & 
                                      CP_subset == "Subset"))

summary(sf.within.nn)
# ...visualization: SF and NN by CP/Subset ----
all.data.study2 %>%
  mutate(Task_item = factor(Task_item), 
         Correct = as.numeric(as.character(Correct)), 
         Task = factor(Task, levels = c("Next_number", "SF"), 
                       labels = c("Next Number", "Unit Task")), 
         CP_subset = factor(CP_subset, levels = c("Subset", "CP"), 
                            labels = c("Subset-knowers", "CP-knowers")))%>%
  group_by(CP_subset, Task, Task_item)%>%
  langcog::multi_boot_standard("Correct", na.rm = TRUE) %>%
  ggplot(aes(x = Task_item, y = mean, colour = Task, group= Task, shape = Task)) +
  geom_point(size = 1.5) + 
  geom_line(size = .35) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0, size = .35) +
  theme_bw(base_size = 8) + 
  theme(panel.grid = element_blank(), 
        legend.position = "right", 
        legend.title = element_blank()) + 
  facet_wrap(~CP_subset) + 
  scale_y_continuous(breaks = seq(0,1,.25), limits = c(0, 1)) + 
  scale_color_manual(values = task.pal)+ 
  labs(x = "Number queried", y = "Mean task performance")
ggsave("Figures/study2_CPSubset_NNandUnit.png",  units = "in", width = 5, height = 2)

# ...visualization: nKL sf vs. NN comparison, study 2 ----
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
  geom_point(size = 1.5) + 
  geom_line(size = .35) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0, size = .5) +
  theme_bw(base_size = 8) + 
  theme(legend.position = "right", 
        panel.grid = element_blank(), 
        legend.title = element_blank()) + 
  facet_wrap(~Knower_level.combined) + 
  scale_y_continuous(breaks = seq(0,1,.25), limits = c(0, 1)) + 
  scale_color_manual(values = task.pal)+ 
  labs(x = "Number queried", y = "Mean task performance")
ggsave("Figures/study2_nKL_NNandUnit.png", units = "in", width = 5, height = 2)

#Do subset-knowers perform better on NN in comparison to SF overall?
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
t.test(subset(study2.ms, Knower_level == "2" & Task_item == "1")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = -0.69, p = 0.5
t.test(subset(study2.ms, Knower_level == "2" & Task_item == "2")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = 1.15, p = 0.27
t.test(subset(study2.ms, Knower_level == "2" & Task_item == "3")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = 0.56, p = 0.58
t.test(subset(study2.ms, Knower_level == "2" & Task_item == "4")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = 0, p = 1
t.test(subset(study2.ms, Knower_level == "2" & Task_item == "5")$mean, mu = .5, var.equal = TRUE) #ns t(13) = 0.56, p = 0.58

#3-knowers
t.test(subset(study2.ms, Knower_level == "3" & Task_item == "1")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = -0.69, p = 0.5
t.test(subset(study2.ms, Knower_level == "3" & Task_item == "2")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = 1.15, p = 0.27
t.test(subset(study2.ms, Knower_level == "3" & Task_item == "3")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = 0.56, p = 0.58
t.test(subset(study2.ms, Knower_level == "3" & Task_item == "4")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = 0, p = 1
t.test(subset(study2.ms, Knower_level == "3" & Task_item == "5")$mean, mu = .5, var.equal = TRUE) #ns t(13) = 0.56, p = 0.58

#CP-knowers
t.test(subset(study2.ms, Knower_level == "CP" & Task_item == "1")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = -0.69, p = 0.5
t.test(subset(study2.ms, Knower_level == "CP" & Task_item == "2")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = 1.15, p = 0.27
t.test(subset(study2.ms, Knower_level == "CP" & Task_item == "3")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = 0.56, p = 0.58
t.test(subset(study2.ms, Knower_level == "CP" & Task_item == "4")$mean, mu = .5, var.equal = TRUE) #ns; t(13) = 0, p = 1
t.test(subset(study2.ms, Knower_level == "CP" & Task_item == "5")$mean, mu = .5, var.equal = TRUE) #ns t(13) = 0.56, p = 0.58

##Exploratory: Does Highest count predict NN for subset and CP-knowers
nn.data.2 <- all.data.study2 %>%
  filter(Task == "Next_number", 
         !is.na(highest_count))

## subset-knowers first
nn.subset.hc.base <- glmer(Correct ~ 1 + (1|SID), 
                      family = "binomial", 
                      data = subset(nn.data.2, CP_subset == "Subset"))
nn.subset.hc <- glmer(Correct ~ highest_count.c + (1|SID), 
                      family = "binomial", 
                      data = subset(nn.data.2, CP_subset == "Subset"))
anova(nn.subset.hc.base, nn.subset.hc, test = 'lrt') #does not improve fit

## then CP-knowers
nn.cp.hc.base <- glmer(Correct ~ 1 + (1|SID), 
                           family = "binomial", 
                           data = subset(nn.data.2, CP_subset == "CP"), 
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e4)))
nn.cp.hc <- glmer(Correct ~ highest_count.c + (1|SID), 
                      family = "binomial", 
                      data = subset(nn.data.2, CP_subset == "CP"))
anova(nn.cp.hc.base, nn.cp.hc, test = 'lrt') #does improve fit





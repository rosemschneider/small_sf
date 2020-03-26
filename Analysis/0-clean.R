#Data cleaning script for small sf study

rm(list = ls())
library(tidyverse)
library(magrittr)
library(tidylog)
library(stringr)

#filtering function
'%!in%' <- function(x,y)!('%in%'(x,y))

# Study 1 data ----
#load study 1 data
data.raw.study1 <- read.csv("../Data/small_sf_study1.csv")

data.raw.study1 %<>% #filter out pilots right away
  filter(Exclusion_reason != "Pilot")

# ... Exclusions ----
##how many kids pre-exclusions?
data.raw.study1 %>%
  distinct(SID, Age)%>%
  summarise(n = n())

##why are kids being excluded?
data.raw.study1 %>%
  filter(Exclude == 1)%>%
  distinct(SID, Exclusion_reason)%>%
  group_by(Exclusion_reason)%>%
  summarise(n = n())

##exclude these kids 
data.raw.study1 %<>%
  filter(Exclude == 0)

##exclude trials
data.raw.study1 %<>%
  filter(Exclude_trial != 1)

##exclude training trials
data.raw.study1 %<>%
  filter(is.na(Trial_number) | Trial_number != "Training")

# Study 2 data ----
#load raw data
data.raw.study2 <- read.csv("../Data/small_sf_study2.csv")

data.raw.study2 %<>% #filter out pilots right away
  filter(Exclusion_reason != "Pilot")

# ... Exclusions ----
##how many kids pre-exclusions?
data.raw.study2 %>%
  distinct(SID, Age)%>%
  summarise(n = n())

##why are kids being excluded?
data.raw.study2 %>%
  filter(Exclude == 1)%>%
  distinct(SID, Exclusion_reason)%>%
  group_by(Exclusion_reason)%>%
  summarise(n = n())

##exclude these kids 
data.raw.study2 %<>%
  filter(Exclude == 0)

##exclude trials
data.raw.study2 %<>%
  filter(Exclude_trial != 1)

##exclude training
data.raw.study2 %<>%
  mutate(Exclude_NN = ifelse((Task == "Next_number" & Trial_number == "1"), 1, 0))%>%
  filter(is.na(Trial_number) | Trial_number != "Training", 
         Exclude_NN != 1)

# Data manipulations ----
#Highest count
hc.study1 <- data.raw.study1 %>%
  filter(Task == "Highest_count")%>%
  distinct(SID, Response)%>%
  dplyr::rename("highest_count" = "Response")

data.raw.study1 <- left_join(data.raw.study1, hc.study1, by = "SID") #n = 1 does not have highest count

hc.study2 <- data.raw.study2 %>%
  filter(Task == "Highest_count")%>%
  distinct(SID, Response)%>%
  dplyr::rename("highest_count" = "Response")

data.raw.study2 <- left_join(data.raw.study2, hc.study2, by = "SID") #n = 1 does not have highest count

# Mutate data types, rename ---- 
all.data.study1 <- data.raw.study1 %>%
  mutate(highest_count = as.numeric(as.character(highest_count)), 
         age.c = as.vector(scale(Age, center = TRUE, scale = TRUE)), 
         highest_count.c = as.vector(scale(highest_count, center = TRUE, scale = TRUE)))%>%
  filter(Task == "SF") #we only need SF data

all.data.study2 <- data.raw.study2 %>%
  mutate(highest_count = as.numeric(as.character(highest_count)), 
         age.c = as.vector(scale(Age, center = TRUE, scale = TRUE)), 
         highest_count.c = as.vector(scale(highest_count, center = TRUE, scale = TRUE))) %>%
  filter(Task == "SF" | Task == "Next_number")

# Validation ----
#SF1
sf.check <- all.data.study1 %>%
  filter(Task == "SF")%>%
  mutate(Task_item = as.numeric(as.character(Task_item)), 
         Response = as.numeric(as.character(Response)), 
         correct.check = ifelse(Response == Task_item +1, 1, 0), 
         correct.validate = ifelse(Correct == correct.check, 1, 0))%>% # n = 5 errors, so we will replace if they match
  filter(correct.validate != 1)

#replace
all.data.study1 %<>%
  mutate(Task_item = as.numeric(as.character(Task_item)), 
         Response = as.numeric(as.character(Response)), 
         correct.check = ifelse(Response == Task_item +1, 1, 0), 
         correct.validate = ifelse(Correct == correct.check, 1, 0), 
         Correct = correct.check)

#SF2
sf.check <- all.data.study2 %>%
  filter(Task == "SF")%>%
  mutate(Task_item = as.numeric(as.character(Task_item)), 
         Response = as.numeric(as.character(Response)), 
         correct.check = ifelse(Response == Task_item +1, 1, 0), 
         correct.validate = ifelse(Correct == correct.check, 1, 0)) %>%
  filter(correct.validate != 1)#0 errors
#NN
nn.check <- all.data.study2 %>%
  filter(Task == "Next_number")%>%
  mutate(Task_item = as.numeric(as.character(Task_item)), 
         Response = as.numeric(as.character(Response)), 
         correct.check = ifelse(Response == Task_item +1, 1, 0), 
         correct.validate = ifelse(Correct == correct.check, 1, 0))%>%
  filter(correct.validate != 1) #0 errors

# Save and export ----
save(all.data.study1, file = "../Data/all_data_study1.RData")
save(all.data.study2, file = "../Data/all_data_study2.RData")


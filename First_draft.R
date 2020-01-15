# Assignment 1
library(tidyverse)
###########################################
# EDA
###########################################
# Open the data 
data = readRDS("Longnecker.rds")

# Dimensions
dim(data)
colnames(data)

# wierd stuff. First, we see that the, gestational age is terrible, as
# it has an upper tail that does not make sense. Data are terrible. 
table(data$maternal_age)
table(data$gestational_age)


# One woman that has the every pcb not reported. Should we impute the values?
# It shouldn't affect the a
data[is.na(data$pcb_028),]

# albumin is almost a null column. 

# histogram of DDE vs race
hist(data$dde)
ggplot(data = data) +
  geom_histogram(aes(x=dde))+
  facet_wrap(~race)

# relationship between DDE and gestational_age across race
ggplot(data = data) +
  geom_point(aes(x=dde, y=gestational_age, color = race))
# we immediately see that there are outliers that do not really make sense
# we detect them via cook's distance
mod_outliers = lm(gestational_age~dde, data = data)
plot(mod_outliers)  # very close to 1. 
# Race
ggplot(data = data[data$gestational_age<50,]) +
  geom_point(aes(x=dde, y=gestational_age, color = race))         
# Smoking status
ggplot(data = data[data$gestational_age<50,]) +
  geom_point(aes(x=dde, y=gestational_age, color = as.factor(smoking_status))) +
  facet_wrap(~race)+
  theme(legend.position = "bottom")

ggplot(data = data[data$gestational_age<50,]) +
  geom_point(aes(x=pcb_052, y=gestational_age, color = as.factor(smoking_status))) +
  facet_wrap(~race)+
  theme(legend.position = "bottom")

ggplot(data = data[data$gestational_age<50,]) +
  geom_histogram(aes(x=gestational_age, fill= as.factor(smoking_status)))

ggplot(data = data[data$gestational_age<50,]) +
  geom_point(aes(x=cholesterol, y=gestational_age, color = as.factor(smoking_status))) +
  facet_wrap(~race)+
  theme(legend.position = "bottom")

ggplot(data = data[data$gestational_age<50,], aes(x=cholesterol, y=dde)) +
  geom_point(aes(color = race, alpha =0.5)) +
  theme(legend.position = "bottom") +
  geom_smooth(method='lm',formula= y~x)

facet_wrap(~race)+
  theme(legend.position = "bottom")

ggplot(data = data[data$gestational_age<50,], aes(x=triglycerides, y=dde)) +
  geom_point(aes(color = race, alpha =0.5)) +
  theme(legend.position = "bottom") +
  geom_smooth(method='lm',formula= y~x)



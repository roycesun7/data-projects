#data <- read.csv("~/Downloads/patient_tumor_data.csv")
library(dplyr)
library(ggplot2)

data2 = select(data, -3, -7, -8)

data2 %>%
  group_by(Sex, Behavior.code.ICD.O.3) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

race <- data2 %>%
  group_by(Race.recode..W..B..AI..API., Behavior.code.ICD.O.3) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

age <- data2 %>%
  group_by(Age.recode.with..1.year.olds, Behavior.code.ICD.O.3) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

age_site <- data2 %>%
  group_by(Site.recode.ICD.O.3.WHO.2008, Age.recode.with..1.year.olds, Behavior.code.ICD.O.3) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

site <- data2 %>%
  group_by(Site.recode.ICD.O.3.WHO.2008, Behavior.code.ICD.O.3) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

sex_behavior <- data2 %>%
  group_by(Sex, Behavior.code.ICD.O.3) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

sex_site <- data2 %>%
  group_by(Sex, Site.recode.ICD.O.3.WHO.2008) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

sex_site_behavior <- data2 %>%
  group_by(Sex, Site.recode.ICD.O.3.WHO.2008, Behavior.code.ICD.O.3) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

sum(overall$n)

ggplot(race, aes(x=Race.recode..W..B..AI..API., y=freq, fill=Behavior.code.ICD.O.3)) + geom_bar(stat="identity")
ggplot(site, aes(x=Site.recode.ICD.O.3.WHO.2008, y=freq, fill=Behavior.code.ICD.O.3)) + geom_bar(stat="identity")
ggplot(sex, aes(x=Sex, y=freq, fill=Behavior.code.ICD.O.3)) + geom_col(position="identity")
ggplot(age, aes(x=Age.recode.with..1.year.olds, y=freq, fill=Behavior.code.ICD.O.3)) + geom_bar(stat="identity")

ggplot(sex_site, aes(x=Sex, y=freq, fill=Site.recode.ICD.O.3.WHO.2008)) + geom_bar(stat="identity")
ggplot(sex_site_behavior, aes(x=Sex, y=freq, fill=Site.recode.ICD.O.3.WHO.2008)) + geom_bar(stat="identity")

ggplot(sex, aes(x=Behavior.code.ICD.O.3, y=freq, fill=Sex)) + geom_bar(stat="identity")
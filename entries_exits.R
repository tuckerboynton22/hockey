library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)

setwd("/Users/tuckerboynton/Desktop")

data_2020 <- read_excel("teams_2020.xlsx")
data_2019 <- read_excel("teams_2019.xlsx")
data_2018 <- read_excel("teams_2018.xlsx")

data_2020 %>%
  ggplot(aes(x = `Entries w/ Passes`, y = Shots)) +
  geom_point()

data_2020 %>%
  ggplot(aes(x = `5v4 Entries`, y = `5v4 Shots`)) +
  geom_point()

fit <- lm(Shots ~ + Carries + `Dump-ins`, data = data_2020)
fit2 <- lm(Shots ~ , data = data_2020)


summary(fit)
summary(fit2)
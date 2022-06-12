# DSM 5008 FINAL

library(tidyverse)
library(dplyr)
library(ggplot2)

df <- read.csv("/Users/berkayerbayat/Desktop/GIT/GitforR/msc_projects/datasets/wdbc.data",header=F)

head(df)
colnames(df)[1:2] <- c("ID", "diagnosis")

df %>% 
  select()


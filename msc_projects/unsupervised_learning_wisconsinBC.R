# DSM 5008 FINAL

library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("pastecs")
library(pastecs)
library(corrplot)
library(magrittr)
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

df <- read.csv("/Users/berkayerbayat/Desktop/GIT/GitforR/msc_projects/datasets/wdbc.data",header=F)

head(df)
colnames(df)[1:2] <- c("ID", "diagnosis")

new_df <- df %>% 
  select(diagnosis, V3:V12)
head(new_df)

numeric_summary <- function(data){
  #requires pastect package
  data %>% 
    select_if(is.numeric) %>% 
    stat.desc() %>% 
    t()
}

numeric_summary(new_df)

new_df %<>% 
  na.omit()

num_data <- select_if(new_df,is.numeric)

res <- cor(num_data)

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

chart.Correlation(res, histogram=TRUE, pch=19)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)

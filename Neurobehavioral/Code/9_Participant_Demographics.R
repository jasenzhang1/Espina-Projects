library(tidyverse)
library(gridExtra)
library(geepack)
library(scales)

dir <- getwd()
codes_dir <- paste(substr(dir,1, nchar(dir)-4), "Code/00_Codes.R", sep = '')
source(codes_dir)


raw_data_dir <- paste(substr(dir,1, nchar(dir)-4), "Data/raw_data.200713.RData", sep = '')
load(raw_data_dir)

df_demographics <- raw_data %>%
  select(b_age, c_age, c_race, c_gender, child1604, child1608) %>%
  filter(! is.na(child1604)) %>%
  filter(child1608 == 2)

write.csv(df_demographics, 'C:/Users/Jasen Zhang/Dropbox/Summer 2021/Georgia/Figures/Demographics_v2.csv')

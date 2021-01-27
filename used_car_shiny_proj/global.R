library(dplyr)
library(ggplot2)
library(shiny)
library(DT)

df = read.csv('used_car_sales.csv')
names(df) <- tolower(names(df))
colnames(df)

sell_years <- unique(df$yearsold)

#unique(df$model)


#test = df %>% group_by(make) %>% summarize(make_count=n()) %>% arrange(desc(make_count)) %>% .[1:20,]

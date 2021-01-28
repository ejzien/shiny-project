library(dplyr)
library(ggplot2)
library(shiny)
library(DT)

df_initial = read.csv('used_car_sales.csv')
df = df_initial

names(df) <- tolower(names(df))
df <- df %>% mutate(mileage_group = ifelse(mileage < 25000, 'Less Than 25000','N/A'))
df <- df %>% mutate(mileage_group = ifelse(mileage > 50000, '25000-50000',mileage_group))
df <- df %>% mutate(mileage_group = ifelse(mileage > 75000, '50000-75000',mileage_group))
df <- df %>% mutate(mileage_group = ifelse(mileage > 100000, '75000-100000',mileage_group))
df <- df %>% mutate(mileage_group = ifelse(mileage > 125000, '100000-125000',mileage_group))
df <- df %>% mutate(mileage_group = ifelse(mileage > 150000, '125000-150000',mileage_group))
df <- df %>% mutate(mileage_group = ifelse(mileage > 175000, '150000-175000',mileage_group))
df <- df %>% mutate(mileage_group = ifelse(mileage > 200000, '175000-200000',mileage_group))
df <- df %>% mutate(mileage_group = ifelse(mileage > 225000, '200000-225000',mileage_group))
df <- df %>% mutate(mileage_group = ifelse(mileage > 250000, '225000-250000',mileage_group))
df <- df %>% mutate(mileage_group = ifelse(mileage > 250000, '250000+',mileage_group))

df <- df %>% mutate(year = gsub('0000','',as.character(year)))
df <- df %>% mutate(year = ifelse(nchar(year) == 2, paste0('19',year),year))


unique(df$mileage_group)

#df = df %>% filter(mileage>=25000)
colnames(df)

sell_years <- unique(df$yearsold)

#unique(df$model)


test = df %>% group_by(make,model) %>% summarize(total_count=n()) %>% arrange(desc(total_count)) %>% filter(total_count>=500)
unique(test$model)
df = inner_join(df,test,by=c("make","model"))

cyl_df = df %>% group_by(numcylinders) %>% summarize(total_count=n()) %>% arrange(desc(total_count)) %>% filter(total_count>1)
df = inner_join(df,cyl_df,by=c("numcylinders"))


unique(df$model)


count_df = df %>% group_by(drivetype) %>% summarize(total_count=n()) %>% arrange(desc(total_count))


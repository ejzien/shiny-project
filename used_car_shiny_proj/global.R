library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(scales)

df_initial = read.csv('./data/used_car_sales.csv')
df = df_initial

#making all col names lowercase
names(df) <- tolower(names(df))
#having a backup year column
df$year_initial = df$year

#function for grouping the mileage in groups of 25k miles
get_mileage_group <- function(m) {
  output=c()
  for (i in 1:length(m)){
    if(m[i]<25000){
      output[i]='Less Than 25000'} else if (m[i]<50000){
        output[i]='25000-50000'
      } else if (m[i]<75000){
        output[i]='50000-750000'
      } else if (m[i]<100000){
        output[i]='75000-100000'
      } else if (m[i]<125000){
        output[i]='100000-125000'
      } else if (m[i]<150000){
        output[i]='125000-150000'
      } else if (m[i]<175000){
        output[i]='150000-175000'
      } else if (m[i]<200000){
        output[i]='175000-200000'
      } else if (m[i]<225000){
        output[i]='200000-225000'
      } else if (m[i]<250000){
        output[i]='225000-250000'
      } else {
        output[i] = 'Greater Than 250000'
      }
  }
  return(output)
}

#function for adding standardization of year values
adjust_year <- function(y) {
  output=c()
  for (i in 1:length(y)){
    temp_y = y[i]
    temp_y = as.character(temp_y)
    temp_y = gsub('0000','',temp_y)
    if (nchar(temp_y) == 2){
      temp_y = paste0('19',temp_y)
      }
    temp_y = as.numeric(temp_y)
    output[i] = temp_y
  }
  return(output)
}

#adjusting columns and filtering out years that don't make sense
df = df %>% mutate(mileage_group=get_mileage_group(mileage),year=adjust_year(year_initial))
df = df %>% filter(year>1950,year<=2020)


#finding which make/model combinations have >500 unique sales to make sure all sales have a decent sample
test = df %>% group_by(make,model) %>% summarize(total_count=n()) %>% arrange(desc(total_count)) %>% filter(total_count>=500)
unique(test$model)
print(sum(df$pricesold))
df = inner_join(df,test,by=c("make","model"))

#filtering out cylinder counts where it did not appear to be any comparison for
cyl_df = df %>% group_by(numcylinders) %>% summarize(total_count=n()) %>% arrange(desc(total_count)) %>% filter(total_count>1)
df = inner_join(df,cyl_df,by=c("numcylinders"))
#dim(df)

#getting filter values
sell_years <- unique(df$yearsold)
models <- unique(df$model)
makes <- unique(df$make)

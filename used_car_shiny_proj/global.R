library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(scales)
library(plotly)


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
      output[i]=25000} else if (m[i]<50000){
        output[i]=50000
      } else if (m[i]<75000){
        output[i]=75000
      } else if (m[i]<100000){
        output[i]=100000
      } else if (m[i]<125000){
        output[i]=125000
      } else if (m[i]<150000){
        output[i]=150000
      } else if (m[i]<175000){
        output[i]=175000
      } else if (m[i]<200000){
        output[i]=200000
      } else if (m[i]<225000){
        output[i]=225000
      } else if (m[i]<250000){
        output[i]=250000
      } else {
        output[i] = 275000
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

get_decade <- function(y) {
  output=c()
  for (i in 1:length(y)){
    temp_y = y[i]
    if (temp_y<1960){
      output[i] = '1950s'
    } else if(temp_y<1970){
      output[i] = '1960s'
    } else if(temp_y<1980){
      output[i] = '1970s'
    } else if(temp_y<1990){
      output[i] = '1980s'
    } else if(temp_y<2000){
      output[i] = '1990s'
    } else if(temp_y<2020){
      output[i] = '2000s'
    } else {
      output[i] = '2010s'
    }
  }
  return(output)
}

get_drive_type <- function(d) {
  output=c()
  for (i in 1:length(d)){
    if(grepl('4wd',tolower(d[i]))){
      output[i]='4 Wheel Drive'} else if (grepl('4dr',tolower(d[i]))){
        output[i]='4 Wheel Drive'} else if (grepl('4x4',tolower(d[i]))){
          output[i]='4 Wheel Drive'} else if (grepl('4dr',tolower(d[i]))){
            output[i]='4 Wheel Drive'} else if (grepl('2dr',tolower(d[i]))){
              output[i]='2 Wheel Drive'} else if (grepl('2wd',tolower(d[i]))){
                output[i]='2 Wheel Drive'} else if (grepl('2 wheel drive',tolower(d[i]))){
                  output[i]='2 Wheel Drive'} else if (grepl('four wheel drive',tolower(d[i]))){
                    output[i]='4 Wheel Drive'} else if (grepl('5 dr',tolower(d[i]))){
                      output[i]='5 Wheel Drive'} else if (grepl('rwd',tolower(d[i]))){
                        output[i]='Rear Wheel Drive'}  else if (grepl('real wheel drive',tolower(d[i]))){
                          output[i]='Rear Wheel Drive'} else if (grepl('5dr',tolower(d[i]))){
                            output[i]='5 Wheel Drive'} else if (grepl('rear drive',tolower(d[i]))){
                              output[i]='Rear Wheel Drive'} else if (grepl('awd',tolower(d[i]))){
                                output[i]='All Wheel Drive'} else if (grepl('all wheel drive',tolower(d[i]))){
                                  output[i]='All Wheel Drive'} else if (grepl('6 wheel drive',tolower(d[i]))){
                                    output[i]='6 Wheel Drive'} else if (grepl('six wheel drive',tolower(d[i]))){
                                      output[i]='6 Wheel Drive'} else if (grepl('4 wheel drive',tolower(d[i]))){
                                        output[i]='4 Wheel Drive'} else if (grepl('rear drive',tolower(d[i]))){
                                          output[i]='Rear Wheel Drive'} else if (grepl('rear wheel',tolower(d[i]))){
                                            output[i]='Rear Wheel Drive'} else if (grepl('2 wd',tolower(d[i]))){
                                              output[i]='2 Wheel Drive'} else if (grepl('fwd',tolower(d[i]))){
                                                output[i]='Front Wheel Drive'} else if (grepl('4 wheel drive',tolower(d[i]))){
                                                  output[i]='4 Wheel Drive'} else if (grepl('2 wheel',tolower(d[i]))){
                                                    output[i]='2 Wheel Drive'} else if (grepl('2wheel',tolower(d[i]))){
                                                      output[i]='2 Wheel Drive'} else if (grepl('2 wheel',tolower(d[i]))){
                                                        output[i]='2 Wheel Drive'} else if (grepl('front wheel drive',tolower(d[i]))){
                                                          output[i]='2 Wheel Drive'} else {
              output[i] = 'remove'
            }
  }
  return(output)
}

#adjusting columns and filtering out years that don't make sense
df = df %>% mutate(mileage_group=get_mileage_group(mileage),year=adjust_year(year_initial),drive_type=get_drive_type(drivetype),numcylinders=paste(numcylinders,'Cyl'))
df = df %>% mutate(decade=get_decade(year))
df = df %>% filter(year>1950,year<=2020,mileage<777777,drive_type!='remove')


#finding which make/model combinations have >500 unique sales to make sure all sales have a decent sample
test = df %>% group_by(make,model) %>% summarize(total_count=n()) %>% arrange(desc(total_count)) %>% filter(total_count>=500)
unique(test$model)
df = inner_join(df,test,by=c("make","model"))


#filtering out cylinder counts where it did not appear to be any comparison for
cyl_df = df %>% group_by(numcylinders) %>% summarize(total_count=n()) %>% arrange(desc(total_count)) %>% filter(total_count>1)
df = inner_join(df,cyl_df,by=c("numcylinders"))
#dim(df)

#getting filter values
sell_years <- unique(df$yearsold)
makes <- unique(df$make)
models <- unique(df$model)
years = sort(unique(df$year))
#models <- df %>% filter(make==input$make) %>% select(model) %>% unique()

# unique(df$drivetype)
colnames(df)
dims = c('Make'='make','Model'='model','Year Sold'='yearsold','Mileage Group'='mileage_group',
           'Car Year'='year','Number Of Cylinders'='numcylinders','Drive Type'='drive_type')
color_vals = c('Model'='model','Make'='make','Year Sold'='yearsold','Mileage Group'='mileage_group',
               'Car Year'='year','Number Of Cylinders'='numcylinders','Drive Type'='drive_type')

rename_vec = c('year'='Car Year','model'='Model','make'='Make','avg_sale_price'='Average Sale Price',
               'max_sale_price'='Maximum Sale Price','min_sale_price'='Minimum Sale Price',
               'avg_mileage'='Average Car Mileage','unique_sales'='Total Cars Sold',
               'mileage_group'='Mileage Group','yearsold','Year Sold','numcylinders'='Number Of Cylinders',
               'drive_type'='Drive Type')


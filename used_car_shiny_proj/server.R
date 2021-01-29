#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output){
    
    # makes_by_year <- reactive({
    #     df %>% filter(yearsold==input$year_sold) %>%
    #         group_by(yearsold,make) %>% 
    #         summarize(make_count=n(),avg_sale=sum(pricesold)) %>% arrange(make)
    # })
    
    year_make_data <- reactive({
        df %>% group_by(yearsold,model,make) %>% 
            summarize(avg_sale_price=mean(pricesold),max_sale_price=max(pricesold),min_sale_price=min(pricesold),unique_sales=n_distinct(id)) %>% arrange(make)
    })
    
    by_model <- reactive({
        df %>% filter(make==input$make) %>% group_by(model) %>% 
            summarize(avg_sale_price=mean(pricesold),max_sale_price=max(pricesold),
                      min_sale_price=min(pricesold),avg_mileage=mean(mileage),max_mileage=max(mileage),
                      min_mileage=min(mileage),unique_sales=n_distinct(id))
    })
    
    output$most_common_makes <- DT::renderDataTable({
        DT::datatable(makes_by_year())
    }
    )
    
    output$year_line <- renderPlot({year_make_data() %>% ggplot(aes(yearsold,avg_sale_price)) + 
            geom_line(aes(group=model,color=model),show.legend=FALSE) + labs(x='Year Sold',y='Average Sale Price') + 
            scale_y_continuous(labels=dollar_format(prefix="$")) + scale_x_continuous(breaks = c(2018, 2019, 2020)) + 
            ggtitle('Average Price Of Make Per Year')})
    
    output$raw_overall_data <- DT::renderDataTable({
        DT::datatable(year_make_data(),rownames=F,colnames=c('Year Sold','Model','Make','Average Sale Price','Maximum Sale Price','Minimum Sale Price','Total Cars Sold')) %>%
            formatCurrency(columns=c('avg_sale_price','max_sale_price','min_sale_price'))
        
    }
    )
    
    output$make_tab_table <- DT::renderDataTable({
        DT::datatable(by_model(),rownames=F,
                      colnames=c('Model','Average Sale Price','Max Sale Price','Min Sale Price','Average Mileage','Max Mileage','Min Mileage','Total Cars Sold')) %>%
            formatCurrency(columns=c('avg_sale_price','min_sale_price','max_sale_price'))
        
    }
    )
    
    
})

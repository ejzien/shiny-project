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
    
    output$year_bar <- renderPlot({df %>% group_by(make,yearsold) %>% summarise(avg_sale_price=mean(pricesold)) %>% ggplot(aes(factor(yearsold),avg_sale_price)) + 
            geom_bar(aes(group=make,color=make),position='dodge',stat='identity',show.legend=TRUE) + labs(x='Year Sold',y='Average Sale Price') + 
            scale_y_continuous(labels=dollar_format(prefix="$")) + 
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
    
    output$milelage_plot_make <- renderPlot({df %>% filter(make==input$make) %>% group_by(mileage_group) %>%
            summarise(avg_price_sold=mean(pricesold)) %>% ggplot(aes(mileage_group,avg_price_sold)) + 
            geom_bar(aes(),stat='identity') + labs(x='Mileage Group',y='Average Car Sale Price') + 
            scale_y_continuous(labels=dollar_format(prefix="$")) + scale_x_continuous(breaks=seq(25000, 275000, by=25000)) + 
            ggtitle('Price By Mileage Group')})
    
    output$milelage_box_make <- renderPlot({df %>% filter(make==input$make)  %>% ggplot(aes(factor(mileage_group),pricesold)) + 
            geom_boxplot() + labs(x='Mileage Group',y='Average Car Sale Price') + 
            scale_y_continuous(labels=dollar_format(prefix="$"))+ 
            ggtitle('Price By Mileage Group Box')})
    
    
    output$year_made_plot_make <- renderPlot({df %>% filter(make==input$make) %>% ggplot(aes(year,pricesold)) + 
            geom_point(aes(color=model)) + labs(x='Year Car Was Made',y='Car Sale Price') + 
            scale_y_continuous(labels=dollar_format(prefix="$")) + scale_x_continuous(breaks = seq(1950, 2020, by=10)) + 
            ggtitle('Price By Car Year')})
    
    output$mileage_line_make <- renderPlot({df %>% filter(make==input$make) %>% group_by(model,mileage_group) %>%
            summarise(avg_price_sold=mean(pricesold)) %>% ggplot(aes(mileage_group,avg_price_sold)) +
            geom_line(aes(group=model,color=model)) + labs(x='Mileage',y='Average Sale Price') + 
            scale_y_continuous(labels=dollar_format(prefix="$")) + scale_x_continuous(breaks=seq(25000, 275000, by=25000)) + 
            ggtitle('Mile Line')})
    
    output$year_line_make <- renderPlot({df %>% filter(make==input$make) %>% group_by(model,year) %>%
            summarise(avg_price_sold=mean(pricesold)) %>% ggplot(aes(year,avg_price_sold)) +
            geom_line(aes(group=model,color=model)) + labs(x='Year',y='Average Sale Price') + 
            scale_y_continuous(labels=dollar_format(prefix="$")) + scale_x_continuous(breaks = seq(1950, 2020, by=10)) + 
            ggtitle('Year Line')})
    
    
})


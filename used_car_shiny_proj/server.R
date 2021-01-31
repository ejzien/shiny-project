#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output, session){
    observe({
        if (input$make=='All'){
            updateSelectizeInput(session,"model",choices=df%>%select(model)%>%unique(),selected='Camry')
            #df%>%select(model)%>%first()
        } else {
            updateSelectizeInput(session,"model",choices=df%>%filter(make==input$make)%>% select(model)%>%unique(),selected=df%>%filter(make==input$make)%>% select(model)%>%first())
        }
    })
    
    # makes_by_year <- reactive({
    #     df %>% filter(yearsold==input$year_sold) %>%
    #         group_by(yearsold,make) %>% 
    #         summarize(make_count=n(),avg_sale=sum(pricesold)) %>% arrange(make)
    # })
    
    year_make_data <- reactive({
        df %>% group_by(yearsold,model,make) %>% 
            summarize(avg_sale_price=mean(pricesold),max_sale_price=max(pricesold),min_sale_price=min(pricesold),avg_mileage=mean(mileage),unique_sales=n_distinct(id)) %>% arrange(make)
    })
    
    by_model <- reactive({
        df %>% filter(make==input$make) %>%group_by(model) %>% 
            summarize(avg_sale_price=mean(pricesold),max_sale_price=max(pricesold),
                      min_sale_price=min(pricesold),avg_mileage=mean(mileage),unique_sales=n_distinct(id))
    })
    
    filter_models <- reactive({
        if (input$make=='All') {
            df %>% filter(model%in%input$model)
        } else {
            df %>% filter(make==input$make,model%in%input$model)
        }
    })
    
    custom_tab_filter <- reactive({
        if ('All' %in% input$makes2){
            df %>% filter(year>=input$year_min,year<=input$year_max,sapply(c(pricesold),dollar_format())>=input$price_min,sapply(c(pricesold),dollar_format())<=input$price_max)
        } else {
            df %>% filter(make%in%input$makes2,year>=input$year_min,year<=input$year_max,sapply(c(pricesold),dollar_format())>=input$price_min,sapply(c(pricesold),dollar_format())<=input$price_max)
        }
    })
    
    custom_tab_table <- reactive({
        dimensions = input$dimensions
        
        grouped_df = custom_tab_filter() %>% group_by(.dots=c(dimensions)) %>%
                summarise(avg_sale_price=mean(pricesold),max_sale_price=max(pricesold),min_sale_price=min(pricesold),avg_mileage=mean(mileage),unique_sales=n_distinct(id))
    })
    
    output$year_bar <- renderPlotly({df %>% group_by(make,yearsold) %>% summarise(avg_sale_price=mean(pricesold)) %>% ggplot(aes(make,avg_sale_price)) + 
            geom_bar(aes(group=yearsold,fill=factor(yearsold)),width=0.6,position='dodge',stat='identity',show.legend=TRUE) + labs(x='Car Make',y='Average Sale Price',fill="Yearsold") + 
            scale_y_continuous(labels=dollar_format(prefix="$")) + 
            ggtitle('Average Price Of Make Per Year') + theme(plot.title = element_text(hjust = 0.5))})
    
    
    output$mileage_scatter_plot_price <- renderPlot({df %>% ggplot(aes(miles_thousands,pricesold)) +
            geom_point(aes(color=factor(decade)),position="jitter") + labs(color='Decade',x='Car Mileage (Thousands Of Miles)',y='Car Sale Price') +
            ggtitle('Price By Car Mileage') + theme(plot.title = element_text(hjust = 0.5)) + ylim(0, 100000) + xlim(25, 150)})


    
    require(scales)
    output$milelage_box_make <- renderPlot({filter_models()  %>% ggplot(aes(factor(mileage_group),pricesold)) +
            geom_boxplot(aes(color=factor(mileage_group))) + labs(x='Mileage Group',y='Average Car Sale Price',color='Mileage Group (Thousands of Miles)') +
            scale_y_continuous(labels=dollar_format(prefix="$")) +
            ggtitle('Price By Mileage Group') + theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_blank(),axis.title.x=element_blank()) + facet_wrap(~model)})
    
    
    output$year_made_plot_make <- renderPlot({filter_models() %>% ggplot(aes(year,pricesold)) + 
            geom_point(aes(color=model)) + labs(x='Year Car Was Made',y='Car Sale Price',color='Model') + 
            scale_y_continuous(labels=dollar_format(prefix="$")) + scale_x_continuous(breaks = seq(1950, 2020, by=10)) + 
            ggtitle('Price By Car Year')+ theme(plot.title = element_text(hjust = 0.5))})
    
    output$year_made_line_make <- renderPlotly({filter_models() %>% group_by(model,year) %>%
            summarise(avg_price_sold=mean(pricesold),total_count=n_distinct(id)) %>% filter(total_count>5) %>% ggplot(aes(year,avg_price_sold)) +
            geom_line(aes(group=model,color=model)) + labs(x='Year',y='Average Sale Price') +
            scale_y_continuous(labels=dollar_format(prefix="$")) + scale_x_continuous(breaks = seq(1950, 2020, by=10)) + 
            ggtitle('Avg Sale Price By Year Car Was Made')+ theme(plot.title = element_text(hjust = 0.5))})
    
    output$dt_bar_model <- renderPlotly({filter_models() %>% group_by(model,drive_type) %>%
            summarise(avg_sale_price=mean(pricesold),dt_count=n_distinct(id)) %>% filter(dt_count>100) %>%
            ggplot(aes(drive_type,avg_sale_price)) + 
            geom_bar(aes(group=model,fill=factor(model)),width=0.6,position='dodge',stat='identity',show.legend=TRUE) +
            labs(x='Car Engine Type',y='Average Sale Price',fill="Model") + 
            scale_y_continuous(labels=dollar_format(prefix="$")) + 
            ggtitle('Average Price Of Engine Type') + theme(plot.title = element_text(hjust = 0.5))})
    
    output$cyl_bar_model <- renderPlotly({filter_models() %>% group_by(model,numcylinders) %>%
            summarise(avg_sale_price=mean(pricesold),cyl_count=n_distinct(id)) %>%
            ggplot(aes(numcylinders,avg_sale_price)) + 
            geom_bar(aes(group=model,fill=factor(model)),width=0.6,position='dodge',stat='identity',show.legend=TRUE) +
            labs(x='Car Cylinders',y='Average Sale Price',fill="Model") + 
            scale_y_continuous(labels=dollar_format(prefix="$")) + 
            ggtitle('Average Price Of Car Cylinders') + theme(plot.title = element_text(hjust = 0.5))})
    
    
    output$custom_raw <- DT::renderDataTable({
        DT::datatable(custom_tab_table(),rownames=F
                      ) %>% formatCurrency(columns=c('avg_sale_price','max_sale_price','min_sale_price')) %>% formatRound(columns=c('avg_mileage'))
    }
    )
    
})


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
    
    makes_by_year <- reactive({
        df %>% filter(yearsold==input$year_sold) %>%
            group_by(yearsold,make) %>% 
            summarize(make_count=n(),avg_sale=sum(pricesold),distinct_sales=n_distinct(ID)) %>% arrange(desc(make_count))
    })
    
    year_line_graph <- reactive({
        df %>%
            group_by(yearsold,make) %>% summarize(avg_sale=sum(pricesold)) %>% ggplot(aes(yearsold,avg_sale)) + 
            geom_line(aes(group=make,color=make)) + labs(x='Year Sold',y='Average Sale Price') + 
            scale_y_continuous(labels=dollar_format(prefix="$")) + scale_x_continuous(breaks = c(2018, 2019, 2020))
    })
    
    
    # output$data.table <- renderGvis(
    #     gvisGeoChart(state_stat, "state.name", input$year_sold,
    #                  options=list(region="US", displayMode="regions", 
    #                               resolution="provinces",
    #                               width="auto", height="auto"))
    # )
    
    #output$hist <- renderGvis(
    #    gvisHistogram(state_stat[,input$selected, drop=FALSE]))
    #output$table <- DT::renderDataTable({
    #    DT::datatable(state_stat, rownames=FALSE) %>% 
    #        DT::formatStyle(input$selected,  
    #                        background="skyblue", fontWeight='bold')
        # Highlight selected column using formatStyle
    #})
    
    output$most_common_makes <- DT::renderDataTable({
        DT::datatable(makes_by_year())
    }
    )
    
    output$year_line <- renderPlot({year_line_graph()})
    
    output$avgBox <- renderInfoBox(
        infoBox('hello',
                mean(df[,'pricesold']), 
                icon = icon("calculator"), fill = TRUE))
    
    
})
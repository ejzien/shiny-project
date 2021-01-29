library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title = "My Dashboard"),
    dashboardSidebar(
        sidebarUserPanel(
            "Ethan Zien"
            #image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg")
        ),
        sidebarMenu(
            menuItem("Overall Metrics", tabName = "Tab1", icon = icon("map")),
            menuItem("Make Based Metrics", tabName = "Tab2", icon = icon("database")),
            menuItem("Price Range Metrics", tabName = "Tab3", icon = icon("database"))
        )
        # selectizeInput("year_sold",
        #                "Select Item to Display",
        #                sell_years)
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "Tab1",
                    #fluidRow(fluidRow(plotOutput("year_line", height = "300px"))),
                    fluidRow(fluidRow(align="center",plotlyOutput("year_bar", height = "500px",width=1200))),
                    fluidRow(box(DT::dataTableOutput("raw_overall_data"),width=25))
                             #infoBoxOutput("maxBox"),
                             #infoBoxOutput("minBox"),
                             #infoBoxOutput("avgBox")),
            ),
            tabItem(tabName = "Tab2",
                    selectizeInput("make",
                                   "Make",
                                   makes),
                    selectizeInput("model","Model",choices=models,multiple=T,selected=c("Bronco","F150")),
                    fluidRow(fluidRow(plotOutput("milelage_box_make"))),
                    #fluidRow(fluidRow(plotOutput("mileage_scatter_plot_make"))),
                    #fluidRow(fluidRow(plotOutput("milelage_box_make"))),
                    fluidRow(fluidRow(plotlyOutput("year_made_line_make"),height=25))
                    #fluidRow(fluidRow(plotlyOutput("year_mileage_plot_make"))),
                    #fluidRow(fluidRow(plotOutput("year_line_make"))),
                    #fluidRow(box(DT::dataTableOutput("make_tab_table"),width=25))
            ),
            tabItem(tabName = "Tab3",
                    selectizeInput("price_min",
                                   "Min Price",
                                   sapply(seq(0, 375000, by=25000),dollar_format())),
                    selectizeInput("price_max",
                                   "Max Price",
                                   sapply(seq(0, 375000, by=25000),dollar_format()))
                    #fluidRow(fluidRow(plotOutput("milelage_box_make"))),
                    #fluidRow(fluidRow(plotOutput("mileage_scatter_plot_make"))),
                    #fluidRow(fluidRow(plotOutput("milelage_box_make"))),
                    #fluidRow(fluidRow(plotlyOutput("year_made_line_make"),height=25))
                    #fluidRow(fluidRow(plotlyOutput("year_mileage_plot_make"))),
                    #fluidRow(fluidRow(plotOutput("year_line_make"))),
                    #fluidRow(box(DT::dataTableOutput("make_tab_table"),width=25))
            )
        )
    )))
    

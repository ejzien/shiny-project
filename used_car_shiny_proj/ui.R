library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title = "My Dashboard"),
    dashboardSidebar(
        sidebarUserPanel(
            "Ethan Zien"
            #image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg")
        ),
        sidebarMenu(
            menuItem("Overall Metrics", tabName = "Tab1", icon = icon("database")),
            menuItem("Make Based Metrics", tabName = "Tab2", icon = icon("database")),
            menuItem("Tab 3", tabName = "Tab3", icon = icon("database"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "Tab1",
                    #fluidRow(fluidRow(plotOutput("year_line", height = "300px"))),
                    fluidRow(align="center",plotlyOutput("year_bar", height = "500px",width=1200)),
                    fluidRow(box(DT::dataTableOutput("raw_overall_data"),width=25))
                             #infoBoxOutput("maxBox"),
                             #infoBoxOutput("minBox"),
                             #infoBoxOutput("avgBox")),
            ),
            tabItem(tabName = "Tab2",
                    fluidRow(column(width=2,selectizeInput("make",
                                   "Make",
                                   c(makes,c('All')))),
                             column(width=6,selectizeInput("model",
                                                           "Model",
                                                           choices=models,
                                                           multiple=T,
                                                           selected=c("Bronco","F150"))),
                    ),
                    fluidRow(plotOutput("milelage_box_make")),
                    #fluidRow(fluidRow(plotOutput("mileage_scatter_plot_make"))),
                    #fluidRow(fluidRow(plotOutput("milelage_box_make"))),
                    fluidRow(plotlyOutput("year_made_line_make"),height=25),
                    fluidRow(plotlyOutput("dt_bar_model"),height=25),
                    fluidRow(plotlyOutput("cyl_bar_model"),height=25)
                    #fluidRow(box(DT::dataTableOutput("tab_three_raw"),width=25))
                    #fluidRow(fluidRow(plotlyOutput("year_mileage_plot_make"))),
                    #fluidRow(fluidRow(plotOutput("year_line_make"))),
                    #fluidRow(box(DT::dataTableOutput("make_tab_table"),width=25))
            ),
            tabItem(tabName = "Tab3",
                    fluidRow(column(width=2,selectizeInput("price_min",
                                                           "Min Price",
                                                           sapply(seq(0, 375000, by=5000),dollar_format()))),
                             column(width=2,selectizeInput("price_max",
                                                           "Max Price",
                                                           sapply(seq(0, 375000, by=5000),dollar_format()),
                                                           selected=sapply(c(375000),dollar_format()))),
                             column(width=2,selectizeInput("year_min",
                                                           "Min Year",
                                                           years)),
                             column(width=2,selectizeInput("year_max",
                                                           "Max Year",
                                                           years,
                                                           selected=2020)),
                             column(width=2,selectizeInput("makes2",
                                                           "Make",
                                                           c(c('All'),makes),
                                                           multiple=T,
                                                           selected='All'))
                             ),
                    fluidRow(column(width=2,selectizeInput("xaxis",
                                                           "X Axis",
                                                           c('One','Two','Three'))),
                             column(width=2,selectizeInput("yaxis",
                                                           "Y Axis",
                                                           c('One','Two','Three'))),
                             column(width=2,selectizeInput("color",
                                                           "Color Variable",
                                                           c('One','Two','Three'))),
                             column(width=2,selectizeInput("graph_type",
                                                           "Graph Type",
                                                           c('Bar Graph','Scatterplot','Line Graph','Box Plot','Histogram')))
                    ),
                    fluidRow(box(DT::dataTableOutput("tab_three_raw"),width=25))
            )
        )
    )))
    

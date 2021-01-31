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
            menuItem("Raw Data Table", tabName = "raw_data_table", icon = icon("database"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "Tab1",
                    fluidRow(align="center",plotlyOutput("year_bar", height = "300px",width=1200)),
                    fluidRow(align="center",plotOutput("mileage_scatter_plot_price",height="600px",width=1200))
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
                    fluidRow(align="center",plotOutput("milelage_box_make"),height="300px",width=1200),
                    fluidRow(align="center",plotlyOutput("year_made_line_make"),height="300px",width=1200),
                    fluidRow(align="center",plotlyOutput("dt_bar_model"),height="300px",width=1200),
                    fluidRow(align="center",plotlyOutput("cyl_bar_model"),height="300px",width=1200)
            ),
            tabItem(tabName = "raw_data_table",
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
                    fluidRow(column(width=10,selectizeInput("dimensions",
                                                           "Dimensions",
                                                           dims,
                                                           "multiple"=T,
                                                           selected='Model'
                                                           ))
                    ),
                    fluidRow(box(DT::dataTableOutput("custom_raw"),width=25))
            )
        )
    )))

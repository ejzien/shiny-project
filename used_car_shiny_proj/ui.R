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
            menuItem("Make Based Metrics", tabName = "Tab2", icon = icon("database"))
        )
        # selectizeInput("year_sold",
        #                "Select Item to Display",
        #                sell_years)
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "Tab1",
                    #fluidRow(fluidRow(plotOutput("year_line", height = "300px"))),
                    fluidRow(fluidRow(plotOutput("year_bar", height = "300px"))),
                    fluidRow(box(DT::dataTableOutput("raw_overall_data"),width=25))
                             #infoBoxOutput("maxBox"),
                             #infoBoxOutput("minBox"),
                             #infoBoxOutput("avgBox")),
            ),
            tabItem(tabName = "Tab2",
                    selectizeInput("make",
                                   "Make",
                                   makes),
                    fluidRow(fluidRow(plotOutput("mileage_line_make"))),
                    #fluidRow(fluidRow(plotOutput("milelage_plot_make"))),
                    #fluidRow(fluidRow(plotOutput("milelage_box_make"))),
                    fluidRow(fluidRow(plotOutput("year_made_plot_make"))),
                    #fluidRow(fluidRow(plotOutput("year_line_make"))),
                    fluidRow(box(DT::dataTableOutput("make_tab_table"),width=25))
            )
        )
    )))
    

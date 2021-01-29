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
                    fluidRow(fluidRow(plotOutput("year_line", height = "300px"))),
                    fluidRow(box(DT::dataTableOutput("raw_overall_data"),width=15))
                             #infoBoxOutput("maxBox"),
                             #infoBoxOutput("minBox"),
                             #infoBoxOutput("avgBox")),
            ),
            tabItem(tabName = "Tab2",
                    selectizeInput("make",
                                   "Make",
                                   makes),
                    fluidRow(box(DT::dataTableOutput("make_tab_table"),width=15))
            )
        )
    )))
    
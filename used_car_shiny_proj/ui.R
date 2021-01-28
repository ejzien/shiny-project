library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title = "My Dashboard"),
    dashboardSidebar(
        sidebarUserPanel(
            "Ethan Zien"
            #image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg")
        ),
        sidebarMenu(
            menuItem("First", tabName = "Tab1", icon = icon("map")),
            menuItem("Second", tabName = "Tab2", icon = icon("database"))
        ),
        selectizeInput("year_sold",
                       "Select Item to Display",
                       sell_years)
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "Tab1",
                    fluidRow(#infoBoxOutput("maxBox"),
                             #infoBoxOutput("minBox"),
                             infoBoxOutput("avgBox")),

                    fluidRow(plotOutput("year_line", height = "300px"))
            ),
            tabItem(tabName = "Tab2",
                    fluidRow(box(DT::dataTableOutput("most_common_makes"), width = 12))
            )
        )
    )))
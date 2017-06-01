library(shinydashboard)

sidebar <- dashboardSidebar(
  
  img(src = "sbg_logo_transparent.png", height = 100, width = 100),
  
  hr(),
  sidebarMenu(id = "tabs",
              menuItem("Setup", tabName = "setup", icon = icon("file-text-o")),
              menuItem("Plot", tabName = "plot", icon = icon("bar-chart")),
              menuItem("Table", tabName = "table", icon = icon("table"))
  ),
  hr()#,
  # conditionalPanel("input.tabs=='plot' || input.tabs==table",
  #                  fluidRow(
  #                    column(10,
  #                           uiOutput("time_filter")
  #                    )
  #                  )
  # )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "setup",
            column(12,  
                   box(width = NULL, status = "primary", solidHeader = TRUE,
                       title = "Setup",
                       
                       fileInput("in_file",
                                 "Select CSV file",
                                 accept = ".csv"),
                       
                       # column selectors
                       uiOutput("selectors1"),
                       uiOutput("selectors2"),
                       uiOutput("selectors3")
                   )
            )
    ),
    tabItem(tabName = "plot",
            fluidRow(
              column(width = 8,
                     align = "center",
                     box(width = NULL, 
                         plotOutput("plot", height = "500px", 
                                    width = "700px"),
                         downloadButton("download_plot", "Save Plot"),
                         collapsible = TRUE, 
                         title = "Differences in Metrics Vs Comparison Group",
                         status = "primary", solidHeader = TRUE)
                     
              ),
              column(width = 4,
                     box(
                       uiOutput("time_filter")
                     )
              )
              
              
            )
    ),
    tabItem(tabName = "table",
            box(width = NULL, status = "primary", solidHeader = TRUE,
                title = "Table",
                downloadButton("download_data", "Download Data"),
                br(), br(),
                tableOutput("table")
            )
    )
  )
)

dashboardPage(
  dashboardHeader(title = "Metric Comparisons"),
  sidebar,
  body
)
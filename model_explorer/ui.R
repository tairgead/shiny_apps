library(shinydashboard)
library(shinyBS)

sidebar <- dashboardSidebar(
  
  img(src = "sbg_logo_transparent.png", height = 100, width = 100),
  
  hr(),
  sidebarMenu(id = "tabs",
              menuItem("Setup", tabName = "setup", icon = icon("file-text-o")),
              menuItem("Plot", tabName = "plot", icon = icon("bar-chart")),
              menuItem("Table", tabName = "table", icon = icon("table")),
              menuItem("Model Code", tabName = "model", icon = icon("file-code-o"))
  ),
  hr()
  
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "setup",
            column(12,
                   fluidRow(
                     column(3,
                       fileInput("in_file",
                             "Select stanfit object",
                             accept = ".Rds"),
                   textInput("formula",
                             label = h5(HTML(paste0("<b>", "Enter RHS of model formula", "</b>")),
                                        tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                        bsButton("q1", label = "", 
                                                 icon = icon("question"), 
                                                 style = "info", 
                                                 size = "extra-small")),
                             placeholder = "b0 + b1x1 + b2x2"),
                   bsPopover(id = "q1", title = "Forgot the formula?",
                             content = HTML(paste("Click on the",
                                             "<b>", "Model Code", "</b>",
                                             "tab in the left panel and check",
                                             "the", "<em>", "model", "</em>",
                                             "section. You probably want the", 
                                             "formula in the for loop",
                                             "(without the [i] or similar",
                                             "indices).")),
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body")
                   ),
                   uiOutput("select_samples"),
                   numericInput("nbr_vars",
                                "Enter number of variables in the model",
                                value = 1,
                                min = 0)
                   )
                    
                   ),
                   
                   fluidRow(
                     column(12,
                            helpText(paste("Variable names should exactly match the", 
                                           "names in the RHS of model formula")),
                            helpText(uiOutput("data_length")))
                   ),
                   
                   fluidRow(
                     column(3,
                            uiOutput("vars")
                            ),
                     column(3,
                            uiOutput("seq_min")
                            ),
                     column(3,
                            uiOutput("seq_max")
                            ),
                     column(3,
                            uiOutput("seq_by")
                            )
                   ),
                   
                   actionButton("go_button", label = "Go",
                                icon = icon("arrow-right"))
                   
                   )
                   
            ),
    
    tabItem(tabName = "plot",
            fluidRow(
             column(width = 3,
                   uiOutput("variables"),
                   selectInput("plot_type", "Plot Type",
                               choices = c("Scatter", "Violin"))
             ),
             column(width = 3,
                   uiOutput("interaction_var1"),
                   uiOutput("interaction_var2")
                   ),
             column(width = 3,
                    uiOutput("slider")
                    )
            ),
            
            fluidRow(
            column(width = 3, offset = 4,
                   # textOutput("test_text"),
                   downloadButton("download_plot", "Save Plot")
                   )  
            ),
            column(width = 12,
             plotOutput("plot")
             ),
            # box(title = "Values of variables (excl. dependent variable)",
                # status = "info",
            column(width = 12,
              h4(helpText("Values of variables (excl. dependent variable)")),
            tableOutput("var_values")     
                   )
            
            
            # )
            
            
    ),
    tabItem(tabName = "table",
            box(width = NULL, status = "primary", solidHeader = TRUE,
                title = textOutput("download_text"),
                downloadButton("download_data", "Download Data"),
                tableOutput("table"),
                br(), br()
                
            )
    ),
    tabItem(tabName = "model",
            verbatimTextOutput("model_code"))
  )
)  



dashboardPage(
  dashboardHeader(title = "Model Explorer"),
  sidebar,
  body
)
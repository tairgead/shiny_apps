library(shinydashboard)

sidebar <- dashboardSidebar(width = 300,
                            
                            hr(),
                            
                            img(src = "ua_rev_horiz_rgb_16.png", 
                                height = 419 / 10, 
                                width = 1768 / 10,
                                style = "display: block; margin-left: auto; margin-right: auto;"),
                            
                            hr(),
                            sidebarMenu(id = "tabs",
                                        menuItem("Setup", tabName = "setup", icon = icon("file-text-o")),
                                        menuItem('Overall Word Clouds', tabName = 'wordcloud', icon = icon('cloud')),
                                        menuItem("Overall Bar Plots", tabName = "plots", icon = icon("bar-chart")),
                                        menuItem("Plots by Question", icon = icon("th-list"),
                                                 menuSubItem("Overall by Question",
                                                             tabName = "question_plots", 
                                                             icon = icon("bar-chart")),
                                                 menuSubItem('Top Positive Words by Question',
                                                             tabName = 'top_words_pos',
                                                             icon = icon('line-chart')),
                                                 menuSubItem('Top Negative Words by Question',
                                                             tabName = 'top_words_neg',
                                                             icon = icon('line-chart'))),
                                        menuItem("Table", tabName = "table", icon = icon("table"))
                            ),
                            hr()
                            
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "setup",
            column(12,  
                   box(width = NULL, status = "info", solidHeader = TRUE,
                       title = "Instructions",
                       includeMarkdown("readme.Rmd")),
                   
                   box(width = NULL, status = "primary", solidHeader = TRUE,
                       title = "Setup",
                       
                       fileInput("in_file",
                                 "Select Excel file"),
                       actionButton("go_button", "Go")
                   )
                   
            )
    ),
    
    tabItem(tabName = 'wordcloud',
            
            fluidRow(
              box(width = 4,
                plotOutput("word_cloud"),
                collapsible = TRUE,
                title = "100 Most Commonly Used Words",
                status = "primary", 
                solidHeader = TRUE),
              
              box(width = 4,
                  plotOutput("word_cloud_pos"),
                  collapsible = TRUE,
                  title = "100 Most Commonly Used Positive Words",
                  status = "primary", 
                  solidHeader = TRUE),
              
              box(width = 4,
                  plotOutput("word_cloud_neg"),
                  collapsible = TRUE,
                  title = "100 Most Commonly Used Negative Words",
                  status = "primary", 
                  solidHeader = TRUE)
              )
            ),
    
    tabItem(tabName = "plots",
            
            fluidRow(
              box(width = 4,
                  plotOutput("overall_plot", height = "700px"),
                  # width = "700px"),
                  collapsible = TRUE, 
                  title = "Plots for Overall Data (All Responses)",
                  status = "primary", solidHeader = TRUE
              ),
              box(width = 4,
                  plotOutput("top_10pos_plot", height = "700px"),
                  collapsible = TRUE,
                  title = "Top 10 Words with Positive Sentiment",
                  status = "primary", solidHeader = TRUE
              ),
              box(width = 4,
                  plotOutput("top_10neg_plot", height = "700px"),
                  collapsible = TRUE,
                  title = "Top 10 Words with Negative Sentiment",
                  status = "primary", solidHeader = TRUE
              )
            )
            
            
    ),
    
    tabItem(tabName = "question_plots",
            fluidRow(
              box(width = 12,
                  plotOutput("top_level_questions", height = "800px"),
                  title = "Proportion of Words by Sentiment for Each Question",
                  status = "primary", solidHeader = TRUE)
            )
    ),
    
    tabItem(tabName = "top_words_pos", 
            fluidRow(
              box(width = 12,
                  plotOutput("top_10pos_plot_q", height = "800px"),
                  title = paste0("Top 10 Positive Words by Question", 
                                 " - Limited to 10 Words (ties may continue)"),
                  status = "primary", solidHeader = TRUE)
            )
    ),
    
    tabItem(tabName = "top_words_neg", 
            fluidRow(
              box(width = 12,
                  plotOutput("top_10neg_plot_q", height = "800px"),
                  title = paste0("Top 10 Positive Words by Question", 
                                 " - Limited to 10 Words (ties may continue)"),
                  status = "primary", solidHeader = TRUE)
            )
    )
  )
)



dashboardPage(
  dashboardHeader(title = "TCE Open Text\nAnalysis", titleWidth = 300),
  sidebar,
  body
)

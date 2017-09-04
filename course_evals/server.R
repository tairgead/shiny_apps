library(scales)
library(stringr)
library(shiny)
library(readxl)
library(grid)
library(gridExtra)
library(wordcloud)
library(tidyverse)
library(tidytext)


#TODO Add sentence-level sentiment


# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
  
  # allow large files
  options(shiny.maxRequestSize = 100*1024^2)
  
  # read in the data the user selected
  data_input <- reactive({
    read_xlsx(input$in_file$datapath,
              skip = 2,
              col_names = c("response")
    )
  })
  
  questions <- function() {
    # Take the questions from the sheet and call them out so we can use them
    # to create a question column and then remove them from the response column
    questions <- c("What did you especially like about the way this instructor taught the course?",
                   "What suggestions would you make to improve the way this instructor taught the course?",
                   "What did you especially like about this course?",
                   "What suggestions would you make to improve this course?",
                   "Please write any additional comments you may have below.")
    questions
  }
  
  tidy_data  <- eventReactive(input$go_button, {
    
    raw_data <- data_input()
    # Clean data --------------------------------------------------------------
    
    # Create clean_data, which has a column with the questions, one with the 
    # responses, and one with the row number which will help identify which
    # response words belong to
    clean_data <- raw_data %>%
      mutate(question = ifelse(response %in% questions(), response, NA),
             question = zoo::na.locf(question),
             question = factor(question, levels = questions())) %>%
      filter(!(response %in% questions())) %>%
      mutate(response_number = row_number())
    
    
    # Create tidytext dataframe -----------------------------------------------
    
    tidy_data <- clean_data %>%
      unnest_tokens(input = response, output = word, token = "words") %>%
      group_by(response_number) %>%
      mutate(lag_word = lag(word)) %>%
      select(question, response_number, lag_word, word) %>%
      mutate(negation = ifelse(tolower(lag_word) == "not", 1, 0)) %>%
      replace_na(list("negation" = 0)) %>%
      mutate(nothing_good = str_detect(string = question, pattern = "improve"))
    
    tidy_data
  })    
  
  # Get sentiments ----------------------------------------------------------
  sentiments <- function() {
    tidy_data() %>%
      inner_join(get_sentiments("bing"))
  }
  
  top_level_sentiment <- reactive(
    sentiments() %>%
      group_by(sentiment) %>%
      count() %>%
      ungroup() %>%
      mutate(proportion = n / sum(n))
  )
  
  top_level_words <- reactive(
    sentiments() %>%
      group_by(sentiment, word) %>%
      count() %>%
      group_by(sentiment) %>%
      mutate(proportion = n / sum(n)) %>%
      arrange(sentiment, desc(proportion))
  )
  
  by_question_sentiments <- reactive(
    sentiments() %>%
      group_by(question, sentiment) %>%
      count() %>%
      group_by(question) %>%
      mutate(proportion = n / sum(n))
  )
  
  by_question_words <- reactive(
    sentiments() %>%
      group_by(sentiment, question, word) %>%
      count() %>%
      group_by(sentiment, question) %>%
      mutate(proportion = n / sum(n)) %>%
      arrange(question, sentiment, desc(proportion))
  )
  
  
  overall_plot <- function() {
    top_level_sentiment() %>%
      ggplot(aes(x = sentiment, y = proportion, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = scales::percent(proportion)),
                nudge_y = 0.02) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Proportion of Words by Sentiment",
           subtitle = "Entire Evaluation Form",
           x = "Sentiment",
           y = "Proportion of Words") +
      theme(panel.background = element_blank(),
            axis.line = element_line())
  }
  
  
  top_10pos_plot <- function() {
    top_level_words() %>%
      group_by(sentiment) %>%
      top_n(10, wt = proportion) %>%
      filter(sentiment == "positive") %>%
      arrange(proportion) %>%
      mutate(word = factor(word, levels = .$word)) %>%
      ggplot(aes(x = word, y = proportion)) +
      geom_col(fill = "#00BFC4", show.legend = FALSE) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip() +
      labs(title = "Top 10 Words with Positive Sentiment",
           subtitle = "Based on the entire dataset",
           x = NULL,
           y = NULL) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            axis.text.y = element_text(size = 12))
  }
  
  top_10neg_plot <- function() {
    top_level_words() %>%
      group_by(sentiment) %>%
      top_n(10, wt = proportion) %>%
      filter(sentiment == "negative") %>%
      arrange(proportion) %>%
      mutate(word = factor(word, levels = .$word)) %>%
      ggplot(aes(x = word, y = proportion, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip() +
      labs(title = "Top 10 Words with Negative Sentiment",
           subtitle = "Based on the entire dataset",
           x = NULL,
           y = NULL) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            axis.text.y = element_text(size = 12))
  }
  
  # by question plots
  top_level_questions <- function() {
    
    # create dataframe with quesitons and order to ensure plot renders right
    question_order <- data.frame("question" = questions(), 
                                 "order" = seq(1, length(questions()), 1)) %>%
      mutate(question_label = factor(str_wrap(question, width = 45)))
    
    by_question_sentiments() %>%
      ungroup() %>%
      left_join(question_order, by = c("question" = "question")) %>%
      ggplot(aes(x = sentiment, y = proportion, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = scales::percent(proportion)),
                nudge_y = 0.02) +
      scale_y_continuous(labels = scales::percent) +
      facet_wrap(~ reorder(question_label, order)) +
      labs(x = "Sentiment",
           y = "Proportion of Scored Words") +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            strip.text = element_text(size = 12))
  }
  
  
  ## top 10 positive words
  ### by question
  
  top_10pos_by_q <- function() {
    
    plot_list <- vector("list", length = length(questions()))
    
    for (i in 1:length(questions())) {
      g <- by_question_words() %>%
        filter(question == questions()[[i]]) %>%
        filter(sentiment == "positive") %>%
        group_by(question) %>%
        top_n(10, wt = proportion) %>%
        slice(1:10) %>%
        arrange(desc(proportion)) %>%
        ggplot(aes(x =  reorder(word, proportion), y = proportion)) +
        geom_col(fill = "#00BFC4") +
        scale_y_continuous(labels = scales::percent) +
        coord_flip() +
        labs(title = paste("Question:", questions()[[i]]),
             x = "Prop. of total scored words",
             y = "Word") +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              axis.text.y = element_text(size = 12))
      
      plot_list[[i]] <- g
    }
    plot_list
  }
  
  top_10pos_plot_q <- function() {
    gridExtra::grid.arrange(grobs = top_10pos_by_q())
  }
  
  
  
  top_10neg_by_q <- function() {
    
    plot_list <- vector("list", length = length(questions()))
    
    for (i in 1:length(questions())) {
      g <- by_question_words() %>%
        filter(question == questions()[[i]]) %>%
        filter(sentiment == "negative") %>%
        group_by(question) %>%
        top_n(10, wt = proportion) %>%
        slice(1:10) %>%
        arrange(desc(proportion)) %>%
        ggplot(aes(x =  reorder(word, proportion), y = proportion)) +
        geom_col(fill = "#F8766D") +
        scale_y_continuous(labels = scales::percent) +
        coord_flip() +
        labs(title = paste("Question:", questions()[[i]]),
             x = "Prop. of total scored words",
             y = "Word") +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              axis.text.y = element_text(size = 12))
      
      plot_list[[i]] <- g
    }
    plot_list
  }
  
  top_10neg_plot_q <- function() {
    gridExtra::grid.arrange(grobs = top_10neg_by_q())
  } 
  
  word_cloud <- function () {
    top_level_words() %>%
      with(wordcloud(word, n, max.words = 100))
  }
  
  word_cloud_pos <- function () {
    top_level_words() %>%
      filter(sentiment == "positive") %>%
      with(wordcloud(word, n, 
                     colors = c("steelblue1", "steelblue2",
                                "steelblue3", "steelblue"),
                     max.words = 100))
  }
  
  word_cloud_neg <- function () {
    top_level_words() %>%
      filter(sentiment == "negative") %>%
      with(wordcloud(word, n,
                     colors= c("indianred1","indianred2","indianred3","indianred"),
                     max.words = 100))
  }
  
  
  
  output$overall_plot <- renderPlot(overall_plot())
  output$top_10pos_plot <- renderPlot(top_10pos_plot())
  output$top_10neg_plot <- renderPlot(top_10neg_plot())
  output$top_10pos_plot_q <- renderPlot(top_10pos_plot_q())
  output$top_10neg_plot_q <- renderPlot(top_10neg_plot_q())
  output$top_level_questions <- renderPlot(top_level_questions())
  output$word_cloud <- renderPlot(word_cloud())
  output$word_cloud_pos <- renderPlot(word_cloud_pos())
  output$word_cloud_neg <- renderPlot(word_cloud_neg())
  
  observeEvent(input$go_button, {
    updateTabItems(session, inputId = "tabs", selected = "wordcloud")
  })
})



library(shiny)
library(dplyr)
library(rstan)
library(shinyBS)

shinyServer(function(session, input, output) {
  
  
  # allow large files
  options(shiny.maxRequestSize=100*1024^2)
  
  # data_input --------------------------------------------------------------
  # load the model from file
  data_input <- reactive({
    readRDS(input$in_file$datapath)
  })
  
  
  # Render UI ---------------------------------------------------------------  
  # select number of samples
  output$select_samples <- renderUI({
    numericInput("n_samples",
                 "Number of samples to draw (per combination of variables below)",
                 value = 100,
                 min = 0,
                 max = nrow(data.frame(extract(data_input())))) #nrow(extract(data)))
  })
  
  
  # select number of variables in model
  output$vars <- renderUI({
    num_vars <- as.integer(input$nbr_vars)
    lapply(1:num_vars, function(i) {
      textInput(paste0("var", i),
                label = paste("variable", i))
    })
  })
  
  # select 'from' section of seq() for each variable
  output$seq_min <- renderUI({
    num_vars <- as.integer(input$nbr_vars)
    lapply(1:num_vars, function(i) {
      numericInput(paste0("min", i),
                   label = paste("min for variable", i),
                   value = 0)
    })
  })
  
  # select 'to' section of seq() for each variable
  output$seq_max <- renderUI({
    num_vars <- as.integer(input$nbr_vars)
    lapply(1:num_vars, function(i) {
      numericInput(paste0("max", i),
                   label = paste("max for variable", i),
                   value = 1)
    })
  })
  
  # select 'length.out' section of seq() for each variable
  output$seq_by <- renderUI({
    num_vars <- as.integer(input$nbr_vars)
    lapply(1:num_vars, function(i) {
      numericInput(paste0("length_out", i),
                   label = paste("number of steps for variable", i),
                   value = 2)
    })
  })
  
  output$data_length <- renderText({
    HTML(paste("With these variable options, the estimation dataframe will be",
               "<b>", 
               format(nrow(expand_grid()) * input$n_samples, big.mark = ","),
               "</b> rows by", input$nbr_vars, "columns"))
  })
  
  output$download_text <- renderText({
    paste("20 random rows of data, click 'Download Data' to download the entire", 
          format(nrow(expand_grid()) * input$n_samples, big.mark = ","), 
          "rows by", 
          input$nbr_vars + 1, "columns of data")
  })
  
  output$number_estimates <- renderText({
    paste("Generating", 
          format(nrow(expand_grid()) * input$n_samples, big.mark = ","), 
          "estimates. This may take a while...")
  })
  
  var_names <- reactive({
    num_vars <- as.integer(input$nbr_vars)
    
    vars_list <- lapply(1:num_vars, function(i){
      input[[paste0("var", i)]]
    })
    
    as.character(vars_list)
  })
  
  output$variables <- renderUI({
    selectizeInput("x_var",
                   "Dependent Variable",
                   choices = var_names(),
                   selected = var_names()[1],
                   multiple = FALSE)
    
  })
  
  output$interaction_var1 <- renderUI({
    selectizeInput("interaction_var1",
                   "Interaction Variable 1",
                   choices = c("None", var_names()),
                   selected = "None",
                   multiple = FALSE)
    
  })
  
  output$interaction_var2 <- renderUI({
    selectizeInput("interaction_var2",
                   "Interaction Variable 2",
                   choices = c("None", var_names()),
                   selected = "None",
                   multiple = FALSE)
    
  })
  
  output$slider <- renderUI({
    tagList(
      output$slider <- renderUI({
        tagList(
          selectInput("secondary_val", paste(input$interaction_var1,"value"),
                      choices = unique(expand_grid()[[input$interaction_var1]])),
          
          selectInput("third_val", paste(input$interaction_var2,"value"),
                      choices = unique(expand_grid()[[input$interaction_var2]]))
        )
      })
    )
  })
  
  
  # expand_grid -------------------------------------------------
  expand_grid <- reactive({
    num_vars <- as.integer(input$nbr_vars)
    
    variables <- vector("list", length = num_vars)
    
    for (i in 1:num_vars) {
      from <- input[[paste0("min", i)]]
      to <- input[[paste0("max", i)]]
      length_out <- input[[paste0("length_out", i)]]
      
      variables[[i]] <- seq(from = from, to = to, length.out = length_out)
    }
    
    vars_list <- lapply(1:num_vars, function(i){
      input[[paste0("var", i)]]
    })
    
    names(variables) <- vars_list
    expand.grid(variables)
  })
  
  
  observeEvent(input$go_button, {
    updateTabItems(session, inputId = "tabs", selected = "plot")
  })
  
  
  # Download Handlers -------------------------------------------------------  
  # download data
  output$download_data <- downloadHandler(
    filename = function(){
      paste0("estimates_", Sys.Date(), ".csv")
    },
    content = function(file){
      write.csv(analysis(), file, row.names =  FALSE)
    }
  )
  
  # download plot
  output$download_plot <- downloadHandler(
    filename = function(){
      paste0("model_explorer_plot_", Sys.Date(), ".png")
    },
    content = function(file){
      ggsave(file, plot_input(), device = "png", width = 8, height = 6,
             units = "in", dpi = 600)
      
    },
    contentType = "image/png"
  )
  
  # Get model code ----------------------------------------------------------
  
  output$model_code <- renderText({
    model <- data_input()
    get_stancode(model)
  })
  
  # stan_pred_data ----------------------------------------------------------
  stan_pred_data <- reactive({
    # load the expand_grid object that holds all the predictor values
    data <- expand_grid()
    
    stanfit_object <- data_input()
    
    # extract samples from the posterior
    samples <- extract(stanfit_object)
    
    # convert samples to dataframe
    samples <- data.frame(samples)
    
    # randomly select n_samples rows from dataframe
    samples_p <- samples[sample(nrow(samples), size = input$n_samples), ]
    
    # Create n repeats of samples, where n = # of rows in data
    pred_data <- samples_p[rep(seq_len(nrow(samples_p)), nrow(data)), ]
    
    # create row_id so I can order the data to ensure that each row in the data
    # gets each sample from an MCMC chain
    data_rep <- data %>%
      mutate(row_id = row_number())
    
    # create N repeats of data, where N = # of rows in samples
    data_rep <- data_rep[rep(seq_len(nrow(data_rep)), nrow(samples_p)), ]
    
    # arrange data by row_id 
    # (so row 1 is repeated N times before row 2 appears, etc)
    data_rep <- data_rep %>%
      arrange(row_id)
    
    # combine data and MCMC samples
    pred_data <- cbind(data_rep, pred_data)
    
    # return pred_data for use in the analysis section
    pred_data
  })
  
  
  # Analysis ----------------------------------------------------------------
  analysis <- eventReactive(input$go_button, {
    
    # Stan object section -----------------------------------------------------    
    if (class(data_input()) == "stanfit") {
      
      # load the stanfit object
      stanfit_object <- data_input()
      
      pred_data <- stan_pred_data()
      
      # create estimates based on user-provided formula, data, and MCMC samples
      y_hat <- tryCatch(with(data = pred_data, 
                                   eval(parse(text = paste(input$formula)))),
                              error = function(cond) {
                                message(cond)
                                message(paste("\nDoes the formula only contain",
                                              "coefficients and variables that are",
                                              "contained in the stan_model and x?"))
                              })
      
      
      
      
      cbind(y_hat, pred_data[, 1:ncol(expand_grid())])
    }
    
    # OLS/MLE Section ---------------------------------------------------------    
    # if (class(data_input()) %in% c("lm", "glm", "merMod")) {
    #   
    # }
  })
  
  
  
  
  # Output plot -------------------------------------------------------------
  
  plot_input <- function(){
    # create names for axis labels
    x_name <- input$x_var
    y_name <- "y_hat"
    
    data <- analysis()
    
    
    # create variables based on user input
    x_var <- data[[input$x_var]]
    y_var <- data[[y_name]]
    interaction1_ind <- input$interaction_var1 == "None"
    interaction2_ind <- input$interaction_var2 == "None"
    
    if (!interaction1_ind) interaction_var1 <-  data[[input$interaction_var1]]
    if(!interaction2_ind) interaction_var2 <-  data[[input$interaction_var2]]
    
    slider_value <- as.numeric(input$secondary_val)
    slider_value2 <- as.numeric(input$third_val)
    
    if(interaction1_ind & interaction2_ind) {
      plot_data_t <- data %>%
        filter_at(vars(-one_of(c("y_hat", paste0(input$x_var)))),
                  all_vars(min(.) == .)) %>%
        group_by_(paste(input$x_var)) %>%
        mutate(mean_estimate = mean(y_hat),
               lower = quantile(y_hat, 0.025),
               upper = quantile(y_hat, 0.975))
    } else if (interaction1_ind) {
      plot_data_t <- data %>%
        filter(interaction_var2 == round(slider_value2, 3)) %>% 
        filter_at(vars(-one_of(c("y_hat", paste0(input$x_var)))),
                  all_vars(min(.) == .)) %>%
        group_by_(paste(input$x_var)) %>%
        mutate(mean_estimate = mean(y_hat),
               lower = quantile(y_hat, 0.025),
               upper = quantile(y_hat, 0.975))
    } else if (interaction2_ind) {
      plot_data_t <- data %>%
        filter(interaction_var1 == round(slider_value, 3)) %>% 
        filter_at(vars(-one_of(c("y_hat", paste0(input$x_var)))),
                  all_vars(min(.) == .)) %>%
        group_by_(paste(input$x_var)) %>%
        mutate(mean_estimate = mean(y_hat),
               lower = quantile(y_hat, 0.025),
               upper = quantile(y_hat, 0.975))
    } else {
      plot_data_t <- data %>%
        filter(interaction_var1 == round(slider_value, 3),
               interaction_var2 == round(slider_value2, 3)) %>% 
        filter_at(vars(-one_of(c("y_hat", paste0(input$x_var)))),
                  all_vars(min(.) == .)) %>%
        group_by_(paste(input$x_var)) %>%
        mutate(mean_estimate = mean(y_hat),
               lower = quantile(y_hat, 0.025),
               upper = quantile(y_hat, 0.975))
    }
    
    
    
    plot_data_t$xvar <- as.factor(plot_data_t[[input$x_var]])
    
    # Generate df and output to show values of vars
    # (excluding y_hat and x_var)
    output$var_values <- renderTable({
      plot_data_t %>%
        ungroup() %>%
        select(-one_of(c("y_hat", "mean_estimate", "lower", "upper",
                         "xvar", paste0(input$x_var)))) %>%
        distinct()
    })
    
    
    # create scatterplot
    if(input$plot_type == "Scatter") {
      g <- plot_data_t %>%
        ggplot(aes_string(x = paste(x_name), y = y_name)) +
        geom_point(alpha = sqrt(0.25/(input$n_samples))) +
        geom_smooth(se = FALSE, method = "gam") +
        geom_ribbon(aes(ymin = lower,
                        ymax = upper), alpha = 1/5) +
        scale_y_continuous(limits = c(min(analysis()[y_name]), 
                                      max(analysis()[y_name]))) +
        labs(x = x_name,
             y = expression(hat(y))) +
        theme(panel.background = element_blank(),
              axis.line = element_line())
      
      
      # create violin plot with 25th, 50th, and 75th percentiles
    } else if (input$plot_type == "Violin") {
      g <- plot_data_t %>%
        ggplot(aes_string(x = "xvar", y = paste(y_name))) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),
                    fill = "#2CA01C",
                    alpha = 3/5) +
        scale_y_continuous(limits = c(min(analysis()[y_name]), 
                                      max(analysis()[y_name]))) +
        labs(x = x_name,
             y = expression(hat(y))) +
        theme(panel.background = element_blank(),
              axis.line = element_line())
      
    }
    
    g
  }
  output$plot <- renderPlot({plot_input()})
  
  
  
  
  
  
  
  # Output Table ------------------------------------------------------------
  output$table <- renderTable({
    analysis()[sample(seq_len(nrow(analysis())), 20), ]
  })
})













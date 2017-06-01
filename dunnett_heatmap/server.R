library(shiny)

shinyServer(function(session, input, output) {
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(shiny, multcomp, ggplot2, dplyr, tidyr, stringr)
  
  library(shiny)
  library(multcomp)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(plotly)
  
  options(shiny.maxRequestSize=100*1024^2)
  
  heat_colors <- c("#BF4600", "#B84807", "#B14B0F", "#AB4D17", "#A4501F",
                   "#9E5227", "#97552F", "#905735", "#8A5A3F", "#835C47",
                   "#7D5F4E", "#766256","#6F645E", "#696766", "#62696E",
                   "#5C6C76", "#556E7E", "#4F7186", "#48738E", "#417696",
                   "#3B799D", "#347BA5", "#2E7EAD", "#2780BF", "#2083BD",
                   "#1A85C5", "#1388CD", "#0D8AD5", "#068DDD", "#0090E5")
  
  data_input <- reactive({
    read.csv(input$in_file$datapath,
             header = TRUE,
             na.strings = c("", " ", "  ", "NA"))
  })
  
  output$selectors1 <- renderUI({
    selectizeInput("group_var",
                   "Select grouping variable",
                   choices = names(data_input()),
                   multiple = FALSE)
  })
  
  output$selectors2 <- renderUI({
    selectizeInput("target_group",
                   "Select target group",
                   choices = unique(data_input()[
                     complete.cases(data_input()[[input$group_var]]), #ignore NA
                     input$group_var]))
  })
  
  output$selectors3 <- renderUI({
    tagList(
      selectizeInput("time_var",
                     "Select timeframe variable",
                     choices = c("None", names(data_input())),
                     multiple = FALSE),
      
      selectizeInput("vars_to_test_input",
                     "Select variables to test",
                     choices = names(data_input()[sapply(data_input(), 
                                                         FUN = is.numeric)]),
                     multiple = TRUE),
      
      actionButton("go_button",
                   "Go")
    )
    
  })
  
  
  output$time_filter <- renderUI({
    selectInput("timeframe",
                "Timeframe",
                choices = (if(input$time_var == "None") "None" else 
                  unique(data_input()[[input$time_var]])))
  })
  
  analysis <- eventReactive(input$go_button, {
    
    
    # analysis <- reactive({
    data <- data_input()
    
    cols <- paste(input$vars_to_test, input$group_var, input$time_var)
    
    comp_timeframe <- data[[input$time_var]]
    
    if (input$time_var == "None"){
      timeframe <- "None"
    } else timeframe <- unique(data[[input$time_var]])
    
    vars_to_test <- input$vars_to_test_input
    group_var <- input$group_var
    rows_to_keep <- !is.na(group_var)
    
    data <- data[rows_to_keep, ]
    
    # get target level and change data so this level is first
    target_level <- input$target_group
    # create a vector that only has the target group
    target <- unique(data[group_var])[unique(data[group_var]) == target_level]
    # create a vector that has all groups except the target group
    non_targets <- unique(data[group_var])[unique(data[group_var]) != target_level]
    #concatenate the target and non-targets so target is first
    group_order <- c(target, non_targets)
    
    data[group_var] <- factor(data[[group_var]], levels = group_order)
    
    
    if (input$time_var == "None"){
      final_data_list <- vector("list", 1)
      
      loop_data <- data
      
      # create lists that will hold results from each iteration of the loop
      metric_list <- vector("list", length(vars_to_test))
      coeff_list <- vector("list", length(vars_to_test))
      p_val_list <- vector("list", length(vars_to_test))
      
      argument <- list("Dunnett")
      names(argument) <- group_var
      cmp <- do.call(mcp, argument)
      
      for(var in 1:length(vars_to_test)){
        fit <- aov(as.formula(paste(vars_to_test[var], "~", group_var)),
                   data = loop_data)
        dunnett <- glht(fit, linfct = cmp)
        
        result <- summary(dunnett)
        
        metric_list[[var]] <- vars_to_test[var]
        coeff_list[[var]] <- result$coef[2:length(result$coef)]
        p_val_list[[var]] <- result$test$pvalues
      }
      
      metric <- do.call("rbind", metric_list)
      coefficients <- data.frame(do.call("rbind", coeff_list))
      p_vals <- data.frame(do.call("rbind", p_val_list))
      
      names(p_vals) <- names(coefficients)
      
      results_1 <- cbind(metric, "output" = c("difference"), coefficients)
      results_2 <- cbind(metric, "output" = c("p-value"), p_vals)
      results <- rbind(results_1, results_2)
      
      results_final <- results %>%
        gather(-metric, -output, key = group, value = value) %>%
        spread(key = output, value = value) %>%
        mutate(group = gsub(x = .$group,
                            pattern = paste(group_var),
                            replacement = ""),
               timeframe = "None") %>%
        rename(p_value = `p-value`)
      
      final_data_list[[1]] <- results_final
    } else {
      final_data_list <- vector("list", length(timeframe))
      for(times in 1:length(timeframe)){
        
        loop_data <- data %>%
          filter(comp_timeframe == timeframe[times])
        
        # create lists that will hold results from each iteration of the loop
        metric_list <- vector("list", length(vars_to_test))
        coeff_list <- vector("list", length(vars_to_test))
        p_val_list <- vector("list", length(vars_to_test))
        
        argument <- list("Dunnett")
        names(argument) <- group_var
        cmp <- do.call(mcp, argument)
        
        for(var in 1:length(vars_to_test)){
          fit <- aov(as.formula(paste(vars_to_test[var], "~", group_var)),
                     data = loop_data)
          dunnett <- glht(fit, linfct = cmp)#mcp(group_var = "Dunnett"))
          
          result <- summary(dunnett)
          
          metric_list[[var]] <- vars_to_test[var]
          coeff_list[[var]] <- result$coef[2:length(result$coef)]
          p_val_list[[var]] <- result$test$pvalues
        }
        
        metric <- do.call("rbind", metric_list)
        coefficients <- data.frame(do.call("rbind", coeff_list))
        p_vals <- data.frame(do.call("rbind", p_val_list))
        
        names(p_vals) <- names(coefficients)
        
        results_1 <- cbind(metric, "output" = c("difference"), coefficients)
        results_2 <- cbind(metric, "output" = c("p-value"), p_vals)
        results <- rbind(results_1, results_2)
        
        results_final <- results %>%
          gather(-metric, -output, key = group, value = value) %>%
          spread(key = output, value = value) %>%
          mutate(group = gsub(x = .$group,
                              pattern = paste(group_var),
                              replacement = ""),
                 timeframe = paste(timeframe[times])) %>%
          rename(p_value = `p-value`)
        
        final_data_list[[times]] <- results_final
      }
    }
    
    
    
    final_data <- data.frame(do.call("rbind", final_data_list))
    
    final_data
  })
  
  plotInput <- function(){
    # fill_limits <- as.numeric(unlist(strsplit(input$fill_limits,",")))
    # max_lim <- max(abs(fill_limits))
    # fill_limits <- c()
    
    plot_data <- analysis() %>%
      filter(timeframe == input$timeframe) %>%
      mutate(p_val_color = ifelse(p_value > 0.05, "no_color", "color"),
             difference2 = ifelse(p_value > 0.05, NA, difference),
             limit = max(abs(difference2), na.rm = TRUE))
    
    time_text <- ifelse(plot_data$timeframe == "None", "test", 
                        paste("-", unique(plot_data$timeframe)))
    
    lower_lim <- -mean(plot_data$limit)
    upper_lim <- mean(plot_data$limit)
    
    plot_data %>%
      ggplot(aes(x = group, y = metric, fill = difference2)) +
      geom_tile() +
      scale_fill_gradientn(colors = c("#BF4600", "#EBEBEB", "#0090E5"),
                           limits = c(lower_lim, upper_lim),
                           na.value = "#FFFFFF") + #fill_limits) +
      # scale_fill_gradientn(colors = heat_colors, #heat.colors(20),
      #                      limits = fill_limits,
      #                      
      #                      na.value = "lightgray") +
      # limits = c(-15, 15), na.value = "lightgray") +
      labs(title = paste("Comparisons by Group and Metric", time_text),
           subtitle = isolate(paste("Baseline Group:", input$target_group)),
           x = "",
           y = "") +
      theme(panel.background = element_blank()) +
      guides(fill = guide_legend(title ="Difference"))
    
    # p <- plot_ly(plot_data, x = "group", y = "metric", z = "difference2",
    #              colors = colorRamp(c("brown", "blue")), type = "heatmap")
    

  }
  
  output$plot <- renderPlot(plotInput(), height = 400, width = 600)
  
  observeEvent(input$go_button, {
    updateTabItems(session, inputId = "tabs", selected = "plot")
  })
  
  output$download_plot <- downloadHandler(
    filename = function(){
      paste0("heatmap_", Sys.Date(), ".png")
    },
    content = function(file){
      ggsave(file, plotInput(), device = "png", width = 8, height = 6, units = "in",
             dpi = 600)
      
    },
    contentType = "image/png"
  )
  
  output$info <- renderPrint({
    plot_data <- analysis() %>%
      filter(timeframe == input$timeframe) %>%
      mutate(p_val_color = ifelse(p_value > 0.05, "no_color", "color"),
             difference2 = ifelse(p_value > 0.05, NA, difference),
             limit = max(abs(difference2), na.rm = TRUE))
    # xy_str <- function(e) {
    #   if(is.null(e)) return("NULL\n")
    #   paste0("x=", e$p_value, " y=", e$difference, "\n")
    # }
    nearPoints(plot_data, input$plot_hover, threshold = 30, 
                       maxpoints = 1)
   
    
    # paste(
    #   "Difference:", 
    #   "p-value:",
    #   "hover: ", xy_str(input$plot_hover)
    # )
  })
  
  
  # Display table with data from comparisons (differences, p-values, etc.)
  output$table <- renderTable(
    analysis(), digits = 3, width = 600
  )
  
  # download data
  output$download_data <- downloadHandler(
    filename = function(){
      paste0("multiple_comparisons_", Sys.Date(), ".csv")
    },
    content = function(file){
      write.csv(analysis(), file, row.names =  FALSE)
    }
  )
})



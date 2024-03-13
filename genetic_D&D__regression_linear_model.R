
# Take correlations of the currently processed data set as input
# and check whether a linear model can be applied to these correlations.
# Regression analyses in this script is always conducted directly after correlation analyses. 
# This is why the input file is still defined in the environment and we do not need to load it.



# Create ways to handle the significant correlations of the data set with effective population size.
# Prepare an empty list for method names that correlate significantly
method_names_with_Ne <- c()
# Identify which methods correlate significantly and save them in a list.
for (p_value in correlation_results_with_Ne[, 2]) {
  if (!is.na(p_value) && p_value < 0.05) {
    method_name <- correlation_results_with_Ne[, 1][which(correlation_results_with_Ne[, 2] == p_value)]
    method_names_with_Ne <- c(method_names_with_Ne, method_name)
  }  
}

# Create ways to handle the significant correlations of the data set without effective population size.
# Prepare an empty list for method names that correlate significantly
method_names_without_Ne <- c()
# Identify which methods correlate significantly and save them in a list.
for (p_value in correlation_results_without_Ne[, 2]) {
  if (!is.na(p_value) && p_value < 0.05) {
    method_name <- correlation_results_without_Ne[, 1][which(correlation_results_without_Ne[, 2] == p_value)]
    method_names_without_Ne <- c(method_names_without_Ne, method_name)
  }  
}


# If there is any significant correlation with Ne included we can try to apply a linear model.
if (!length(method_names_with_Ne) == 0)  {
  
  # Create an empty data frame to save the regression model(s) that fit our significant correlation(s).
  regression_models_with_Ne <- data.frame(
    model_name = c(), 
    modelled_influence_percentage = c(), # influence of one variable on the other variable.
    regression_p_value = c()
  )
  
  # save information about the regression model for every entry in a data frame if the model meets the model assumptions.
  # If it doesn't R should tell us what doesn't fit. 
  for (method_name in method_names_with_Ne) {
    
    # split entry in two method names:
    method_name <- unlist(strsplit(method_name, "_"))
    
    ##### Create regression model: 
    # define variables: 
    # y = dependent variable, x = independent variable, lm(y ~ x)
    # How good can we explain y with x?
    if ("Ne" %in% method_name) {
      x <- as.numeric(all_results_with_Ne$Ne)
      x_method <- "Ne"
      if ("diversity" %in% method_name) {
        y <- all_results_with_Ne$allelic_diversity
        y_method <- "Diversity Index"
      } 
      if ("Overlaps" %in% method_name) {
        y <- all_results_with_Ne$observed_overlap
        y_method <- "EcoSim"
      }
      if ("Amova" %in% method_name) {
        y <- all_results_with_Ne$Difference
        y_method <- "AMOVA"
      } 
    } else if ("diversity" %in% method_name) {
      x <- all_results_with_Ne$allelic_diversity
      x_method <- "Diversity Index"
      if ("Amova" %in% method_name) {
        y <- all_results_with_Ne$Difference
        y_method <- "AMOVA"
      }
      if ("Overlaps" %in% method_name) {
        y <- all_results_with_Ne$observed_overlap
        y_method <- "Overlaps"
      }
    } else {
      x <- all_results_with_Ne$Difference
      x_method <- "AMOVA"
      y <- all_results_with_Ne$observed_overlap
      y_method <- "Overlaps"
    }
    regression_model <- lm(y ~ x)
    #summary(regression_model)
    
    
    # show regression model on coordinate system (straight line): 
    plot(x, y, xlab = x_method, ylab = y_method)
    abline(regression_model)
    dev.copy(png ,paste0("Results", describe_results_variable, "/Compare_all_methods/regression_plot_with_Ne.png"))
    dev.off()
    
    
    ##### Test model assumptions:
    
    # Explanations on model assumptions: 
    # A regression model is based on assumptions.
    # If the assumptions are not applicable, the model is doubtful.
    # Model assumptions: 
    # 1. Linearity:
    # this can be done visually: plot(regression_model, 1)
    # another way of displaying the plot: 
    # plot(fitted(regression_model),residuals(regression_model))
    # abline(0,0)
    # If data points are equally distributed beneath and above the abline, the linearity assumption is met. 
    # with gvlma() we can see the result of the linearity test at "Global Stat"
    # 2. Normality of residuals: 
    # plot(regression_model, 2)
    # shapiro.test(residuals(regression_model)) ## normally distributed if p-value > 0.05
    # The third option is testing with gvlma(). The Skewness and Kurtosis assumptions show that the distribution of the residuals are normal.
    # 3. Nature of the variable: 
    # Is your dependent variable truly continuous, or categorical? 
    # Rejection of the null (p < .05) indicates that you should use an alternative form of the generalized linear model (e.g. logistic or binomial regression).
    # 4. Homogeneity of variance
    # plot(regression_model, 3)
    # ncvTest(regression_model) ## p <0.05, suggests that the data is not homoscedastic
    # This test is also included in gvlma(). Look at "Heteroscedasticity".
    # 5. Independence: (no autocorrelation)
    # durbinWatsonTest(regression_model ) ## if p-value > 0.05 the assumption is met 
    
    
    
    # Test model assumptions:
    # First, a temporary file is created to save test results. 
    console_message_file_path <- paste0("Results", describe_results_variable, "/Compare_all_methods/model_assumptions.txt")
    console_message_file <- file(console_message_file_path, open = "wt")
    #gvlma(regression_model) ## with this command you can test 1. - 4. of the model assumptions.
    
    # save the results of the model assumptions test:
    capture.output(gvlma(regression_model), file = console_message_file)
    # load the message into R
    gvla_results <- readLines(paste0("Results", describe_results_variable, "/Compare_all_methods/model_assumptions.txt"))
    # terminate the sink() process (this command is probably an overkill command, but the usual sink() didn't work and I wasn't sure how else to solve the problem)
    closeAllConnections()
    # remove the file
    file.remove(console_message_file_path)
    
    # Test for independence: 
    Independence_test <- durbinWatsonTest(regression_model)
    
    
    # This code creates a summary vector of the results if all assumptions are met. 
    # If one or more assumptions are failing it tells you which.
    if (length(grep("acceptable", gvla_results, value = TRUE)) ==5 && Independence_test$p > 0.05) {
      # save results of regression model:
      # full results: 
      regression_model_summary <- summary(regression_model)
      # Multiple R-squared: How big is the influence of x on y ? How much can we explain with our model?
      modelled_influence_percentage <- regression_model_summary$r.squared *100
      # p_value:
      regression_p_value <- regression_model_summary$coefficients[8]
      
      ###### Save model results in the dataframe
      # Create a new row
      new_row <- data.frame(
        model_name = method_name,
        modelled_influence_percentage = modelled_influence_percentage,
        regression_p_value = regression_p_value
      )
      # Append results:
      regression_models_with_Ne <- rbind(regression_models_with_Ne, new_row)
      
    } else if (length(grep("acceptable", gvla_results, value = TRUE)) ==5 ) {
      cat(" There is an autocorrelation in the data for the ", method_name , "model. ")
    } else if (Independence_test$p > 0.05) {
      if (!grepl("acceptable", gvla_results[18])) {
        cat("The data is not linear of " , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[19])) {
        cat("The residuals are not normally distributed in "  , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[20])) {
        cat("The residuals are not normally distributed in " , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[21])) {
        cat("You might want to pick a logistic or binomial regression instead of the linear regression of  " , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[22])) {
        cat("The data is not homoscedastic of ",  method_name , "model. ")
      }
    } else {
      cat(" There is an autocorrelation in the data for the ", method_name , "model. ")
      if (!grepl("acceptable", gvla_results[18])) {
        cat("The data is not linear of " , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[19])) {
        cat("The residuals are not normally distributed in "  , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[20])) {
        cat("The residuals are not normally distributed in " , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[21])) {
        cat("You might want to pick a logistic or binomial regression instead of the linear regression of  " , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[22])) {
        cat("The data is not homoscedastic of ",  method_name , "model. ")
      }
    }
  }  
  # Because of the splitting in method name the rows will be doubled. 
  # Correct this error: 
  regression_models_with_Ne <- regression_models_with_Ne %>%
    group_by(modelled_influence_percentage, regression_p_value) %>%
    summarize(model_name = paste(model_name, collapse = "_")) %>%
    as.data.frame()    
  # Change the order of columns
  regression_models_with_Ne <- regression_models_with_Ne %>%
    select(model_name, modelled_influence_percentage, regression_p_value)
  
  
  # if the dataframe is not empty save it as csv.- file
  if (nrow(regression_models_with_Ne) > 0) {
    write.csv(regression_models_with_Ne, file =  paste0("Results", describe_results_variable, "/Compare_all_methods/regression_models_with_Ne.csv"), row.names = FALSE)
  }
} else {
  cat("None of the correlations with Ne have significant results.")
}

# If there is any significant correlation with Ne excluded we can try to apply a linear model.  
if (!length(method_names_without_Ne) == 0) {
  # Create an empty data frame to save the regression model(s) that fit our significant correlation(s).
  regression_models_without_Ne <- data.frame(
    model_name = c(), 
    modelled_influence_percentage = c(), # influence of one variable on the other variable.
    regression_p_value = c()
  )
  
  # save information about the regression model for every entry in a data frame if the model meets the model assumptions.
  # If it doesn't R should tell us what doesn't fit. 
  for (method_name in method_names_without_Ne) {
    
    # split entry in two method names:
    method_name <- unlist(strsplit(method_name, "_"))
    
    ##### Create regression model: 
    # define variables: 
    # y = dependent variable, x = independent variable, lm(y ~ x)
    # How good can we explain y with x?
    if ("diversity" %in% method_name) {
      x <- diversity_results$X.N..N...1.....lambda[1:(nrow(diversity_results)-1)]
      x_method <- "Diversity Index"
        
      if ("Amova" %in% method_name) {
        y <- amova_results$Difference
        y_method <- "AMOVA"
      }
      if ("Overlaps" %in% method_name) {
        y <- Overlaps_results$observed_overlap
        y_method <- "EcoSim"
      }
    } else {
      x <- amova_results$Difference
      y <- Overlaps_results$observed_overlap
      
      x_method <- "AMOVA"
      y_method <- "EcoSim"
    }
    regression_model <- lm(y ~ x)
    
    # show regression model on coordinate system (straight line): 
    plot(x, y, xlab = x_method, ylab = y_method)
    abline(regression_model)
    dev.copy(png ,paste0("Results", describe_results_variable, "/Compare_all_methods/regression_plot_without_Ne.png"))
    dev.off()
    
    ##### Test model assumptions:
    
    # Explanations on model assumptions: 
    # A regression model is based on assumptions.
    # If the assumptions are not applicable, the model is doubtful.
    # Model assumptions: 
    # 1. Linearity:
    # this can be done visually: plot(regression_model, 1)
    # another way of displaying the plot: 
    # plot(fitted(regression_model),residuals(regression_model))
    # abline(0,0)
    # If data points are equally distributed beneath and above the abline, the linearity assumption is met. 
    # with gvlma() we can see the result of the linearity test at "Global Stat"
    # 2. Normality of residuals: 
    # plot(regression_model, 2)
    # shapiro.test(residuals(lm)) ## normally distributed if p-value > 0.05
    # The third option is testing with gvlma(). The Skewness and Kurtosis assumptions show that the distribution of the residuals are normal.
    # 3. Nature of the variable: 
    # Is your dependent variable truly continuous, or categorical? 
    # Rejection of the null (p < .05) indicates that you should use an alternative form of the generalized linear model (e.g. logistic or binomial regression).
    # 4. Homogeneity of variance
    # plot(regression_model, 3)
    # ncvTest(regression_model) ## p <0.05, suggests that the data is not homoscedastic
    # This test is also included in gvlma(). Look at "Heteroscedasticity".
    # 5. Independence: (no autocorrelation)
    # durbinWatsonTest(regression_model ) ## if p-value > 0.05 the assumption is met 
    
    
    
    # Test model assumptions:
    # First, a temporary file is created to save test results. 
    console_message_file_path <- paste0("Results", describe_results_variable, "/Compare_all_methods/model_assumptions.txt")
    console_message_file <- file(console_message_file_path, open = "wt")
    #gvlma(regression_model) ## with this command you can test 1. - 4. of the model assumptions.
    
    # save the results of the model assumptions test:
    capture.output(gvlma(regression_model), file = console_message_file)
    # load the message into R
    gvla_results <- readLines(paste0("Results", describe_results_variable, "/Compare_all_methods/model_assumptions.txt"))
    # terminate the sink() process (this command is probably an overkill command, but the usual sink() didn't work and I wasn't sure how else to solve the problem)
    closeAllConnections()
    # remove the file
    file.remove(console_message_file_path)
    
    # Test for independence: 
    Independence_test <- durbinWatsonTest(regression_model)
    
    
    # This code creates a summary vector of the results if all assumptions are met. 
    # If one or more assumptions are failing it tells you which.
    if (length(grep("acceptable", gvla_results, value = TRUE)) ==5 && Independence_test$p > 0.05) {
      # save results of regression model:
      # full results: 
      regression_model_summary <- summary(regression_model)
      # Multiple R-squared: How big is the influence of x on y ? How much can we explain with our model?
      modelled_influence_percentage <- regression_model_summary$r.squared *100
      # p_value:
      regression_p_value <- regression_model_summary$coefficients[8]
      
      ###### Save model results in the dataframe
      # Create a new row
      new_row <- data.frame(
        model_name = method_name,
        modelled_influence_percentage = modelled_influence_percentage,
        regression_p_value = regression_p_value
      )
      # Append results:
      regression_models_without_Ne <- rbind(regression_models_without_Ne, new_row)
      
    } else if (length(grep("acceptable", gvla_results, value = TRUE)) ==5 ) {
      cat(" There is an autocorrelation in the data for the ", method_name , "model. ")
    } else if (Independence_test$p > 0.05) {
      if (!grepl("acceptable", gvla_results[18])) {
        cat("The data is not linear of " , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[19])) {
        cat("The residuals are not normally distributed in "  , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[20])) {
        cat("The residuals are not normally distributed in " , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[21])) {
        cat("You might want to pick a logistic or binomial regression instead of the linear regression of  " , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[22])) {
        cat("The data is not homoscedastic of ",  method_name , "model. ")
      }
    } else {
      cat(" There is an autocorrelation in the data for the ", method_name , "model. ")
      if (!grepl("acceptable", gvla_results[18])) {
        cat("The data is not linear of " , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[19])) {
        cat("The residuals are not normally distributed in "  , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[20])) {
        cat("The residuals are not normally distributed in " , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[21])) {
        cat("You might want to pick a logistic or binomial regression instead of the linear regression of  " , method_name , "model. ")
      }
      if (!grepl("acceptable", gvla_results[22])) {
        cat("The data is not homoscedastic of ",  method_name , "model. ")
      }
    }
  }  
  # Because of the splitting in method name the rows will be doubled. 
  # Correct this error: 
  regression_models_without_Ne <- regression_models_without_Ne %>%
    group_by(modelled_influence_percentage, regression_p_value) %>%
    summarize(model_name = paste(model_name, collapse = "_")) %>%
    as.data.frame()    
  # Change the order of columns
  regression_models_without_Ne <- regression_models_without_Ne %>%
    select(model_name, modelled_influence_percentage, regression_p_value)
  
  
  # if the dataframe is not empty save it as csv.- file
  if (nrow(regression_models_without_Ne) > 0) {
    write.csv(regression_models_without_Ne, file =  paste0("Results", describe_results_variable, "/Compare_all_methods/regression_models_without_Ne.csv"), row.names = FALSE)
  }
  
} else {
  cat("None of the correlations without Ne have significant results.")
}

# Thank you Lukas Knob for explaining the basics of linear regressions in R!

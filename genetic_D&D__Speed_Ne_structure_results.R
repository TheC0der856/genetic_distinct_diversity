
# Continue with the code if Ne folder exists otherwise tell that the folder is missing
if (dir.exists(paste("Results", describe_results_variable,"/NeSpeed/input_files", sep = ""))) {

  # Continue with the code if Ne folder contains .txt-files otherwise tell that the folder does not contain files
  if (length(list.files(paste("Results", describe_results_variable,"/NeSpeed/input_files", sep = ""), pattern = "\\.txt$", full.names = TRUE)) > 0) {
    
    # Specify folder name to save results
    output_folder_path <- paste("Results", describe_results_variable,"/NeSpeed", sep = "")
    # Create folder if it does not already exist
    if (!file.exists(output_folder_path)) {
      dir.create(output_folder_path, recursive = TRUE)
    }
    
    
    ############################ Isolate results from Speed_Ne folder #################################
    
    # Create an empty data frame for the effective population sizes.
    Ne_results <- data.frame(
      site = numeric(0),
      Ne = numeric(0),
      lower_97_5_interval_percentile = numeric(0),
      upper_97_5_interval_percentile = numeric(0)
    )
    
    # Create a list of the files in my folder Speed_Ne:
    Speed_Ne_result_files <- list.files(path = paste("Results", describe_results_variable,"/NeSpeed/input_files", sep = ""), pattern = "\\.txt$", full.names = TRUE)
    # Exclude "r2_distributions.txt"
    Speed_Ne_result_files <- Speed_Ne_result_files[!grepl("r2_distributions", Speed_Ne_result_files)]
    
    # Set a counter. Later it will count every loop run.
    counter <- 0
    
    # Isolate the results of all files within the folder:
    for(Speed_Ne_result_file in Speed_Ne_result_files) {
      # remember the name of the site
      site_name <- sub("\\.txt$", "", basename(Speed_Ne_result_file))
      
      # read in the Speed Ne output file
      Speed_Ne_output <- readLines(Speed_Ne_result_file)
      
      # If this line "Estimates with Weir's (1979) S/(S-1) weighting of r^2 for bias correction." is present, isolate results, otherwise print an error message.
      if (length(grep("Estimates with Weir's", Speed_Ne_output)) > 0) {
      
        Speed_Ne_line_number <- grep("Estimates with Weir's" , Speed_Ne_output)
        
        number_of_result_line <- Speed_Ne_line_number[1] + 11 # percentile confidence interval, + 4 AWT
        number_of_result_line_parametric <- Speed_Ne_line_number[1] + 14 # normal distribution, +7 AWT
        
        ################### save numbers with percentile confidence intervals ######################
        # The following code isolates the results from the result line.
        # The results consist of Ne (first number),  
        # the lower end (second number) and the upper end (third number) of the 97.5% confidence interval.                                       
        # Check if the result consists of numbers and of infinite values:
        if (grepl("\\d+\\.?\\d*", Speed_Ne_output[number_of_result_line]) && grepl("infinity", Speed_Ne_output[number_of_result_line])) {
          # search for numbers in the text and store their positions
          position_numbers <- gregexpr("\\d+\\.?\\d*", Speed_Ne_output[number_of_result_line], perl = TRUE)
          # search for "infinite" in the text and store their positions
          position_infinites <- gregexpr("infinity", Speed_Ne_output[number_of_result_line])
          # extract the found numbers at the stored positions
          numbers <- unlist(regmatches(Speed_Ne_output[number_of_result_line], position_numbers))
          # combine the found numbers and "infinite" in the original order
          results_in_position <- character(length(Speed_Ne_output[number_of_result_line]))
          results_in_position[position_numbers[[1]]] <- numbers
          results_in_position[position_infinites[[1]]] <- "infinity"
          # Clean this vector from missing values and empty entries
          Speed_Ne_result <- results_in_position[results_in_position != "" & !is.na(results_in_position)]
        # If this is not the case, check if the result consists only of numbers:
        } else if (grepl("\\d+\\.?\\d*", Speed_Ne_output[number_of_result_line])) {
          # save all numbers:
          Speed_Ne_result <- as.numeric(unlist(regmatches(Speed_Ne_output[number_of_result_line], gregexpr("[0-9]+\\.*[0-9]*", Speed_Ne_output[number_of_result_line], perl = TRUE))))                                                    
        # If this is not the case, check if the result consists only of infinite values:
        } else if (grepl("infinity", Speed_Ne_output[number_of_result_line])) {
          # save all infinite values:
          Speed_Ne_result <- regmatches(Speed_Ne_output[number_of_result_line], gregexpr("infinity", Speed_Ne_output[number_of_result_line]))[[1]]
        # If there are no numbers neither infinite values, save a note for missing values.  
        } else {  
          Speed_Ne_result <- c(NA, NA, NA)
          cat("The result line does not contain numbers and does not contain infinity.")
        }
        
        ################### save numbers with parametric confidence intervals ######################
        # The following code isolates the results from the result line.
        # The results consist of Ne (first number),  
        # the lower end (second number) and the upper end (third number) of the 97.5% confidence interval.                                       
        # Check if the result consists of numbers and of infinite values:
        if (grepl("\\d+\\.?\\d*", Speed_Ne_output[number_of_result_line_parametric]) && grepl("infinity", Speed_Ne_output[number_of_result_line_parametric])) {
          # search for numbers in the text and store their positions
          position_numbers <- gregexpr("\\d+\\.?\\d*", Speed_Ne_output[number_of_result_line_parametric], perl = TRUE)
          # search for "infinite" in the text and store their positions
          position_infinites <- gregexpr("infinity", Speed_Ne_output[number_of_result_line_parametric])
          # extract the found numbers at the stored positions
          numbers <- unlist(regmatches(Speed_Ne_output[number_of_result_line_parametric], position_numbers))
          # combine the found numbers and "infinite" in the original order
          results_in_position <- character(length(Speed_Ne_output[number_of_result_line_parametric]))
          results_in_position[position_numbers[[1]]] <- numbers
          results_in_position[position_infinites[[1]]] <- "infinity"
          # Clean this vector from missing values and empty entries
          Speed_Ne_result_normal_dist <- results_in_position[results_in_position != "" & !is.na(results_in_position)]
          # If this is not the case, check if the result consists only of numbers:
        } else if (grepl("\\d+\\.?\\d*", Speed_Ne_output[number_of_result_line_parametric])) {
          # save all numbers:
          Speed_Ne_result_normal_dist <- as.numeric(unlist(regmatches(Speed_Ne_output[number_of_result_line_parametric], gregexpr("[0-9]+\\.*[0-9]*", Speed_Ne_output[number_of_result_line], perl = TRUE))))                                                    
          # If this is not the case, check if the result consists only of infinite values:
        } else if (grepl("infinity", Speed_Ne_output[number_of_result_line_parametric])) {
          # save all infinite values:
          Speed_Ne_result_normal_dist <- regmatches(Speed_Ne_output[number_of_result_line_parametric], gregexpr("infinity", Speed_Ne_output[number_of_result_line]))[[1]]
          # If there are no numbers neither infinite values, save a note for missing values.  
        } else {  
          Speed_Ne_result_normal_dist <- c(NA, NA, NA)
          cat("The result line does not contain numbers and does not contain infinity.")
        }
        
        
        # add results to the dataframe if they do not consist of missing values and there are three result values.
        if (!is.na(Speed_Ne_result[1]) && length(Speed_Ne_result) == 3) {
          # Create a new data_frame to add it later on to the already existing
          new_row <- data.frame(
            site = site_name,
            Ne = Speed_Ne_result[1],
            lower_97_5_interval_percentile = Speed_Ne_result[2],
            upper_97_5_interval_percentile = Speed_Ne_result[3], 
            lower_97_5_interval_parametric = Speed_Ne_result_normal_dist[2],
            upper_97_5_interval_parametric = Speed_Ne_result_normal_dist[3]
          )
          
          # Add the new row to the result table
          Ne_results <- rbind(Ne_results, new_row)
        } else {
          cat( "There are problems with site", site_name, ". " )
        } 
          
      } else {  
        new_row <- data.frame(
          site = site_name,
          Ne = NA,
          lower_97_5_interval_percentile = NA,
          upper_97_5_interval_percentile = NA, 
          lower_97_5_interval_parametric = NA, 
          upper_97_5_interval_parametric = NA
        )
        Ne_results <- rbind(Ne_results, new_row)
        cat("The line >>Estimates with Weir's (1979) S/(S-1) weighting of r^2 for bias correction.<< does not exist in the Speed Ne programm output.")
      }
      
    }  
    
    
    ###################### Detect outliers and clean up the data set ###################################
    
    # remove the row if it has a negative Ne, infinity or NA as Ne
    if ("infinity" %in% Ne_results$Ne || any(as.numeric(Ne_results$Ne) < 0) || any(is.na(Ne_results$Ne))) {
      Ne_results <- subset(Ne_results, !(Ne == "infinity" | as.numeric(Ne) < 0 | is.na(Ne)))
    }
    
    # # visualize outliers:
    # boxplot(as.numeric(Ne_results$Ne))
    # 
    # # save plot:
    # dev.copy(png, file.path(output_folder_path, "outliers_removed.png"))
    # dev.off()
    # 
    # # remove outliers: 
    # boxplot(as.numeric(Ne_results$Ne), plot=FALSE)$out
    # outliers <- boxplot(as.numeric(Ne_results$Ne), plot=FALSE)$out
    # to_remove <- which(as.numeric(Ne_results$Ne) %in% outliers)
    # if (length(to_remove) > 0) {
    #   Ne_results <- Ne_results[-to_remove, ]
    # }
    # The Code above uses the interquartile range method (IQR): 
    # This method should not be applied to all distributions! 
    # Be careful and check you distribution! 
    #shapiro.test(Ne_results$Ne)
    # This is in detail how the outliers are identified:
    #quartiles <- quantile(Ne_results$Ne, probs=c(.25, .75), na.rm = FALSE)
    #IQR <- IQR(Ne_results$Ne)
    #Lower <- quartiles[1] - 1.5*IQR
    #Upper <- quartiles[2] + 1.5*IQR 
    #Ne_results$Ne_cleaned <- subset(Ne_results$Ne, Ne_results$Ne > Lower & Ne_results$Ne < Upper)
    

    ######################### save table ####################################################
    
    
    # calculate proportions of Ne on a site to the Ne of all the other sites
    #Ne_results_with_percentages <- mutate(Ne_results, proportions_of_Ne = (as.numeric(Ne) / sum(as.numeric(Ne_results$Ne))*100))
    
    # save results as .csv -file
    write.csv(Ne_results, file = paste("Results", describe_results_variable, "/NeSpeed/Ne_results.csv", sep =""), row.names = FALSE)
    
    
    
    ######################### create confidence interval plot ##########################
    
    # make confidence interval values readable
    # replace "infinity" with "Inf"
    for (colname in colnames(Ne_results)) {
      Ne_results[[colname]] <- gsub("infinity", "Inf", Ne_results[[colname]])
    }
    
    # recognize if the infinity is negative
    for (index in 1:length(Ne_results$site)) {
      if (as.numeric(Ne_results[index, 2]) < as.numeric(Ne_results[index, 3])) {
        if (as.numeric(Ne_results[index, 4]) == Inf) {
          Ne_results[index, 4] <- -Inf
        }
      }
    }
    
    
    # create a plot with confidence intervals:
    # dev.off()
    # install ggplot package if you want to see this plot
    # create empty plot: 
    # confidence_intervals_plot <- ggplot(Ne_results, aes(x = site, y = as.numeric(Ne))) +
    #   geom_boxplot()
    # 
    # # Add confidence intervals:
    # confidence_intervals_plot + 
    #   geom_errorbar(aes(ymin = as.numeric(lower_97_5_interval_percentile), ymax = as.numeric(upper_97_5_interval_percentile)), width = 0.2) +
    #   labs(x = expression(bold("sites")), y = expression(bold("individuals"))) + 
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
    # 
    # save plot:
    # dev.copy(png, file.path(output_folder_path, "CI_Ne.png"))
    # dev.off()
    
  } else {
    print("The folder does not contain any .txt files.")
  }
   
} else {
  cat("There is no folder namend 'Speed_Ne'.")
}
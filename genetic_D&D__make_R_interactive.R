show_menu <- function() {
  while (TRUE) {
    cat("=========================== Menu ===========================\n")
    cat("This menu shows you possible methods to calculate genetic diversity and uniqueness.\n")
    cat("Type in the number that precedes the calculation method you want to use.\n")
    cat("If you want to process several data sets at once type L infront of the number representing the calculation method of your choice.\n")
    cat("1. Inform myself about the options \n")
    cat("2. Calculate lambda \n")
    cat("3. Calculate AMOVA \n")
    cat("4. Calculate overlaps \n")
    cat("     The prerequisite for calculating overlaps is the use of EcoSim-R. First download EcoSim-R: \n")
    cat("     https://www.uvm.edu/~ngotelli/EcoSim/EcoSim.html \n")
    cat("     After downloading EcoSim-R, save it in the folder of this R project, so that the further code can be executed. \n")
    cat("5. Calculate AvTD")
    cat("6. Calculate Ne with Ne Estimator and prepare input for Speed Ne")
    cat("7. Extract important numbers from Speed Ne output.")
    cat("8. Compare all methods\n")
    cat("9. Show citations\n")
    cat("0. Exit\n")
    
    choice <- readline("Choose an option: ")
    
    if (choice == "0") {
      cat("The program is terminated.\n")
      break
    } else if (choice == "1") {
      
      cat(" \n")
      cat(" \n") 
      cat("================= diversity indices =================.\n")
      cat(" \n")
      cat(" \n")
      cat("There are many ways to calculate diversity indices. In this case, poppr() from the package poppr is used. ")
      cat("Read more about poppr in this article:\n")
      cat("Kamvar, Z. N., Tabima, J. F., & Grünwald, N. J. (2014). Poppr: an R package for genetic analysis of populations with clonal, partially clonal, and/or sexual reproduction. PeerJ, 2, e281.\n")
      cat(" \n")
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat("================= AMOVA (Analyses of Molecular Variance) =================.\n")
      cat(" \n")    
      cat(" \n")   
      cat("AMOVA is a method applied to SNP and microsatellite datasets to determine whether there are significant differences in the genetic composition of a species at different locations. In this R-script, not all sites are compared with each other, but only one site is compared with the rest. The value indicating how significantly different this location is from the other locations is noted in a data frame, so that at the end this significance value can be compared between the locations.\n")
      cat(" \n")   
      cat("There are many ways to calculate an AMOVA. In this case, poppr.amova() from the package poppr is used. ")
      cat("Read more about poppr in this article:\n")
      cat("Kamvar, Z. N., Tabima, J. F., & Grünwald, N. J. (2014). Poppr: an R package for genetic analysis of populations with clonal, partially clonal, and/or sexual reproduction. PeerJ, 2, e281.\n")
      cat("Use the command ?poppr::poppr.amova to read more about poppr.amova().\n")
      cat(" \n")
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat(" \n")    
      cat(" \n")  
      cat("================= Calculating overlaps with EcoSimR using the Pianka Metric =================.\n")
      cat(" \n")    
      cat(" \n")   
      cat(" EcoSimR is a program designed to calculate niche overlaps of species in R. Instead of niche overlaps this R project calculates overlaps of allele occurences at different study sites. \n")
      cat(" Similar to the AMOVA calculation alleles occuring at one site are compared with alleles occuring at all the other sites for beeing able to assess which site shares most alleles with other sites and which site shares the least alleles with other sites. Only the observed overlap value is further processed and the simulated overlap value is left aside. This project kept the default settings. \n")
      cat(" \n")
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat(" \n")    
      cat(" \n")  
      cat("================= Calculating AvTD =================.\n")
      cat(" \n")    
      cat(" \n")   
      cat(" AvTD is a designed for species occurence data, but is applied to allele frequencies within this project. AvTD should capture the diversity and the distinctiveness. To read more about how it is done open the help page ?taxondive of the package vegan. \n")
      cat(" \n")
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat(" \n")    
      cat(" \n") 
      cat("================= Calculating Ne, the effective population size =================.\n")
      cat(" \n")    
      cat(" \n")   
      cat(" The effective population size is the minimum size of a population to keep the observed variation of alleles. Ne Estimator V2 can be calculated in R. All results will be saved and a extra csv file will be created saving Ne, the parametric and non parametric confidence intervals of each population if there is no threshold used for rare alleles. Meanwhile the input files for Speed Ne are created. To use the Speed Ne application Matlab needs to be installed first.\n")
      cat(" \n")
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat(" \n")    
      cat(" \n")  
      cat("================= Extract output from Speed Ne results =================.\n")
      cat(" \n")    
      cat(" \n")   
      cat(" This script creates a table saved as .csv-file. Content of this table is the AWT Ne calculation with rc² and corresponding confidence intervals of all sites that could be calculated. Ne without a threshold for rare alleles and parametric and non parametric confidence intervals are saved for that value.\n")
      cat(" \n")
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat("================= Compare all methods =================.\n")
      cat(" \n")    
      cat(" \n") 
      cat("The methods are compared by calculating the correlations and pie charts of the KBA criteria.")
      cat("To use this option of the menu it is necessary you have all results of all methods and all data sets used in corresponding paper. Although some data are normally distributed and suitable for a Pearson correlation, this project calculates the correlations with the Kendall method, because also the option is offered to calculate several correlations with each other and the comparability between the correlations suffers if different methods are used. The Kendall method is more precise than the Spearman method, which is why it was chosen.\n") 
      cat(" \n")
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat(" \n")    
      cat(" \n")

      
    } else if (choice == "2") { 
      source("genetic_D&D__lambda.R")
    } else if (choice == "L2") { 
      file_names <- list.files(pattern = "\\.xlsx$", full.names = TRUE)
      file_names <- basename(file_names)
      for  (file_name in file_names ) {
        file_path <- file_name
        # empty your environment to avoid complications
        # rm(list = ls())
        ploidy <- 2 # "1" and "2" are valid options
        method_type_to_handle_missing_values <- "geno" # you can decide between "ignore"/"asis" , "loci", "genotype", "mean" and "zero". For more information conduct ?missingno()
        threshold_percentage_of_missing_values_that_is_removed <- 0.2 #equals 20% 
        remove_sites <- 29 # Remove sites with too few individuals, define a threshold: if you type 5, all sites with 5 individuals or less will be deleted from your data set. 
        #####################################################################################################################################
        # Do not modify the code in this section!
        sheet <- "welcomeR"
        # load R Scripts
        # First, specify Calculate_genetic_distinctiveness_and_diversity as your R project to be able to use the following commands.
        source("genetic_D&D__libraries.R")
        source("genetic_D&D__functions.R")
        # load file
        original_file <- read_xlsx(file_path, sheet = sheet)
        # remove sites with too few individuals
        if (any(table(original_file$...2 < remove_sites))) {
          original_file <- remove_sites_with_too_few_individuals_from_dataset(original_file)
        }
        # This code helps to structure the results, especially if you have several data sets.
        describe_results_variable <- paste("_", tools::file_path_sans_ext(file_path), sep = "")
        # specify the name of the folder where the results will be saved
        output_folder_path <- paste("Results", describe_results_variable, sep = "")
        # If it does not exist already
        if (!file.exists(output_folder_path)) {
          # create folder
          dir.create(output_folder_path, recursive = TRUE)
        }
        # save the most necessary objects in the environment for being able to tidy up later
        all_objects_at_beginning_in_environment <- ls()
        all_objects_at_beginning_in_environment <- ls()
        source("genetic_D&D__lambda.R")
      }
    } else if (choice == "3") {
      source("genetic_D&D__AMOVA.R")
    } else if (choice == "L3") { 
      file_names <- list.files(pattern = "\\.xlsx$", full.names = TRUE)
      file_names <- basename(file_names)
      for  (file_name in file_names ) {
        file_path <- file_name
        # empty your environment to avoid complications
        # rm(list = ls())
        ploidy <- 2 # "1" and "2" are valid options
        method_type_to_handle_missing_values <- "geno" # you can decide between "ignore"/"asis" , "loci", "genotype", "mean" and "zero". For more information conduct ?missingno()
        threshold_percentage_of_missing_values_that_is_removed <- 0.2 #equals 20% 
        remove_sites <- 29 # Remove sites with too few individuals, define a threshold: if you type 5, all sites with 5 individuals or less will be deleted from your data set. 
        #####################################################################################################################################
        # Do not modify the code in this section!
        sheet <- "welcomeR"
        # load R Scripts
        # First, specify Calculate_genetic_distinctiveness_and_diversity as your R project to be able to use the following commands.
        source("genetic_D&D__libraries.R")
        source("genetic_D&D__functions.R")
        # load file
        original_file <- read_xlsx(file_path, sheet = sheet)
        # remove sites with too few individuals
        if (any(table(original_file$...2 < remove_sites))) {
          original_file <- remove_sites_with_too_few_individuals_from_dataset(original_file)
        }
        # This code helps to structure the results, especially if you have several data sets.
        describe_results_variable <- paste("_", tools::file_path_sans_ext(file_path), sep = "")
        # specify the name of the folder where the results will be saved
        output_folder_path <- paste("Results", describe_results_variable, sep = "")
        # If it does not exist already
        if (!file.exists(output_folder_path)) {
          # create folder
          dir.create(output_folder_path, recursive = TRUE)
        }
        # save the most necessary objects in the environment for being able to tidy up later
        all_objects_at_beginning_in_environment <- ls()
        all_objects_at_beginning_in_environment <- ls()
        source("genetic_D&D__AMOVA.R")
      }
    } else if (choice == "4") {
      source("genetic_D&D__Overlaps.R") 
    } else if (choice == "L4") {
      file_names <- list.files(pattern = "\\.xlsx$", full.names = TRUE)
      file_names <- basename(file_names)
      for  (file_name in file_names ) {
        file_path <- file_name
        # empty your environment to avoid complications
        # rm(list = ls())
        ploidy <- 2 # "1" and "2" are valid options
        method_type_to_handle_missing_values <- "geno" # you can decide between "ignore"/"asis" , "loci", "genotype", "mean" and "zero". For more information conduct ?missingno()
        threshold_percentage_of_missing_values_that_is_removed <- 0.2 #equals 20% 
        remove_sites <- 29 # Remove sites with too few individuals, define a threshold: if you type 5, all sites with 5 individuals or less will be deleted from your data set. 
        #####################################################################################################################################
        # Do not modify the code in this section!
        sheet <- "welcomeR"
        # load R Scripts
        # First, specify Calculate_genetic_distinctiveness_and_diversity as your R project to be able to use the following commands.
        source("genetic_D&D__libraries.R")
        source("genetic_D&D__functions.R")
        # load file
        original_file <- read_xlsx(file_path, sheet = sheet)
        # remove sites with too few individuals
        if (any(table(original_file$...2 < remove_sites))) {
          original_file <- remove_sites_with_too_few_individuals_from_dataset(original_file)
        }
        # This code helps to structure the results, especially if you have several data sets.
        describe_results_variable <- paste("_", tools::file_path_sans_ext(file_path), sep = "")
        # specify the name of the folder where the results will be saved
        output_folder_path <- paste("Results", describe_results_variable, sep = "")
        # If it does not exist already
        if (!file.exists(output_folder_path)) {
          # create folder
          dir.create(output_folder_path, recursive = TRUE)
        }
        # save the most necessary objects in the environment for being able to tidy up later
        all_objects_at_beginning_in_environment <- ls()
        all_objects_at_beginning_in_environment <- ls()
        source("genetic_D&D__Overlaps.R")
      }
    } else if (choice == "5") {
      source("genetic_D&D__AvTD.R")
    } else if (choice == "L5") {
      file_names <- list.files(pattern = "\\.xlsx$", full.names = TRUE)
      file_names <- basename(file_names)
      for  (file_name in file_names ) {
        file_path <- file_name
        # empty your environment to avoid complications
        # rm(list = ls())
        ploidy <- 2 # "1" and "2" are valid options
        method_type_to_handle_missing_values <- "geno" # you can decide between "ignore"/"asis" , "loci", "genotype", "mean" and "zero". For more information conduct ?missingno()
        threshold_percentage_of_missing_values_that_is_removed <- 0.2 #equals 20% 
        remove_sites <- 29 # Remove sites with too few individuals, define a threshold: if you type 5, all sites with 5 individuals or less will be deleted from your data set. 
        #####################################################################################################################################
        # Do not modify the code in this section!
        sheet <- "welcomeR"
        # load R Scripts
        # First, specify Calculate_genetic_distinctiveness_and_diversity as your R project to be able to use the following commands.
        source("genetic_D&D__libraries.R")
        source("genetic_D&D__functions.R")
        # load file
        original_file <- read_xlsx(file_path, sheet = sheet)
        # remove sites with too few individuals
        if (any(table(original_file$...2 < remove_sites))) {
          original_file <- remove_sites_with_too_few_individuals_from_dataset(original_file)
        }
        # This code helps to structure the results, especially if you have several data sets.
        describe_results_variable <- paste("_", tools::file_path_sans_ext(file_path), sep = "")
        # specify the name of the folder where the results will be saved
        output_folder_path <- paste("Results", describe_results_variable, sep = "")
        # If it does not exist already
        if (!file.exists(output_folder_path)) {
          # create folder
          dir.create(output_folder_path, recursive = TRUE)
        }
        # save the most necessary objects in the environment for being able to tidy up later
        all_objects_at_beginning_in_environment <- ls()
        all_objects_at_beginning_in_environment <- ls()
        source("genetic_D&D__AvTD.R")
      }
    } else if (choice == "6") {
      source("genetic_D&D__NeEstimator.R")
      source("genetic_D&D__Speed_Ne_split_input.R")
    } else if (choice == "L6") {
      file_names <- list.files(pattern = "\\.xlsx$", full.names = TRUE)
      file_names <- basename(file_names)
      for  (file_name in file_names ) {
        file_path <- file_name
        # empty your environment to avoid complications
        # rm(list = ls())
        ploidy <- 2 # "1" and "2" are valid options
        method_type_to_handle_missing_values <- "geno" # you can decide between "ignore"/"asis" , "loci", "genotype", "mean" and "zero". For more information conduct ?missingno()
        threshold_percentage_of_missing_values_that_is_removed <- 0.2 #equals 20% 
        remove_sites <- 29 # Remove sites with too few individuals, define a threshold: if you type 5, all sites with 5 individuals or less will be deleted from your data set. 
        #####################################################################################################################################
        # Do not modify the code in this section!
        sheet <- "welcomeR"
        # load R Scripts
        # First, specify Calculate_genetic_distinctiveness_and_diversity as your R project to be able to use the following commands.
        source("genetic_D&D__libraries.R")
        source("genetic_D&D__functions.R")
        # load file
        original_file <- read_xlsx(file_path, sheet = sheet)
        # remove sites with too few individuals
        if (any(table(original_file$...2 < remove_sites))) {
          original_file <- remove_sites_with_too_few_individuals_from_dataset(original_file)
        }
        # This code helps to structure the results, especially if you have several data sets.
        describe_results_variable <- paste("_", tools::file_path_sans_ext(file_path), sep = "")
        # specify the name of the folder where the results will be saved
        output_folder_path <- paste("Results", describe_results_variable, sep = "")
        # If it does not exist already
        if (!file.exists(output_folder_path)) {
          # create folder
          dir.create(output_folder_path, recursive = TRUE)
        }
        # save the most necessary objects in the environment for being able to tidy up later
        all_objects_at_beginning_in_environment <- ls()
        all_objects_at_beginning_in_environment <- ls()
        source("genetic_D&D__NeEstimator.R")
        source("genetic_D&D__Speed_Ne_split_input.R")
      }
    } else if (choice == "7") {
      source("genetic_D&D__Speed_Ne_structure_results.R")
    } else if (choice == "L7") {
      file_names <- list.files(pattern = "\\.xlsx$", full.names = TRUE)
      file_names <- basename(file_names)
      for  (file_name in file_names ) {
        file_path <- file_name
        # empty your environment to avoid complications
        # rm(list = ls())
        ploidy <- 2 # "1" and "2" are valid options
        method_type_to_handle_missing_values <- "geno" # you can decide between "ignore"/"asis" , "loci", "genotype", "mean" and "zero". For more information conduct ?missingno()
        threshold_percentage_of_missing_values_that_is_removed <- 0.2 #equals 20% 
        remove_sites <- 29 # Remove sites with too few individuals, define a threshold: if you type 5, all sites with 5 individuals or less will be deleted from your data set. 
        #####################################################################################################################################
        # Do not modify the code in this section!
        sheet <- "welcomeR"
        # load R Scripts
        # First, specify Calculate_genetic_distinctiveness_and_diversity as your R project to be able to use the following commands.
        source("genetic_D&D__libraries.R")
        source("genetic_D&D__functions.R")
        # load file
        original_file <- read_xlsx(file_path, sheet = sheet)
        # remove sites with too few individuals
        if (any(table(original_file$...2 < remove_sites))) {
          original_file <- remove_sites_with_too_few_individuals_from_dataset(original_file)
        }
        # This code helps to structure the results, especially if you have several data sets.
        describe_results_variable <- paste("_", tools::file_path_sans_ext(file_path), sep = "")
        # specify the name of the folder where the results will be saved
        output_folder_path <- paste("Results", describe_results_variable, sep = "")
        # If it does not exist already
        if (!file.exists(output_folder_path)) {
          # create folder
          dir.create(output_folder_path, recursive = TRUE)
        }
        # save the most necessary objects in the environment for being able to tidy up later
        all_objects_at_beginning_in_environment <- ls()
        all_objects_at_beginning_in_environment <- ls()
        source("genetic_D&D__Speed_Ne_structure_results.R")
      }
    } else if (choice == "8") {
      
      source("genetic_D&D__correlations.R")
      source("genetic_D&D__KBA_criteria.R")
      
    } else if (choice == "9") {
      
      print(citation("adegenet"))
      print(citation("dplyr"))
      print(citation("tidyr"))
      print(citation("readxl"))
      print(citation("vegan"))
      print(citation("poppr"))
      cat( "Nicholas J. Gotelli, Edmund M. Hart and Aaron M. Ellison (2015) EcoSimR: Null model analysis for ecological data. R package version 0.1.0. http://github.com/gotellilab/EcoSimR doi:10.5281/zenodo.16522 \n ")
      cat( "Corresponding BibTeX entry:
        
        @Manual{,
          title = {EcoSimR: Null model analysis for ecological data},
          author = {Nicholas J. Gotelli and Edmund M. Hart and Aaron M.
            Ellison},
          year = {2015},
          note = {R package version 0.1.0},
          url = {http://github.com/gotellilab/EcoSimR},
          doi = {10.5281/zenodo.16522},
        }
       \n" )
      print(citation("corrplot"))

      
    } else {
      cat("This option is invalid. Select an option from the menu. \n")
    }
    
    cat("\n")
  }
}
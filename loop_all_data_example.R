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
  
  source("genetic_D&D__allelic_richness.R")
}
  

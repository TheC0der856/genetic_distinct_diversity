
############################ Create an empty space to save results ####################################

# Specify folder name
output_folder_path <- paste("Results", describe_results_variable, "/Overlaps", sep = "")
# Create folder if it does not already exist
if (!file.exists(output_folder_path)) {
  dir.create(output_folder_path, recursive = TRUE)
}

#specify folder name
output_folder_path <- paste("Results", describe_results_variable, "/Overlaps/EcoSim_files", sep = "")
# Create folder if it does not already exist
if (!file.exists(output_folder_path)) {
  # create folder
  dir.create(output_folder_path, recursive = TRUE)
}


# Specify where to store the result table of EcoSim-R, if you want to store it as .xlsx - file. 
#save_results_path <- paste("Results", describe_results_variable, "/Overlaps/Overlaps_results.xlsx", sep = "")


######################### Calculate allele frequencies #####################################


# Save the ID of the individual as a vector to add it later to the genind object
individual <- as.vector(original_file[1]$...1)
population <- as.vector(original_file$...2)
# df2genind() assumes a data frame, not a tribble object
# sort data frame, so all individuals of a population should follow each other without gaps
original_file <- as.data.frame(original_file)[order(as.data.frame(original_file)[, 2]), ]
# remove information about site and ID from the data frame
data_frame_without_information <- original_file %>% select(-c(1, 2)) 

if (ploidy == 2 ) {
  # The missing values:
  # Missing values must be converted to NAs to be recognized as missing values by the Genind object. 
  # Information is lost about a known allele next to a missing allele at a locus.  
  # Negative numbers (e.g. -9) and 0 are searched for and transformed into NAs.
  
  # convert 0 into -9:
  # it is easier to search for a - instead of 0s later on and replace them like this
  for (locus in names(data_frame_without_information)) { # all column names
    for (i in 1:nrow(data_frame_without_information)) { # index every row
      # if the data point is not NA, but is 0 it can be converted (asking only for 0s if there are NAs in the data set, can cause trouble)
      if (!is.na(data_frame_without_information[i, locus]) && data_frame_without_information[i, locus] == 0) {
        data_frame_without_information[i, locus] <- 0  # convert 0!
      } else if (is.na(data_frame_without_information[i, locus])) {
        data_frame_without_information[i, locus] <- 0  # convert <NA>! why? because <NA> needs to be the entry for both alleles and not only one.
      }
    }
  }
  # Codominate, diploid data set requires one loci name per column, both alleles are separated by " ".
  data_frame_formatted <- data_frame_without_information  
  for (columns_number in 1:(length(colnames(data_frame_without_information)))) {
    if (columns_number %% 2 != 0) {
      data_frame_formatted <- data_frame_formatted %>%
        unite(!!colnames(data_frame_without_information)[columns_number], c(!!colnames(data_frame_without_information)[columns_number], !!colnames(data_frame_without_information)[columns_number+1]), sep = " ")
    }
  }
  # convert negative numbers into 0:
  for (locus in names(data_frame_formatted)) {
    missing_values <- grepl("-", data_frame_formatted[, locus])
    if (any(missing_values)) {
      data_frame_formatted[missing_values, locus] <- 0
    }
  }
  # convert "NA" into 0
  for (locus in names(data_frame_formatted)) {
    missing_values <- grepl("NA", data_frame_formatted[, locus])
    if (any(missing_values)) {
      data_frame_formatted[missing_values, locus] <- 0
    }
  }
  # How many missing values does my data set have?
  # sum(is.na(data_frame_formatted))
  
  
} else if (ploidy == 1) {
  data_frame_formatted <- data_frame_without_information
  # convert negative numbers:
  for (locus in names(data_frame_formatted)) {
    missing_values <- grepl("-", data_frame_formatted[, locus])
    if (any(missing_values)) {
      data_frame_formatted[missing_values, locus] <- 0
    }
  }
  # # convert 0:
  # for (locus in names(data_frame_formatted)) {
  #   #  Check if " " occurs in the column
  #   if (any(data_frame_formatted[, locus] == 0)) {
  #     # replace "0" with "NA" in every column
  #     data_frame_formatted[data_frame_formatted[, locus] == 0, locus] <- 0
  #   }
  # }
} else {
  cat("This ploidy cannot be calculated.")
}


#if there is any "." in colnames it should be replaces by "_" otherwise there will be error messages
colnames(data_frame_formatted) <- gsub("\\.", "_", colnames(data_frame_formatted))


Genind <- df2genind(data_frame_formatted, pop= population, ind.names = individual , sep = " ", ploidy = ploidy)
# check if the user determined a threshold for missing values that should be removed
if (exists('threshold_percentage_of_missing_values_that_is_removed')) { # if the threshold exists
  Genind <- missingno(Genind, type = method_type_to_handle_missing_values, cutoff = threshold_percentage_of_missing_values_that_is_removed, quiet = FALSE, freq = FALSE)
} else {
  Genind <- missingno(Genind, type = method_type_to_handle_missing_values, quiet = FALSE, freq = FALSE)
}  


########################### Calculate allele frequencies ######################################
# set a counter
locus_index <- 0
# and an empty dataframe
allele_frequencies <- data.frame(locus_allel_name = character(), stringsAsFactors = FALSE)

for (locus in names(Genind@all.names)) {
  # With each loop pass over the loci, the locus_index is increased by 1 (counts at which locus is currently being calculated).
  locus_index <- locus_index + 1
  # allele per locus:
  locus_allel_name <- colnames(Genind@tab)[grepl(names(Genind@all.names)[locus_index], colnames(Genind@tab))]
  
  # Create an empty data frame to store results:
  # The first column in the data frame contains the names of the alleles.
  # All further columns should later contain the allele frequencies for each population of the investigated locus.
  # The population names are already entered as column labels of the still empty columns.
  
  
  # Small information on the subject of missing values:
  # If 0s and negative numbers are not converted into NAs the following command can be used to omit missing values (-9):
  # locus_allel_name <- locus_allel_name[!grepl("-9", locus_allel_name)]
  # the calculation will produce similar results like GenAlEx if you use these commands:
  # sum_allele_frequencies_for_locus_for_pop <- colSums(allele_frequency[, -1])
  # allele_frequency <- sweep(allele_frequency[, -1], 2, sum_allele_frequencies_for_locus_for_pop, " ")
  # after "Add the frequencies of the allele in the population to the data frame:"
  # Eventhough information is lost about a known allele next to a missing allele at a locus, 
  # I decided to use missingno() to handle missing data instead of the "locus_allel_name <- locus_allel_name[!grepl("-9", locus_allel_name)]"-calculation, 
  # because missingno() provides many options for the user and is more difficult to avoid in the AMOVA calculation (all methods should be as comparable as possible)
  # If you want to change this Code to loose fewer information, I consider "locus_allel_name <- locus_allel_name[!grepl("-9", locus_allel_name)]" as a valid option. 
  # Other calculation methods to handle missing values could be also inserted here e.g. the "mean" method of missingno()...
  
  
  allele_frequency <- data.frame(locus_allel_name)
  for (population in unique(Genind@pop)) {
    name_of_column <- paste(population)
    allele_frequency[[name_of_column]] <- 0
  }
  
  # For all populations, the frequency of an allele in the population is to be calculated and stored:
  for (population in unique(Genind@pop)) {
    # calculate the frequency for each allele in the population:
    allele_frequencies_for_locus_for_population <- sapply(locus_allel_name, function(locus_allel_name) {
      allele_per_individual_in_population <- Genind@tab[Genind@pop == population, locus_allel_name]
      sum(allele_per_individual_in_population)
    })
    # Add the frequencies of the allele in the population to the data frame:
    allele_frequency[, as.character(population)] <- allele_frequencies_for_locus_for_population
    # Rename data frame after locus name
    assign(locus, allele_frequency)
  }
}
# All created tables are merged into one large table
for (locus in names(Genind@all.names)) {
  # Call up data frame
  little_allele_table <- get(locus)
  # Add data frame to large table
  allele_frequencies <- rbind(allele_frequencies, little_allele_table)
}
# Reset the row labels in the large table
rownames(allele_frequencies) <- NULL


# In order to use the allele frequencies for further calculations, the data must be cleaned from error sources introduced by missingno().
# If the "mean" option is selected, rounding of the values is necessary.
allele_frequencies <- round(allele_frequencies[-1])
# Lines that add up to 0 must be deleted.
row_sum_is_zero <- allele_frequencies[rowSums(allele_frequencies[-1] != 0) == 0, ]
allele_frequencies <- allele_frequencies[!(rownames(allele_frequencies) %in% rownames(row_sum_is_zero)), ]


######################## Create input files for the EcoSim-R program ####################

# All sites to be analysed with Overlaps: 
sites <- colnames(allele_frequencies)


# Add a new TOTAL column to the table
allele_frequencies <- allele_frequencies %>%
  mutate(TOTAL = rowSums(select(.))) 
# Fill the new column TOTAL with the sum of all allele frequencies
allele_frequencies <- allele_frequencies %>%
  mutate(TOTAL = rowSums(select(., -TOTAL)))


# In the way I wrote this code this is a requirement for the creation of small tables - it might be possible to leave this step and find another wording for the creation of small tables.
# Create a new column counting the examined alleles (all alleles on all loci)
# number of alleles:
number_of_alleles <- nrow(allele_frequencies)
# create a vector with numbers from one to the number of alleles
count_alleles <- seq(1, number_of_alleles)
# add a new column with the counter for the rows/the number of alleles
allele_frequencies <- allele_frequencies %>%
  mutate(count_alleles)



# Create small tables:
# A table should be created for each location
for (site in sites) {
  # All tables should be saved in the folder "EcoSim_files"
  file_path <- file.path(output_folder_path, paste0(site, ".csv"))
  # The tables should consist of the counter for the number of alleles, the site, and the sum of the rest of the sites respectively all sites minus the site
  input_file_for_EcoSim_site_vs_rest <- allele_frequencies %>%
    select(count_alleles, {{site}}, TOTAL = TOTAL) %>%
    mutate(!!paste0("TOTAL_minus_", site) := TOTAL - .data[[site]]) %>%
    select(-TOTAL)
    write.csv(input_file_for_EcoSim_site_vs_rest, file = file_path, row.names = FALSE)
}




####################    EcoSim R ausführen    ###############################################

##############################################
## EcoSimR: R code for Null Model Analysis
##
##
##
##############################################
## EcoSimR Niche Overlap Shell
## Nicholas J. Gotelli & Aaron M. Ellison
##
##
##############################################
## Version 1.00
## 15 June 2013
#############################################
## Modified on 18 May 2013 by NJG to pass a single Param.List to all functions
#############################################
####For beginners - start here####

## clean the slate ##
#rm(list=ls())   # remove all objects in memory


## load EcoSimR

# It is important to specify the file path correctly
# For example it can be done like this:
#Pfad_zu_EcoSimR_Funktionen <- "C:/Users/gronefeld/Desktop/Overlaps-Test/EcoSimR_1.00_24Jun2013/"
#source(file.path(Pfad_zu_EcoSimR_Funktionen, "EcoSimR - Main Source.R"))

# If all scripts of EcoSim-R are copied into the project "Calculating genetic diversity and distinctivness", you can use this command:
source(file.path("EcoSimR - Main Source.R"))

#############################################
## Model input parameters
## USER CAN MODIFY PARAMETERS IN THIS SECTION


# Add a slash after output_folder_path
output_folder_path <- paste0(output_folder_path , "/")

# Calculate the Overlaps index for every file in the folder "EcoSim_files"
# Only a txt. -file is saved with the observed and simulated mean and variance, the plot is not saved!
# Nevertheless the plot can cause the error message: "figure margins too large". If it happens enlarge the window at the bottom right side and try to execute the code again.
for(file in list.files(output_folder_path)[which(substring(list.files(output_folder_path),nchar(list.files(output_folder_path))-3,nchar(list.files(output_folder_path))) == ".csv")]) {
  # If you want to test only file, ignore the loop and type
  #file <- "name_of_the_file.csv"
  
  # Create a file path for every file in the folder and import file into EcoSim-R
  # Data.File is the file path leading to the file, the name is inherent to EcoSim-R
  Data.File <- paste0(output_folder_path,file) 
  
  Output.File <-"Niche Overlap Output.txt"
  Algorithm <- "RA3"	#choices are "RA1", "RA2", "RA3", "RA4"; default is "RA3"
  Metric <- "Pianka"	#choices are "Pianka", "Czekanowski", 
                                #"Pianka.var", "Czekanowski.var",
                                # "Pianka.skew", "Czekanowski.skew"; default is Pianka
  N.Reps <- 1000	# 1000 is the typical number of replicates, but any number > 2 will run
  Random.Seed <- 625 ## If 0, uses random integer. User can replace 0 with your integer of choice e.g. 107
  Plot.Output <- "screen" 	#choices are "file", "screen", "none"; default is "screen"
  Print.Output <- "screen"	#choices are "file", "screen", "none"; default is "screen"
  Display.About <- "none" # choices are "screen", "none"; default is "none"
  Graphic <- "Niche.Overlap.Plot" # other choices will be added with other modules
  #############################################
  
  
  ##############################################
  ## Execute analyses
  ## Beginning users should NOT modify this section
  ##
  ## First command initialized the parameter list from the user inputs
  ## Second command runs niche overlap analysis using Data.File, Algorithm, and Metric from user inputs
  ## Third command outputs graphics and statistics to devices specified from user inputs
  
  
  Param.List <- Get.Params(Data.File,Output.File,Algorithm,Metric,
                           N.Reps,Random.Seed,Plot.Output,Print.Output,Display.About,Graphic)
  RandomInteger <- Set.The.Seed(Param.List)
  
  
  Null.Result <- Null.Model.Engine(Param.List)
  
  # sink() saves the output in a .txt-file
  sink(paste0(substring(Data.File, 1, (nchar(Data.File)-4)),".txt"), append= FALSE)
  Output.Results(Param.List,Null.Result)
  sink()
}

# Thank you for your help Lukas Knob to automate this process!



#
#
#
#
##
###
################## Results of EcoSim-R should be recapped in a table ###################

# function to extract numbers
extract_number <- function(line) {
  # regular expression to extract the number
  pattern <- "\\d+\\.\\d+"
  # search for the number in the line
  match <- regmatches(line, regexpr(pattern, line))
  # If a number was found it should be returned, otherwise NA should be returned
  if (length(match) > 0)
    as.numeric(match)
  else
    NA
}

# Create an empty list for observed indices of all sites
observed_indices_list <- list()

# remove "///" in the end of output_folder_path introduced by EcoSimR
output_folder_path <- gsub("/+$", "", output_folder_path)

# Copy observed indices of all sites from the .txt -files into a list 
EcoSim_results_files <- list.files(path = paste(output_folder_path), pattern = "\\.txt$", full.names = TRUE)
for (EcoSim_results_file in EcoSim_results_files) {
  # extract file name
  file_name <- sub("_edited\\.txt$", "", basename(EcoSim_results_file))
  # read the content of the file in R
  file_content <- readLines(EcoSim_results_file)
  # extract observed index
  observed_index <- extract_number(file_content[9])
  #simulated_index <- extract_number(file_content[10]
  #simulated_variance <- extract_number(file_content[11])
  #
  # add data to the list
  observed_indices_list[[file_name]] <- data.frame(site = file_name, observed_overlap = observed_index[1])
}

# Create a table
Overlaps_results <- do.call(rbind, observed_indices_list)


############################ edit table ############################################

# remove .txt-ending at the site  column 
Overlaps_results$site <- gsub("\\.txt$", "", Overlaps_results$site)

# If all values are negative
if (all(Overlaps_results$observed_overlap < 0)) {
  # Convert negative values to positive values
  Overlaps_results$observed_overlap <- abs(Overlaps_results$observed_overlap)
}


# Calculate the percentage of observed niche overlap:
# Prepare an empty vector for the percentages, the length of which corresponds to the number of rows in the results table.
percentage <- numeric(length(Overlaps_results$site))
# calculate the sum of all observed indices
sum_overlap<- sum(Overlaps_results$observed_overlap)
# Calculate percentages
for (site_Index in seq_along(Overlaps_results$site)) {
  observed_index <- Overlaps_results$observed_overlap[[site_Index]]
  percentage[site_Index] <- (observed_index/ sum_overlap) * 100
}
# Add the percentages to the table
Overlaps_results$observed_overlap_percentages <- percentage

# remove "niche_overlap_observed_index" from my results
# Overlaps_results <- subset(Overlaps_results, select = -niche_overlap_observed_index)


# Note on the calculation of KBA Thresholds: 
# A high overlap in alleles is the opposite of a high percentage of rare alleles.
# A high observed overlap should meet a low KBA rating




############################ save file ############################

# Use this code, if you prefer an .xlsx -file
## create Excel-file 
#wb <- createWorkbook()
#addWorksheet(wb, "Overlaps-Index")
#writeData(wb, sheet = "Overlaps-Index", Overlaps_results, startRow = 1, startCol = 1, colNames = TRUE)
## save Excel-file 
#saveWorkbook(wb, file = paste("Results", describe_results_variable, "/Overlaps/Overlaps_results.xlsx", sep = ""))


# save results as .csv -file
write.csv(Overlaps_results, file = paste("Results", describe_results_variable, "/Overlaps/Overlaps_results.csv", sep = ""), row.names = FALSE)



########################## tidy up #####################
dev.off()

# save all objects in your environment
all_objects <- ls()
all_objects <- all_objects[all_objects != "Overlaps_results"]

# Remove all objects except the most necessary ones you want to keep
objects_to_remove <- setdiff(all_objects, all_objects_at_beginning_in_environment)
rm(list = objects_to_remove)




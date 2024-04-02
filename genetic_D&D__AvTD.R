# Calculate AvTD 
############################ Create an empty space to save results ####################################

# Specify folder name
output_folder_path <- paste("Results", describe_results_variable, "/AvTD", sep = "")
# Create folder if it does not already exist
if (!file.exists(output_folder_path)) {
  dir.create(output_folder_path, recursive = TRUE)
}


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

################# Calculate AvTD ################
# switch columns and rows
t_allele <- t(allele_frequencies)
# Taxonomic distances from a classification table with variable step lengths
taxdis <- taxa2dist(allele_frequencies, varstep=TRUE)
# Calculate AvTD
mod <- taxondive(t_allele, taxdis)

############### Save results ####################
write.csv(summary(mod), file = paste(output_folder_path, "/AvTD_results.csv", sep = ""), row.names = TRUE)


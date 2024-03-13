
##################### create a folder to save results ##############

# this is were results should be saved. The folder is also needed to conduct an AMOVA. 
# Create a folder for AMOVA results
# Specify folder name
output_folder_path <- paste("Results", describe_results_variable, "/AMOVA", sep = "")
# Create folder if it does not already exist
if (!file.exists(output_folder_path)) {
  dir.create(output_folder_path, recursive = TRUE)
}

###################### Reformat table to be able to convert it to a Genind object ############


# Save the ID of the individual as a vector to add it later to the genind object
individual <- as.vector(original_file[1]$...1)
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
        data_frame_without_information[i, locus] <- -9  # convert 0!
      } else if (is.na(data_frame_without_information[i, locus])) {
        data_frame_without_information[i, locus] <- -9  # convert <NA>! why? because <NA> needs to be the entry for both alleles and not only one.
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
  # convert negative numbers into NA:
  for (locus in names(data_frame_formatted)) {
    missing_values <- grepl("-", data_frame_formatted[, locus])
    if (any(missing_values)) {
      data_frame_formatted[missing_values, locus] <- NA
    }
  }
  # convert "NA" into NA
  for (locus in names(data_frame_formatted)) {
    missing_values <- grepl("NA", data_frame_formatted[, locus])
    if (any(missing_values)) {
      data_frame_formatted[missing_values, locus] <- NA
    }
  }
  # How many missing values does my data set have?
  # sum(is.na(data_frame_formatted))
  
  
} else if (ploidy == 1) {
  data_frame_formatted <- data_frame_without_information
  # convert 0:
  for (locus in names(data_frame_formatted)) {
    #  Check if " " occurs in the column
    if (any(data_frame_formatted[, locus] == 0)) {
      # replace "0" with "NA" in every column
      data_frame_formatted[data_frame_formatted[, locus] == 0, locus] <- NA
    }
  }
  # convert negative numbers:
  for (locus in names(data_frame_formatted)) {
    missing_values <- grepl("-", data_frame_formatted[, locus])
    if (any(missing_values)) {
      data_frame_formatted[missing_values, locus] <- NA
    }
  }
} else {
  cat("This ploidy cannot be calculated.")
}

#if there is any "." in colnames it should be replaces by "_" otherwise there will be error messages
colnames(data_frame_formatted) <- gsub("\\.", "_", colnames(data_frame_formatted))


########################### Prepare and execute AMOVA ########################################


# Set populations for the Genind object consisting of site and rest
for (site in unique(pull(original_file, ...2) )) {
  column_site <- data.frame(original_file[, 2])
  column_site <- ifelse(column_site[, 1] != site, "rest", column_site[, 1])
  assign(site, column_site)
}

# create an empty list for the Phi values of the AMOVA:
# the phi value tells how significant the variances are between sites (as opposed to within sites).
AMOVA_Phi_List <- list()
AMOVA_difference_between_sites_percent <- list()

for (site in unique(pull(original_file, ...2)) ) { 
  # remember names of sites
  name_site <- site
  # create a Genind object for each site with the populations: site and rest
  column_site <- get(site)
  Genind <- df2genind(data_frame_formatted, pop= column_site, ind.names = individual , sep = " ", ploidy = ploidy)
  # View Genind with Genind and summary(Genind)
  # add strata, requirement for poppr.amova()
  Genind@strata <- as.data.frame(column_site) 
  nameStrata(Genind) <- ~population
  # Convert the Genind object to a Genclone object, requirement for poppr.amova()
  genclone_object <- as.genclone(Genind)
  # check if the user determined a threshold for missing values that should be removed
  if (exists('threshold_percentage_of_missing_values_that_is_removed')) { # if the threshold exists
    # Calculate AMOVA with threshold
    AMOVA <- poppr.amova(genclone_object, ~population, missing = method_type_to_handle_missing_values, cutoff = threshold_percentage_of_missing_values_that_is_removed)
  } else {
    # Calculate AMOVA with the default setting
    AMOVA <- poppr.amova(genclone_object, ~population, missing = method_type_to_handle_missing_values )
  }
  # an error message about missing contrast can occur if method_type_to_handle_missing_values = "geno" and there is one locus with NA only for one site
  
  # Transfer the significance values of the AMOVA into a list  
  AMOVA_Phi_List[[name_site]] <- AMOVA$statphi[1, "Phi"]
  AMOVA_difference_between_sites_percent[[name_site]] <- AMOVA$componentsofcovariance[1, "%"]
  
}



# Create empty vector for results
AMOVA_Phis <- numeric(length(AMOVA_Phi_List))
AMOVA_differences <- numeric(length(AMOVA_difference_between_sites_percent))

# Isolate all values of how significant the difference is between the sites of the list of Phi values
for (site_Index in seq_along(AMOVA_Phi_List)) {
  AMOVA_Phi <- AMOVA_Phi_List[[site_Index]]
  AMOVA_Phis[site_Index] <- AMOVA_Phi
  AMOVA_difference <- AMOVA_difference_between_sites_percent[[site_Index]]
  AMOVA_differences[site_Index] <- AMOVA_difference
  
}

# Create a results table
amova_results <- data.frame ( site = names(AMOVA_Phi_List), 
                              Difference = AMOVA_differences, 
                              Significance = AMOVA_Phis)



# create a table like in the descriptions of the KBA standard
# 
# ############################ calculate percentages of variance between sites ####################################################
# 
# # sum the genetic variations at all sites:
# Sum_AMOVA_Phi <-sum(unlist(AMOVA_Phi_List))
# # Create empty vector for results
# percentage <- numeric(length(AMOVA_Phi_List))
# # Calculate percentages
# for (site_Index in seq_along(AMOVA_Phi_List)) {
#   AMOVA_Phi <- AMOVA_Phi_List[[site_Index]]
#   percentage[site_Index] <- (AMOVA_Phi/ Sum_AMOVA_Phi) * 100
# }
# 
# 
# ###################################### Display results ###################################
# 
# # Create table (for an assignment to the site)
# results_table <- data.frame ( site = names(AMOVA_Phi_List), 
#                               Difference = percentage)
# 
# 
# # sort the table with the sites from A to Z
# results_table <- results_table[order(results_table$site), ]
# # round results_table 
# #results_table$Difference <- round(results_table$Difference, 3)
# # add another column "Threshold_met":
# results_table$Threshold_met <- ifelse(results_table$Difference > 10, "B1, A1b", 
#                                              ifelse(results_table$Difference > 1, "A1b", ""))
# # Rename column headers:
# colnames(results_table) <- c("Site", "Difference [%]", "Threshold met")
# 
# 
# 
# # Rename the table so that the results of this calculation method do not get mixed up with the results of other calculation methods.
# amova_results <- results_table
# 

################################### save results ##########################################################

 ## Save as an Excel file
## Create or open Excel file
#wb <- createWorkbook()
#addWorksheet(wb, "AMOVA")
#writeData(wb, sheet = "AMOVA", amova_results, startRow = 1, startCol = 1, colNames = TRUE)
## save datafile
#saveWorkbook(wb, file = paste("Results", describe_results_variable, "/AMOVA/amova_results.xlsx", sep = ""))


# Save as .csv -file
write.csv(amova_results, file = paste("Results", describe_results_variable, "/AMOVA/amova_results.csv", sep = ""), row.names = FALSE)






########################## tidy up #####################

# save all objects in your environment
all_objects <- ls()
all_objects <- all_objects[all_objects != "amova_results"]

# Remove all objects except the most necessary ones you want to keep
objects_to_remove <- setdiff(all_objects, all_objects_at_beginning_in_environment)
rm(list = objects_to_remove)

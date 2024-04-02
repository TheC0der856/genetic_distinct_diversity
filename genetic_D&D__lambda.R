
########### Create a folder where results can be saved ##############################
# Specify folder name
output_folder_path <- paste("Results", describe_results_variable, "/diversity", sep = "")
# Create folder if it does not already exist
if (!file.exists(output_folder_path)) {
  dir.create(output_folder_path, recursive = TRUE)
}

########### Create a genclone object to use poppr() function ###########################
# reformat table to create a Genind object fist.
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

Genind <- df2genind(data_frame_formatted, pop= population, ind.names = individual , sep = " ", ploidy = ploidy)
# check if the user determined a threshold for missing values that should be removed
if (exists('threshold_percentage_of_missing_values_that_is_removed')) { # if the threshold exists
  Genind <- missingno(Genind, type = method_type_to_handle_missing_values, cutoff = threshold_percentage_of_missing_values_that_is_removed, quiet = FALSE, freq = FALSE)
} else {
  Genind <- missingno(Genind, type = method_type_to_handle_missing_values, quiet = FALSE, freq = FALSE)
}  
genclone_object <- as.genclone(Genind)

####################### Mayor calculations to asses the genetic diversity ###########
# genetic diversity calculations using poppr()
# Shannon-Wiener index (H), Stoddart and Taylor’s index (G), and Simpson’s index (lambda) show the diversity.
genetic_diversity <- poppr(genclone_object)

# genetic diversity correction for sample size: 
# In all cases, scaling either Stoddart and Taylor’s G or Shannon and Wiener’s  H by sample size should be avoided. 
N      <- genetic_diversity$N      # number of samples
lambda <- genetic_diversity$lambda # Simpson's index
corrected_Simpson <- (N/(N - 1)) * lambda              # Corrected Simpson's index

# substitute File in genetic_diversity data frame by (N/(N - 1)) * lambda (genetic diversity corrected for sample size)
genetic_diversity$`(N/(N - 1)) * lambda` <- corrected_Simpson
genetic_diversity <- genetic_diversity[, !names(genetic_diversity) %in% "File"]


# save data: 
write.csv(genetic_diversity, file = paste("Results", describe_results_variable, "/diversity/diversity_results.csv",  sep = ""), row.names = FALSE)


# Indices of diversity depend on the number of genotypes in the sample (richness) and how they are distributed in the sample (eveness).

# genetic richness
# MLG and eMLG (eMLG is corrected by the sample size using rarefaction)
# In most cases, sample sizes differ and use of the rarefaction method to calculate richness is more appropriate (Grünwald et al. 2003).
E.tab <- mlg.table(genclone_object, plot = FALSE)
min_sample <- min(rowSums(E.tab))

png(file.path(output_folder_path, "sample_size_richness.png"))
rarecurve(E.tab, sample = min_sample, xlab = "Sample Size", ylab = "Expected MLGs")
dev.off()

# have a look at the evenness
png(file.path(output_folder_path, "evenness.png"))
E.tab <- mlg.table(genclone_object)
dev.off()



########################## tidy up #####################

# save all objects in your environment
all_objects <- ls()
all_objects <- all_objects[all_objects != "amova_results"]

# Remove all objects except the most necessary ones you want to keep
objects_to_remove <- setdiff(all_objects, all_objects_at_beginning_in_environment)
rm(list = objects_to_remove)


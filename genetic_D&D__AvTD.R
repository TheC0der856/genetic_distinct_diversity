# Calculate AvTD 
############################ Create an empty space to save results ####################################

# Specify folder name
output_folder_path <- paste("Results", describe_results_variable, "/AvTD", sep = "")
# Create folder if it does not already exist
if (!file.exists(output_folder_path)) {
  dir.create(output_folder_path, recursive = TRUE)
}


######################### Format data frame into an Genind Object #####################################
######################### to enable easy processing with R ############################################

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
  
  # convert 0 into NA:
  # it is easier to search for a - instead of 0s later on and replace them like this
  for (locus in names(data_frame_without_information)) { # all column names
    for (i in 1:nrow(data_frame_without_information)) { # index every row
      # if the data point is not NA, but is 0 it can be converted (asking only for 0s if there are NAs in the data set, can cause trouble)
      if (!is.na(data_frame_without_information[i, locus]) && data_frame_without_information[i, locus] == 0) {
        data_frame_without_information[i, locus] <- NA  # convert 0!
      } else if (is.na(data_frame_without_information[i, locus])) {
        data_frame_without_information[i, locus] <- NA  # convert <NA>! why? because <NA> needs to be the entry for both alleles and not only one.
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
  # convert negative numbers:
  for (locus in names(data_frame_formatted)) {
    missing_values <- grepl("-", data_frame_formatted[, locus])
    if (any(missing_values)) {
      data_frame_formatted[missing_values, locus] <- NA
    }
  }
  # convert 0:
  for (locus in names(data_frame_formatted)) {
     #  Check if " " occurs in the column
     if (any(data_frame_formatted[, locus] == 0)) {
       # replace "0" with "NA" in every column
       data_frame_formatted[data_frame_formatted[, locus] == 0, locus] <- NA
     }
   }
} else {
  cat("This ploidy cannot be calculated.")
}


#if there is any "." in colnames it should be replaces by "_" otherwise there will be error messages
colnames(data_frame_formatted) <- gsub("\\.", "_", colnames(data_frame_formatted))


Genind <- df2genind(data_frame_formatted, pop= population, ind.names = individual , sep = " ", ploidy = ploidy)

######################### Remove bad quality data ##################################################

# check if the user determined a threshold for missing values that should be removed
if (exists('threshold_percentage_of_missing_values_that_is_removed')) { # if the threshold exists
  Genind <- missingno(Genind, type = method_type_to_handle_missing_values, cutoff = threshold_percentage_of_missing_values_that_is_removed, quiet = FALSE, freq = FALSE)
} else {
  Genind <- missingno(Genind, type = method_type_to_handle_missing_values, quiet = FALSE, freq = FALSE)
}  

################################### calculate distances ############################

# Calculate allele abundances per population 
# default setting will calculate the mean of NA
allele_abundances <- genind2genpop(Genind)


if (!exists("distance_method")) {
  
  # switch columns and rows
  allele_abundances_tab <- t(allele_abundances@tab)
  # Taxonomic distances from a classification table with variable step lengths
  taxdis <- taxa2dist(allele_abundances_tab, varstep=TRUE)
  # Calculate AvTD
  mod <- taxondive(allele_abundances@tab, taxdis)
} else {
  # Calculate allele frequencies
  allele_frequencies <- makefreq(allele_abundances,missing="mean",quiet=TRUE)
  X <- t(allele_frequencies)
  
  # Berechnung aus ?dist.genpop()
  if (distance_method == "Edward") {
    # Edwards distance
    # Also have a look at: ?edwards.dist() of poppr package
    # nloc can be translated into the number of columns/ populations/ areas
    nloc <- ncol(X)
    X <- sqrt(X)
    d <- X%*%t(X)
    d <- 1-d/nloc
    diag(d) <- 0
    d <- sqrt(d)
    d <- as.dist(d)
  } else if (distance_method == "Nei"){
    # Neis distance
    d <- X%*%t(X)
    vec <- sqrt(diag(d))
    d <- d/vec[col(d)]
    d <- d/vec[row(d)]
    d <- -log(d)
    d <- as.dist(d)
  } else if (distance_method == "pairwise"){
    # raw pairwise distances
    # https://rdrr.io/r/stats/dist.html , 
    # https://grunwaldlab.github.io/poppr/reference/poppr.amova.html 
    d <- dist(X, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
  } else {
    cat("The method you specified to calculate the distance between loci is not existing.")
  }
  
  
  ################# Calculate AvTD ################
  # pure d can produce NaN, that's why d will be scaled
  # the highest value should be 100 likewise to taxondive
  # preserve the ratio between all distances within the matrix
  max <- max(d)
  multiplier <- 100 /max
  scaled_d <- d * multiplier
  # calculate AvTD
  # plot(hclust(scaled_d), hang = -1)
  mod <- taxondive(allele_abundances@tab, scaled_d)
  #summary(mod)
  
}  
  
############### Save results ####################
write.csv(summary(mod), file = paste(output_folder_path, "/AvTD_results.csv", sep = ""), row.names = TRUE)


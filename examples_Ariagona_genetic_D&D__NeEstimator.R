

# create a directory to save results if it does not exist already
path_directory <- paste("Results", describe_results_variable, "/NeEstimator", sep = "")
if (!file.exists(path_directory)) {
  dir.create(path_directory, recursive = TRUE)
}
# make sure results are saved in the folder for Ne results (created in R)
setwd(path_directory)

# rename original_file
Ne_Estimator_file <- original_file

# Format input file similar to the example file given from the package RLDNe
# Format Ne_Estimator_file until it is similar to "wgp_example_2col"

# change tibble object into a data frame
Ne_Estimator_file <- as.data.frame(Ne_Estimator_file)
# if there are "G"s, "T"s , "A"s and "C"s in your data set they have to be replaced by 1, 2, 3 and 4
Ne_Estimator_file <- as.data.frame(lapply(Ne_Estimator_file, function(x) {
  ifelse(grepl("^A$", x), "1", x)
}))
Ne_Estimator_file <- as.data.frame(lapply(Ne_Estimator_file, function(x) {
  ifelse(grepl("^C$", x), "2", x)
}))
Ne_Estimator_file <- as.data.frame(lapply(Ne_Estimator_file, function(x) {
  ifelse(grepl("^G$", x), "3", x)
}))
Ne_Estimator_file <- as.data.frame(lapply(Ne_Estimator_file, function(x) {
  ifelse(grepl("^T$", x), "4", x)
}))
# rename population (site) column and ID (individuals) column
colnames(Ne_Estimator_file)[colnames(Ne_Estimator_file) == "...2"] <- "pop"
colnames(Ne_Estimator_file)[colnames(Ne_Estimator_file) == "...1"] <- "ind_id"
# remember names of the data frame as they are right now
existing_names <- colnames(Ne_Estimator_file)
# create empty vectors
old_names <- character(0)
new_names <- character(0)
# fill the vectors with content: names that should be changed (old_names) and names for replacement (new_names)
for (col_name in existing_names) {
  # search for names in the data frame that should be changed, e.g. "...4"
  if (grepl("^\\.\\..\\d+$", col_name)) {  
    # save the names that should be changed
    old_names <- c(old_names, col_name)
    # search for the name of the locus:
  } else if (col_name != "pop" && col_name != "ind_id") { 
    # replace 1 in the end of the locus and transform it into 2 ("Dru_10722_92_1" into "Dru_10722_92_2")
    col_name <- paste0(col_name, "_2")
    # save the newly transformed names
    new_names <- c(new_names, col_name)
  }
}
# replace old names with new names
for (i in 1:length(old_names)) {
  colnames(Ne_Estimator_file)[colnames(Ne_Estimator_file) == old_names[i]] <- new_names[i]
}

# Delete individuals with too much missing data
# Create an index vector to keep track of individuals to be removed
individuals_to_remove <- numeric()
# find individuals
for (individual in 1:nrow(Ne_Estimator_file)) {
  # count missing data in every row of the data set 
  count_missing_data <- sum(is.na(Ne_Estimator_file[individual,]) | Ne_Estimator_file[individual,] <= 0, na.rm = TRUE)
  # calculate ratio of missing data
  columns <- ncol(Ne_Estimator_file)
  ratio <- count_missing_data / columns
  # add the individual to a removal vector if they have more missing data than the threshold ratio
  if (ratio > threshold_percentage_of_missing_values_that_is_removed) {
    individuals_to_remove <- c(individuals_to_remove, individual)
  }
}
# Delete individuals
if (length(individuals_to_remove) != 0) {
  Ne_Estimator_file <- Ne_Estimator_file[-individuals_to_remove, ]
}

# only comment when running Ariagona file!
# # check if all sites still have more than the threshold of individuals per site. Otherwise remove the site from the analyses.
# # initiate a list for the populations with to few individuals
# remove_populations <- c()
# # make a note if there are less individuals left than the threshold
# for (site in unique(Ne_Estimator_file$pop)) {
#   count <- sum(Ne_Estimator_file$pop == site) # count individuals per population
#   # add them
#   if (count < remove_sites+1){
#     remove_populations <- c(remove_populations, site)
#   }
# }
# # Delete population
# Ne_Estimator_file <- subset(Ne_Estimator_file, !(pop %in% remove_populations))

# convert negative values, 0 and "NA" into <NA>
for (locus in names(Ne_Estimator_file[-1:-2])) {
  negative_values <- grepl("-", Ne_Estimator_file[, locus])
  missing_values <- grepl("NA", Ne_Estimator_file[, locus])
  summen <- sapply(Ne_Estimator_file[, locus], function(x) sum(as.numeric(x), na.rm = TRUE))    
  if (any(negative_values)) { # convert negative numbers into <NA>:
    Ne_Estimator_file[negative_values, locus] <- NA
  } else if (any(missing_values)) { # convert "NA" into <NA>
    Ne_Estimator_file[missing_values, locus] <- NA
    # When the sum is 0, 
  } else if (any(summen == 0)) {
    # replace the value with NA.
    Ne_Estimator_file[summen == 0, locus] <- NA
  }
}


# ncode is the number of characters per allele
for (locus in names(Ne_Estimator_file[-1:-2])) {
  for (allele in Ne_Estimator_file[, locus]){
    if (!is.na(allele)){
      ncode <- nchar(allele)
      break
    }
  }
}

# How to communicate ploidy with NeEstimator
if (ploidy==1){ploidy <- F} else if (ploidy == 2) {ploidy <- T} else {
  cat("There is no function which can calculate this ploidy.")
}

# if the ploidy is 1 there can be a problem with an "uneven number of alleles" , sadly there is no manual or example code. 
if (ploidy == F) {
  if (ncol(Ne_Estimator_file) %% 2 != 0) {
    Ne_Estimator_file <- Ne_Estimator_file[,-3]
  }
}

# only for Ariagona: 
# renaming of columns
names(Ne_Estimator_file) <- gsub("_1_2", "_2", names(Ne_Estimator_file))
names(Ne_Estimator_file) <- gsub("L", "Locus_", names(Ne_Estimator_file) )
# create integers
Ne_Estimator_file[] <- lapply(Ne_Estimator_file, function(x) {
  if (all(x %in% c("1", "2", "3", "4", NA))) {
    return(as.integer(x))  
  } else {
    return(x)  
  }
})
# change switch pop and ind_id
Ne_Estimator_file <- Ne_Estimator_file[, c("pop", "ind_id", setdiff(names(Ne_Estimator_file), c("pop", "ind_id")))]
# sort alphabetical order of pop
Ne_Estimator_file <- Ne_Estimator_file[order(Ne_Estimator_file$pop), ]
# replace names through pop-1, pop-2, etc...
pop_levels <- unique(Ne_Estimator_file$pop)
Ne_Estimator_file$pop <- paste("pop", match(Ne_Estimator_file$pop, pop_levels), sep = "-")
#replace ind names
Ne_Estimator_file$ind_id <- paste("ind", seq_along(Ne_Estimator_file$ind_id), sep = "-")
  
# generate the effective population size: 
colnames(Ne_Estimator_file) <- gsub("_(\\d)$",replacement = "\\.A\\1",colnames(Ne_Estimator_file))
      
efgl <- readInData(Ne_Estimator_file,genotypeStart = 3,pedigreeColumn = 1,nameColumn = 2)
rldne <- exportGenePop_RLDNe(EFGLdata = efgl)
rldne <- create_LDNe_params(rldne)
std_out <- run_LDNe(rldne)
df <- read_LDNeOutFile(rldne)
        
# use original names for Pop
df$Pop <-  rep(sort(unique(original_file$...2)), each = 4)

# save results
write.csv(df, file = "C:/Users/Gronefeld/Desktop/D&D/Calculate_D&D_shortened/Results_Ariagona/NeEstimator/Ne_results.csv", row.names = FALSE)

df[df$CritValue == 0.05, c("Pop", "Ne")]

# go back to the directory of the project, to enable running other scripts after this script
setwd("..")
setwd("..")

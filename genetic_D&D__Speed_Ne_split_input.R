
# folder path to save input files for Matlab
folderpath <- paste("Results", describe_results_variable,"/NeSpeed/", sep = "")
if (!file.exists(folderpath)) {
  dir.create(folderpath)
} else {
}
folderpath <- paste("Results", describe_results_variable,"/NeSpeed/input_files/", sep = "")
if (!file.exists(folderpath)) {
  dir.create(folderpath)
} else {
}


# rename the file
Speed_Ne_file <- as.data.frame(original_file)

# If there are "NA"s in the Speed_Ne_file they have to be substituted by 0for (locus in names(data_frame_without_information)) { # all columnnames
for (locus in names(Speed_Ne_file)) { # all column names
  for (i in 1:nrow(Speed_Ne_file)) { # index every row
    if (is.na(Speed_Ne_file[i, locus])) {
      Speed_Ne_file[i, locus] <- -9  # convert <NA> into 0!
    } else if (grepl("NA", Speed_Ne_file[i , locus])) {
      Speed_Ne_file[i, locus] <- -9 # convert "NA" into 0!
    } else if (Speed_Ne_file[i, locus] == 0) {
      Speed_Ne_file[i, locus] <- -9
    }
  }
}


################################# Create files containing tables for every site ####################################################

# Remember names of the sites. This script only works if names of the sites are in the second column!
sites <- unique(Speed_Ne_file[, 2])


# create an empty list for the data of each site 
# the list has the length as the number of sites existing
data_sites_list <- vector("list", length(sites)) 
# every list element is named after a site
names(data_sites_list) <- sites


# add data of the sites to the empty list and delete individuals with to much missing data
for (site in sites) {
  does_the_row_contain_site <- Speed_Ne_file[, 2] == site
  data_site <- Speed_Ne_file[does_the_row_contain_site, ] # load the data
  rows <- nrow(data_site) # number of individuals
  # every row is TRUE at first (means less missing data than 20%), until proven wrong
  rows_to_keep <- rep(TRUE, rows)
  # check all rows/individuals for missing data
  for (row in 1:rows) {
    #calculate the percentage of missing data for every individual
    columns <- ncol(data_site) # number of columns
    count_missing_data <- sum(data_site[row, ] == -9) 
    percentage <- count_missing_data / columns
    # if one individual has more missing data than the threshold make a note
    if (percentage > threshold_percentage_of_missing_values_that_is_removed) {
      rows_to_keep[row] <- FALSE
    }
  }
  # only keep sites with missing values below the threshold
  data_site <- data_site[rows_to_keep, ]
  # only save the site if there are still more than 30 individuals left or a different threshold set at the beginning
  rows <- nrow(data_site) # number of individuals left in the data set for that site
  if (rows > remove_sites+1){
    data_sites_list[[site]] <- data_site
  }
  # save the data of each site as a single .csv- document
  file_name <- paste(site, ".csv", sep = "")
  write.csv(data_site[, -c(1, 2)], file = file.path(folderpath, file_name), row.names = FALSE)
}


# The files contain column names that are confusing for Matlab and must be deleted.

# Load the input files back into R
input_files <- list.files(folderpath, pattern = "\\.csv$", full.names = TRUE)


# Loop to delete the first line and overwrite the files
for (file in input_files) {
  text <- readLines(file)
  text <- gsub("\"", "", text) #remove ""
  text <- text[-1]
  writeLines(text, file)
}


########################## tidy up #####################

# save all objects in your environment
all_objects <- ls()

# Remove all objects except the most necessary ones you want to keep
objects_to_remove <- setdiff(all_objects, all_objects_at_beginning_in_environment)
rm(list = objects_to_remove)



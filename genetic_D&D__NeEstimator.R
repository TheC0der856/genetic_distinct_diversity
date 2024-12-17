

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

# check if all sites still have more than the threshold of individuals per site. Otherwise remove the site from the analyses.
# initiate a list for the populations with to few individuals
remove_populations <- c()
# make a note if there are less individuals left than the threshold
for (site in unique(Ne_Estimator_file$pop)) {
  count <- sum(Ne_Estimator_file$pop == site) # count individuals per population
  # add them
  if (count < remove_sites+1){
    remove_populations <- c(remove_populations, site)
  }
}
# Delete population
Ne_Estimator_file <- subset(Ne_Estimator_file, !(pop %in% remove_populations))

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

# if the ploidy is 1 there can be a problem with an "uneven number of alleles" , sadly there is no manual or example code. Thats why I did not know how to help myself in a different way than deleting on allele (locus)...
if (ploidy == F) {
  if (ncol(Ne_Estimator_file) %% 2 != 0) {
    Ne_Estimator_file <- Ne_Estimator_file[,-3]
  }
}
  
# generate the effective population size: 
genotype_examp<-alleles2genotypes(df = Ne_Estimator_file,allele_cols = 3:ncol(Ne_Estimator_file),allelesAsIntegers = TRUE)
gp_file<-write_genepop_zlr(loci = genotype_examp[,3:ncol(genotype_examp)],pops = genotype_examp$pop,ind.ids = genotype_examp$ind_id,folder = "",filename ="genepop_output.txt",missingVal = NA,ncode = ncode,diploid = ploidy)
param_files<- NeV2_LDNe_create(input_file = gp_file$Output_File ,param_file = "Ne_params.txt" ,NE_out_file = "Ne_out.txt")
run_LDNe(LDNe_params = param_files$param_file)
Ne_estimates<-readLDNe_tab(path = param_files$Ne_out_tab)
  

 
# Ne output will be saved as "Ne_outxLD.txt"
# load Ne output into R
Ne_output_txt <- readLines("Ne_outxLD.txt")
Ne_output_txt <- gsub("Infinite", "Inf", Ne_output_txt) # Inf will always interpreted as positive even if it is not
 
# create an empty data frame to save Ne for every site
Ne_R_results <- data.frame(
  site = character(0),
  Ne = numeric(0),
  lower_jackknife_confidence_interval = numeric(0),  #we recommend the general use of the jackknife CIs, particularly when the number of loci is large (>100) NeEstimatorV2.1 Help file
  upper_jackknife_confidence_interval = numeric(0)
)

# at line 16 the first site starts (have a look at sample size at .txt- document) 
# with the lowest allele frequency of 0.1
# I would like to take all allele frequencies into account no matter how low they were
# for this reason my Nes have higher numbers than calculation with a restrinction in allele frequencies
# I decided to do so, because I do not have any lower limit for the other calculations as well.
# (page 22 "Critical Values" of help file NeEstimator2.1)
 
# at line 19 we have the results of the first site without any allele frequency restrictions.
# at line 17 we have a threshold of 0.05
counter <- 17 
for (site in unique(Ne_Estimator_file$pop)) {
  # simplify reading the important lines
  line_of_Ne_output_text <- strsplit(Ne_output_txt[counter], " ")[[1]]
  just_numbers <- line_of_Ne_output_text[line_of_Ne_output_text != ""]
  # search for the important numbers
  Ne <- as.numeric(unlist(just_numbers)[6])
  lower_jackknife_confidence_interval <- as.numeric(unlist(just_numbers)[9]) 
  upper_jackknife_confidence_interval <- as.numeric(unlist(just_numbers)[10])
  lower_parametric_confidence_interval <- as.numeric(unlist(just_numbers)[7])
  upper_parametric_confidence_interval <- as.numeric(unlist(just_numbers)[8])
  
  # move on to the next site
  counter <- counter+4 
  # transfer the important numbers into a data frame
  Ne_R_results <- rbind(Ne_R_results, data.frame(site = site, Ne = Ne, lower_jackknife_confidence_interval = lower_jackknife_confidence_interval, upper_jackknife_confidence_interval = upper_jackknife_confidence_interval, lower_parametric_confidence_interval = lower_parametric_confidence_interval, upper_parametric_confidence_interval = upper_parametric_confidence_interval))
}
 
# recognize if Inf is a negative value
for (index in 1:length(Ne_R_results$site)) {
  if (Ne_R_results[index, 2] < Ne_R_results[index, 3] && Ne_R_results[index, 4] == Inf) {
    Ne_R_results[index, 4] <- -Inf
  }
}
 
# delete negative values 
if (any(Ne_R_results$Ne < 0)) {
   Ne_R_results <- Ne_R_results[Ne_R_results$Ne >= 0, ]
}
 
# save results as a .csv- file
# make sure the final results are saved in the correct folder
write.csv(Ne_R_results, file = "Ne_results.csv", row.names = FALSE)

# go back to the directory of the project, to enable running other scripts after this script
setwd("..")
setwd("..")

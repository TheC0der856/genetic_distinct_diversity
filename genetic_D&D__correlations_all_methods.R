
# remove all objects in the memory - can be useful while working on this script
#rm(list=ls())   

# make sure the results of all methods are present:
# Load the diversity table into R, if it does not exist already.
diversity_results <- read.csv(paste("Results", describe_results_variable, "/diversity/diversity_results.csv", sep = ""))
# Load the Overlaps table into R
Overlaps_results <- read.csv(paste("Results", describe_results_variable, "/Overlaps/Overlaps_results.csv", sep = ""))
# Load the Amova table into R
amova_results <- read.csv(paste("Results", describe_results_variable, "/AMOVA/amova_results.csv", sep = "", quote = ""))
# Load the Ne table into R (Ne-Estimator)
Ne_results <- read.csv(paste("Results", describe_results_variable, "/NeEstimator/Ne_results.csv", sep = "", quote = ""))  
# Load the Ne table into R (Speed-Ne)
NeS_results <- read.csv(paste("Results", describe_results_variable, "/NeSpeed/Ne_results.csv", sep = "", quote = ""))  

################################## Testing correlations #########################################

if (Ne_results[[1]][1] <= NeS_results[[1]][1]) {
  # Prepare data for correlation calculations with Ne:
  # For analyses including Ne rows of the other methods most likely must be removed, because it might have been impossible to calculate Ne for all sites.
  # remove information about confidence interval to keep it simple
  all_results_with_Ne <- NeS_results[, -c(3, 4, 5)]
  colnames(all_results_with_Ne)[2] <- "NeS"
  # create new columns for every calculation method
  all_results_with_Ne$allelic_diversity <- NA
  all_results_with_Ne$observed_overlap <- NA
  all_results_with_Ne$Difference <- NA
  all_results_with_Ne$Ne <- NA
  # Append Ne table, add results of other calculations. 
  # Thereby unnecessary rows are removed automatically from the data set. 
  for(site in all_results_with_Ne$site) {
    all_results_with_Ne$allelic_diversity[which(all_results_with_Ne$site == site)] <- diversity_results$X.N..N...1.....lambda[which(diversity_results$Pop == site)]
    all_results_with_Ne$observed_overlap[which(all_results_with_Ne$site == site)] <- Overlaps_results$observed_overlap[which(Overlaps_results$site == site)]
    all_results_with_Ne$Difference[which(all_results_with_Ne$site == site)] <- amova_results$Difference [which(amova_results$site == site)]
    all_results_with_Ne$Ne[which(all_results_with_Ne$site == site)] <- Ne_results$Ne[which(NeS_results$site == site)]
  }
} else {
  # Prepare data for correlation calculations with Ne:
  # For analyses including Ne rows of the other methods most likely must be removed, because it might have been impossible to calculate Ne for all sites.
  # remove information about confidence interval to keep it simple
  all_results_with_Ne <- Ne_results[, -c(3, 4, 5, 6)]
  # create new columns for every calculation method
  all_results_with_Ne$allelic_diversity <- NA
  all_results_with_Ne$observed_overlap <- NA
  all_results_with_Ne$Difference <- NA
  all_results_with_Ne$NeS <- NA
  # Append Ne table, add results of other calculations. 
  # Thereby if a row has missing data in at least one of the calculations the row will just be filled with NA. 
  i <- 1
  while (i <= nrow(all_results_with_Ne)) {
    site <- all_results_with_Ne$site[i]
    # Check whether there is a result for the site in NeS_results
    match_index <- which(NeS_results$site == site)
    if (length(match_index) > 0) {  
      all_results_with_Ne$NeS[i] <- NeS_results$Ne[match_index]
    }
    # write results of other methods in data frame
    all_results_with_Ne$allelic_diversity[i] <- diversity_results$X.N..N...1.....lambda[which(diversity_results$Pop == site)]
    all_results_with_Ne$observed_overlap[i] <- Overlaps_results$observed_overlap[which(Overlaps_results$site == site)]
    all_results_with_Ne$Difference[i] <- amova_results$Difference[which(amova_results$site == site)]
    # remove rows from the loop: they should be NA
    if (nrow(all_results_with_Ne[rowSums(!is.na(all_results_with_Ne[, -1])) > 1 & rowSums(is.na(all_results_with_Ne[, -1])) > 0, ]) > 0) {
      all_results_with_Ne <- all_results_with_Ne[-which(rowSums(!is.na(all_results_with_Ne[, -1])) > 1 & rowSums(is.na(all_results_with_Ne[, -1])) > 0), ]
    }
    # Increment the index for the next run
    i <- i + 1
  }
}


# Note: 
# It is possible to visually estimate whether a correlation would be conceivable by creating a graph
# with the results of one method on the x -axis and the results of the other method on the y -axis. 
# Here are two examples: 
# plot(all_results_with_Ne$Ne,all_results_with_Ne$allelic_diversity)
# plot(amova_results$Difference, Overlaps_results$observed_overlap)

correlation_method <- "kendall" # can be "pearson", "kendall" or "spearm"
# To know which correlation method is best to use the distribution of the data should be known. 
# Tests if data is normally distributed:
# test_normal_distribution_Ne <- shapiro.test(as.numeric(all_results_with_Ne$Ne))
# test_normal_distribution_Rarefunction_cut <- shapiro.test(all_results_with_Ne$allelic_diversity)
# test_normal_distribution_Overlaps_cut <- shapiro.test(all_results_with_Ne$observed_overlap)
# test_normal_distribution_Amova_cut <- shapiro.test(all_results_with_Ne$Difference)
# test_normal_distribution_Amova <- shapiro.test(amova_results$Difference)
# test_normal_distribution_Overlaps <- shapiro.test(Overlaps_results$observed_overlap)
# test_normal_distribution_Rarefunction <- shapiro.test(diversity_results$allelic_diversity)
# 
# normally distributed if p-value > 0.05
# test_normal_distribution_Ne$p.value
# test_normal_distribution_Rarefunction_cut$p.value
# test_normal_distribution_Overlaps_cut$p.value
# test_normal_distribution_Amova_cut$p.value
# test_normal_distribution_Rarefunction$p.value
# test_normal_distribution_Overlaps$p.value
# test_normal_distribution_Amova$p.value

# If the data sets are normally distributed, a Pearson correlation would be best,
# but not all example file result data sets are normally distributed! 
# And with testing more files the situation will become more complicated.
# Therefore I am going to apply a Kendall correlation test on all possible combinations of method result data sets.
# A Kendall correlation test is more robust and slightly more efficient than Spearman's rank correlation. This is why I prefer Kendall to Spearman.



# Testing correlations:
# The independent variable is named at the beginning and the dependent variable at the second place.


#### Testing correlations only with sites where Ne could be calculated: 
# Test the correlation between Ne and allelic diversity
Ne_diversity_correlation_test <- cor.test(as.numeric(all_results_with_Ne$Ne), as.numeric(all_results_with_Ne$allelic_diversity), method = correlation_method)
# save p-value
Ne_diversity_correlation_p_value <- Ne_diversity_correlation_test$p.value
# save the estimated correlation coefficient
Ne_diversity_correlation_coefficient <- Ne_diversity_correlation_test$estimate

# Test the correlation between Ne and rare alleles
Ne_Amova_correlation_test <- cor.test(as.numeric(all_results_with_Ne$Ne),as.numeric(all_results_with_Ne$Difference), method = correlation_method)
# save p-value
Ne_Amova_correlation_p_value <- Ne_Amova_correlation_test$p.value
# save the estimated correlation coefficient
Ne_Amova_correlation_coefficient <- Ne_Amova_correlation_test$estimate

# Test the correlation between Ne and shared alleles
Ne_Overlaps_correlation_test <- cor.test(as.numeric(all_results_with_Ne$Ne),as.numeric(all_results_with_Ne$observed_overlap), method = correlation_method)
# save p-value
Ne_Overlaps_correlation_p_value <- Ne_Overlaps_correlation_test$p.value
# save the estimated correlation coefficient
Ne_Overlaps_correlation_coefficient <- Ne_Overlaps_correlation_test$estimate

# Test the correlation between allele diversity and shared alleles looking only at sites with Ne calculated
diversity_Overlaps_correlation_test_cut <- cor.test(as.numeric(all_results_with_Ne$allelic_diversity), as.numeric(all_results_with_Ne$observed_overlap), method = correlation_method)
# save p-value
diversity_Overlaps_correlation_p_value_cut <- diversity_Overlaps_correlation_test_cut$p.value
# save the estimated correlation coefficient
diversity_Overlaps_correlation_coefficient_cut <- diversity_Overlaps_correlation_test_cut$estimate

# Test the correlation between allele diversity and rare alleles looking only at sites with Ne calculated
diversity_Amova_correlation_test_cut <- cor.test(as.numeric(all_results_with_Ne$allelic_diversity), as.numeric(all_results_with_Ne$Difference), method = correlation_method)
# save p-value
diversity_Amova_correlation_p_value_cut <- diversity_Amova_correlation_test_cut$p.value
# save the estimated correlation coefficient
diversity_Amova_correlation_coefficient_cut <- diversity_Amova_correlation_test_cut$estimate

# Test the correlation between rare alleles and shared alleles looking only at sites with Ne calculated
# Here I find it difficult to evaluate what is the independent variable and what is the dependent variable. 
# You could probably look at it the other way around too.
Amova_Overlaps_correlation_test_cut <- cor.test(as.numeric(all_results_with_Ne$Difference), as.numeric(all_results_with_Ne$observed_overlap), method = correlation_method) 
# save p-value
Amova_Overlaps_correlation_p_value_cut <- Amova_Overlaps_correlation_test_cut$p.value
# save the estimated correlation coefficient
Amova_Overlaps_correlation_coefficient_cut <- Amova_Overlaps_correlation_test_cut$estimate

# Test the correlation between Ne calculated with different calculation methods
NeS_Ne_correlation_test_cut <- cor.test(as.numeric(all_results_with_Ne$Ne), as.numeric(all_results_with_Ne$NeS), method = correlation_method)
# save p-value
NeS_Ne_correlation_p_value_cut <- NeS_Ne_correlation_test_cut$p.value
# save the estimated correlation coefficient
NeS_Ne_correlation_coefficient_cut <- NeS_Ne_correlation_test_cut$estimate

# Test the correlation between Ne calculated with Speed-Ne and Shannon diversity index corrected for population size
NeS_diversity_correlation_test_cut <- cor.test(as.numeric(all_results_with_Ne$NeS), as.numeric(all_results_with_Ne$allelic_diversity), method = correlation_method)
# save p-value
NeS_diversity_correlation_p_value_cut <- NeS_diversity_correlation_test_cut$p.value
# save the estimated correlation coefficient
NeS_diversity_correlation_coefficient_cut <- NeS_diversity_correlation_test_cut$estimate

# Test the correlation between Ne calculated with Speed-Ne and AMOVA
NeS_Amova_correlation_test_cut <- cor.test(as.numeric(all_results_with_Ne$NeS), as.numeric(all_results_with_Ne$Difference), method = correlation_method)
# save p-value
NeS_Amova_correlation_p_value_cut <- NeS_Amova_correlation_test_cut$p.value
# save the estimated correlation coefficient
NeS_Amova_correlation_coefficient_cut <- NeS_Amova_correlation_test_cut$estimate

# Test the correlation between Ne calculated with Speed-Ne and AMOVA
NeS_Overlaps_correlation_test_cut <- cor.test(as.numeric(all_results_with_Ne$NeS), as.numeric(all_results_with_Ne$observed_overlap), method = correlation_method)
# save p-value
NeS_Overlaps_correlation_p_value_cut <- NeS_Overlaps_correlation_test_cut$p.value
# save the estimated correlation coefficient
NeS_Overlaps_correlation_coefficient_cut <- NeS_Overlaps_correlation_test_cut$estimate



#### Testing correlations without Ne: 
# Test the correlation between allele diversity and shared alleles
diversity_Overlaps_correlation_test <- cor.test(as.numeric(diversity_results$X.N..N...1.....lambda[1:(nrow(diversity_results)-1)]), as.numeric(Overlaps_results$observed_overlap), method = correlation_method)
# save p-value
diversity_Overlaps_correlation_p_value <- diversity_Overlaps_correlation_test$p.value
# save the estimated correlation coefficient
diversity_Overlaps_correlation_coefficient <- diversity_Overlaps_correlation_test$estimate

# Test the correlation between allele diversity and rare alleles
diversity_Amova_correlation_test <- cor.test(as.numeric(diversity_results$X.N..N...1.....lambda[1:(nrow(diversity_results)-1)]), as.numeric(amova_results$Difference), method = correlation_method)
# save p-value
diversity_Amova_correlation_p_value <- diversity_Amova_correlation_test$p.value
# save the estimated correlation coefficient
diversity_Amova_correlation_coefficient <- diversity_Amova_correlation_test$estimate

# Test the correlation between rare alleles and shared alleles
# Here I find it difficult to evaluate what is the independent variable and what is the dependent variable. 
# You could probably look at it the other way around too.
Amova_Overlaps_correlation_test <- cor.test(as.numeric(amova_results$Difference), as.numeric(Overlaps_results$observed_overlap), method = correlation_method) 
# save p-value
Amova_Overlaps_correlation_p_value <- Amova_Overlaps_correlation_test$p.value
# save the estimated correlation coefficient
Amova_Overlaps_correlation_coefficient <- Amova_Overlaps_correlation_test$estimate


# Thank you Lukas Knob for improving my Script on testing correlations!

############################## save correlations #####################################################


# Create a table with correlation test results
correlation_results_with_Ne <- data.frame(
  tested_methods = c("Ne_diversity", "Ne_Amova", "Ne_Overlaps", "diversity_Overlaps", "diversity_Amova", "Amova_Overlaps", "NeS_Ne", "NeS_diversity", "NeS_Amova", "NeS_Overlaps"),
  p_value = c(Ne_diversity_correlation_p_value, Ne_Amova_correlation_p_value, Ne_Overlaps_correlation_p_value, diversity_Overlaps_correlation_p_value_cut, diversity_Amova_correlation_p_value_cut, Amova_Overlaps_correlation_p_value_cut, NeS_Ne_correlation_p_value_cut, NeS_diversity_correlation_p_value_cut, NeS_Amova_correlation_p_value_cut, NeS_Overlaps_correlation_p_value_cut ),  
  correlation_coefficient = c(Ne_diversity_correlation_coefficient, Ne_Amova_correlation_coefficient, Ne_Overlaps_correlation_coefficient, diversity_Overlaps_correlation_coefficient_cut, diversity_Amova_correlation_coefficient_cut, Amova_Overlaps_correlation_coefficient_cut,  NeS_Ne_correlation_coefficient_cut, NeS_diversity_correlation_coefficient_cut, NeS_Amova_correlation_coefficient_cut, NeS_Overlaps_correlation_coefficient_cut )
)

correlation_results_without_Ne <- data.frame(
  tested_methods = c("diversity_Overlaps", "diversity_Amova", "Amova_Overlaps"),
  p_value = c(diversity_Overlaps_correlation_p_value, diversity_Amova_correlation_p_value, Amova_Overlaps_correlation_p_value),  
  correlation_coefficient = c(diversity_Overlaps_correlation_coefficient, diversity_Amova_correlation_coefficient, Amova_Overlaps_correlation_coefficient)
)

# Create a folder for Correlation results
# Specify folder name
output_folder_path <- paste("Results", describe_results_variable, "/Compare_all_methods", sep = "")
# Create folder if it does not already exist
if (!file.exists(output_folder_path)) {
  dir.create(output_folder_path, recursive = TRUE)
}

# Save as .csv -files
write.csv(correlation_results_with_Ne, file = paste("Results", describe_results_variable, "/Compare_all_methods/correlation_results_with_Ne.csv", sep = ""), row.names = FALSE)
write.csv(correlation_results_without_Ne, file = paste("Results", describe_results_variable, "/Compare_all_methods/correlation_results_without_Ne.csv", sep = ""), row.names = FALSE)



############################### Create correlation plots ##########################

# Create matrices for correlation results with Ne
# Create a correlation matrix
correlation_matrix_with_Ne <- data.frame(
  AMOVA = c(1, correlation_results_with_Ne[5,3], correlation_results_with_Ne[6,3], correlation_results_with_Ne[2,3], correlation_results_with_Ne[9,3]),
  rare = c(correlation_results_with_Ne[5,3], 1, correlation_results_with_Ne[4,3], correlation_results_with_Ne[1,3], correlation_results_with_Ne[8,3]),
  Overlaps = c(correlation_results_with_Ne[6,3], correlation_results_with_Ne[4,3], 1, correlation_results_with_Ne[3,3], correlation_results_with_Ne[10,3]), 
  Ne = c(correlation_results_with_Ne[2,3], correlation_results_with_Ne[1,3], correlation_results_with_Ne[3,3], 1, correlation_results_with_Ne[7,3]), 
  NeS = c(correlation_results_with_Ne[9,3], correlation_results_with_Ne[8,3], correlation_results_with_Ne[10,3], correlation_results_with_Ne[7,3], 1)
)
rownames(correlation_matrix_with_Ne) <- c("AMOVA", "rare", "Overlaps", "Ne", "NeS")
correlation_matrix_with_Ne <- as.matrix(correlation_matrix_with_Ne)
# Create a p-value matrix
p_matrix_with_Ne <- data.frame(
  AMOVA = c(1, correlation_results_with_Ne[5,2], correlation_results_with_Ne[6,2], correlation_results_with_Ne[2,2], correlation_results_with_Ne[9,2]),
  rare = c(correlation_results_with_Ne[5,2], 1, correlation_results_with_Ne[4,2], correlation_results_with_Ne[1,2], correlation_results_with_Ne[8,2]), 
  Overlaps = c(correlation_results_with_Ne[6,2], correlation_results_with_Ne[4,2], 1, correlation_results_with_Ne[3,2], correlation_results_with_Ne[10,2]),
  Ne = c(correlation_results_with_Ne[2,2], correlation_results_with_Ne[1,2], correlation_results_with_Ne[3,2], 1, correlation_results_with_Ne[7,2]),
  NeS = c(correlation_results_with_Ne[9,2], correlation_results_with_Ne[8,2], correlation_results_with_Ne[10,2], correlation_results_with_Ne[7,2], 1)
)
colnames(p_matrix_with_Ne) <- c("AMOVA", "rare", "Overlaps", "Ne", "NeS")
rownames(p_matrix_with_Ne) <- c("AMOVA", "rare", "Overlaps", "Ne", "NeS")
p_matrix_with_Ne <- as.matrix(p_matrix_with_Ne )


# Create matrices for correlation results without Ne
# Create a correlation matrix
correlation_matrix_without_Ne <- data.frame(
  AMOVA = c(1, correlation_results_without_Ne[2,3], correlation_results_without_Ne[3,3]),
  rare = c(correlation_results_without_Ne[2,3], 1, correlation_results_without_Ne[1,3]),
  Overlaps = c(correlation_results_without_Ne[3,3], correlation_results_without_Ne[1,3], 1)
  )
rownames(correlation_matrix_without_Ne) <- c("AMOVA", "rare", "Overlaps")
correlation_matrix_without_Ne <- as.matrix(correlation_matrix_without_Ne)
# Create a p-value matrix
p_matrix_without_Ne <- data.frame(
  AMOVA = c(1, correlation_results_without_Ne[2,2], correlation_results_without_Ne[3,2]),
  rare = c(correlation_results_without_Ne[2,2], 1, correlation_results_without_Ne[1,2]),
  Overlaps = c(correlation_results_without_Ne[3,2], correlation_results_without_Ne[1,2], 1)
)
rownames(p_matrix_without_Ne) <- c("AMOVA", "rare", "Overlaps")
p_matrix_without_Ne <- as.matrix(p_matrix_without_Ne)



# requirement to use plot function:
# for being able to execute adjust_points_corrplot() you have to load the corrplot() environment, otherwise many functions are missing to successfully run the function.
environment(adjust_points_corrplot) <- asNamespace('corrplot')
assignInNamespace("corrplot", adjust_points_corrplot, ns = "corrplot")



# Create a plot for correlation results excluding the effective population size:
# prepare a new plot
plot.new()
x_position_point <- 0.12 # good looking without Ne
# Display results
adjust_points_corrplot(correlation_matrix_without_Ne, method= "square", type="upper", order="hclust",
                       addCoef.col = "black", # Add coefficient of correlation
                       tl.col="black", tl.srt=45,  #color and rotation
                       p.mat = p_matrix_without_Ne, 
                       #sig.level = 0.05, 
                       #insig = "blank", 
                       sig.level = c(0.001, 0.01, 0.05), pch.cex = 1.5,
                       insig = 'label_sig', pch.col = "black",
                       
                       col = COL2('RdBu', 10),
                       diag=FALSE )$corrPos -> p1
text(p1$x, p1$y, round(p1$corr, 2))
cat( " *** = p-value is below 0.001, ** = p-value is below 0.01, * = p-value is below 0.05, the numbers show the correlation coefficient. " )

dev.copy(png, file.path(paste("Results", describe_results_variable, "/Compare_all_methods", sep = ""), "correlations_without_Ne.png"))
dev.off()

# Create a plot for correlation results including the effective population size:
# prepare a new plot
plot.new()
x_position_point <- 0.19 # good looking with Ne
# Display results
adjust_points_corrplot(correlation_matrix_with_Ne, method= "square", type="upper", order="hclust",
                       addCoef.col = "black", # Add coefficient of correlation
                       tl.col="black", tl.srt=45,  #color and rotation
                       p.mat = p_matrix_with_Ne, 
                       #sig.level = 0.05, 
                       #insig = "blank", 
                       sig.level = c(0.001, 0.01, 0.05), pch.cex = 1.5,
                       insig = 'label_sig', pch.col = "black",
                       
                       col = COL2('RdBu', 10),
                       diag=FALSE )$corrPos -> p1
text(p1$x, p1$y, round(p1$corr, 2))
cat( " *** = p-value is below 0.001, ** = p-value is below 0.01, * = p-value is below 0.05, the numbers show the correlation coefficient. " )

dev.copy(png, file.path(paste("Results", describe_results_variable, "/Compare_all_methods", sep = ""), "correlations_with_Ne.png"))
dev.off()


########################## tidy up #####################
# 
# # save all objects in your environment
# all_objects <- ls()
# 
# # Remove all objects except the most necessary ones you want to keep
# objects_to_remove <- setdiff(all_objects, all_objects_at_beginning_in_environment)
# rm(list = objects_to_remove)
# 

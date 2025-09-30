

#################################################################################################
######################### Load all results into R ###############################################
#################################################################################################

all_names <- c("Ambystoma_bishopi.xlsx","Avicennia_marina.xlsx","Cameraria_ohridella.xlsx", "Carcinus_meanas.xlsx", "Cercidiphyllum_japonicum.xlsx", "Cymodocea_nodosa.xlsx", "Cystoseira_amentaceae.xlsx", "Euphydryas_aurina.xlsx", "Mytilus_galloprovincialis.xlsx", "Pagophila_eburnea.xlsx", "Panthera_leo.xlsx", "Posidonia_oceanica.xlsx", "Pyura_chilensis.xlsx", "Syncerus_caffer.xlsx", "Varroa_jacobsoni.xlsx", "Abies_alba.xlsx", "Argiope_bruennichi.xlsx" ,"Atriophallophorus_winterbourni.xlsx",  "Dracocephalum_ruyschiana.xlsx", "Entosphenus_tridentatus.xlsx", "Frangula_alnus.xlsx", "Gadus_morhua.xlsx", "Melitaea_cinxia.xlsx", "Nilparvata_lugens.xlsx", "Oncorhynchus_mykiss.xlsx", "Oncorhynchus_tshawytscha.xlsx", "Physeter_macrocephalus.xlsx", "Pinus_halepensis.xlsx", "Salmo_trutta.xlsx", "Tectona_grandis.xlsx")
all_names_with_NeEstimator_results <- c("Ambystoma_bishopi.xlsx","Avicennia_marina.xlsx","Cameraria_ohridella.xlsx", "Carcinus_meanas.xlsx", "Cercidiphyllum_japonicum.xlsx", "Cymodocea_nodosa.xlsx", "Cystoseira_amentaceae.xlsx", "Euphydryas_aurina.xlsx", "Mytilus_galloprovincialis.xlsx", "Pagophila_eburnea.xlsx", "Panthera_leo.xlsx", "Posidonia_oceanica.xlsx", "Pyura_chilensis.xlsx", "Syncerus_caffer.xlsx", "Varroa_jacobsoni.xlsx", "Abies_alba.xlsx", "Atriophallophorus_winterbourni.xlsx",  "Dracocephalum_ruyschiana.xlsx", "Entosphenus_tridentatus.xlsx", "Frangula_alnus.xlsx", "Gadus_morhua.xlsx", "Melitaea_cinxia.xlsx", "Nilparvata_lugens.xlsx", "Oncorhynchus_mykiss.xlsx", "Oncorhynchus_tshawytscha.xlsx", "Physeter_macrocephalus.xlsx", "Pinus_halepensis.xlsx", "Salmo_trutta.xlsx", "Tectona_grandis.xlsx")
all_names_with_SpeedNe_results <- c("Ambystoma_bishopi.xlsx","Avicennia_marina.xlsx","Cameraria_ohridella.xlsx", "Carcinus_meanas.xlsx", "Cercidiphyllum_japonicum.xlsx", "Cymodocea_nodosa.xlsx", "Cystoseira_amentaceae.xlsx", "Mytilus_galloprovincialis.xlsx", "Pagophila_eburnea.xlsx", "Panthera_leo.xlsx", "Posidonia_oceanica.xlsx", "Pyura_chilensis.xlsx", "Syncerus_caffer.xlsx", "Varroa_jacobsoni.xlsx", "Abies_alba.xlsx", "Atriophallophorus_winterbourni.xlsx",  "Dracocephalum_ruyschiana.xlsx", "Entosphenus_tridentatus.xlsx", "Gadus_morhua.xlsx", "Melitaea_cinxia.xlsx", "Oncorhynchus_mykiss.xlsx", "Physeter_macrocephalus.xlsx")

######################## Load data without Ne calculations #####################################
################################################################################################
lambda <- c()
AMOVA <- c()
EcoSim <- c()
AvTD <- c()
Dest <- c()
#plot(AMOVA,EcoSim)
for (name in all_names) {
  file_path <- name 
  # This code helps to structure the results, especially if you have several data sets.
  describe_results_variable <- paste("_", tools::file_path_sans_ext(file_path), sep = "")
  # Load lambda results
  diversity_results <- read.csv(paste("Results", describe_results_variable, "/diversity/diversity_results.csv", sep = "")) 
  diversity_results <- subset(diversity_results, Pop != "Total")
  lambda <- c(lambda, diversity_results$X.N..N...1.....lambda)  
  # Load AMOVA results
  amova_results <- read.csv(paste("Results", describe_results_variable, "/AMOVA/amova_results.csv", sep = "")) 
  AMOVA <- c(AMOVA, amova_results$Difference) 
  # Load EcoSim results
  Overlaps_results <- read.csv(paste("Results", describe_results_variable, "/Overlaps/Overlaps_results.csv", sep = ""))
  EcoSim <- c(EcoSim, Overlaps_results$observed_overlap)
  # Load AvTD results
  AvTD_results <- read.csv(paste("Results", describe_results_variable, "/AvTD/AvTD_results.csv", sep = ""))
  AvTD <- c(AvTD, AvTD_results$expected_Dplus)
  # Load Dest results
  Dest_results <- read.csv(paste("Results", describe_results_variable, "/Dest/Dest_results.csv", sep = ""))
  Dest <- c(Dest, Dest_results$Dest)
}

############ Load data with Ne Estimator ####################################
#############################################################################
all_results_with_Ne_final <- data.frame()
for (name in all_names_with_NeEstimator_results) {
  file_path <- name 
  # This code helps to structure the results, especially if you have several data sets.
  describe_results_variable <- paste("_", tools::file_path_sans_ext(file_path), sep = "")
  diversity_results <- read.csv(paste("Results", describe_results_variable, "/diversity/diversity_results.csv", sep = "")) 
  #diversity_results <- subset(diversity_results, Pop != "Total")
  # Load the Overlaps table into R
  Overlaps_results <- read.csv(paste("Results", describe_results_variable, "/Overlaps/Overlaps_results.csv", sep = ""))
  # Load the Amova table into R
  amova_results <- read.csv(paste("Results", describe_results_variable, "/AMOVA/amova_results.csv", sep = "", quote = ""))
  # Load the Ne table into R
  Ne_results <- read.csv(paste("Results", describe_results_variable, "/NeEstimator/Ne_results.csv", sep = "", quote = ""))   # falls diese Tabelle nicht existiert soll Ne aus den Kalkulationen weggelassen werden
  # Load AvTD
  AvTD_results <- read.csv(paste("Results", describe_results_variable, "/AvTD/AvTD_results.csv", sep = "", quote = ""))
  # Load Dest 
  Dest_results <- read.csv(paste("Results", describe_results_variable, "/Dest/Dest_results.csv", sep = ""))
  # Prepare data for correlation calculations with Ne:
  # For analyses including Ne rows of the other methods most likely must be removed, because it might have been impossible to calculate Ne for all sites.
  # remove information about confidence interval to keep it simple
  all_results_with_Ne <- Ne_results[, -c(3, 4, 5)]
  # create new columns for every calculation method
  all_results_with_Ne$allelic_diversity <- NA
  all_results_with_Ne$observed_overlap <- NA
  all_results_with_Ne$Difference <- NA
  all_results_with_Ne$AvTD <- NA
  all_results_with_Ne$Dest <- NA
  all_results_with_Ne$data <- name
  # Append Ne table, add results of other calculations. 
  # Thereby unnecessary rows are removed automatically from the data set. 
  for(site in all_results_with_Ne$site) {
    all_results_with_Ne$allelic_diversity[which(all_results_with_Ne$site == site)] <- diversity_results$X.N..N...1.....lambda[which(diversity_results$Pop == site)]
    all_results_with_Ne$observed_overlap[which(all_results_with_Ne$site == site)] <- Overlaps_results$observed_overlap[which(Overlaps_results$site == site)]
    all_results_with_Ne$Difference[which(all_results_with_Ne$site == site)] <- amova_results$Difference [which(amova_results$site == site)]
    all_results_with_Ne$AvTD[which(all_results_with_Ne$site == site)] <- AvTD_results$expected_Dplus[which(AvTD_results$area == site)]
    all_results_with_Ne$Dest[which(all_results_with_Ne$site == site)] <- Dest_results$Dest[which(Dest_results$population == site)]
  }
  all_results_with_Ne_final <- rbind(all_results_with_Ne_final, all_results_with_Ne)
}







############################# check for correlations ################################################
#####################################################################################################
correlation_method <- "kendall" # can be "pearson", "kendall" or "spearm"
# To know which correlation method is best to use the distribution of the data should be known. 
# Tests if data is normally distributed:
# test_normal_distribution <- shapiro.test(as.numeric( <enter data> ))
# normally distributed if test_normal_distribution$p.value > 0.05
# If the data sets are normally distributed, a Pearson correlation would be best,
# Since lambda is not normally distributed I am going to apply a Kendall correlation test on all possible combinations of methods.
# A Kendall correlation test is more robust and slightly more efficient than Spearman's rank correlation. 
# This is why I prefer Kendall.
# The independent variable is named at the beginning and the dependent variable at the second place.


########################## Test correlations with Dest ############################################
Ne_Dest_correlation_test <- cor.test(as.numeric(all_results_with_Ne_final$Dest), as.numeric(all_results_with_Ne_final$Ne), method = correlation_method)
# save p-value
Ne_Dest_correlation_p_value <- Ne_Dest_correlation_test$p.value
# save the estimated correlation coefficient
Ne_Dest_correlation_coefficient <- Ne_Dest_correlation_test$estimate

Dest_diversity_correlation_test <- cor.test(as.numeric(Dest), as.numeric(lambda), method = correlation_method)
Dest_diversity_correlation_p_value <- Dest_diversity_correlation_test$p.value
Dest_diversity_correlation_coefficient <- Dest_diversity_correlation_test$estimate

Dest_Amova_correlation_test <- cor.test(as.numeric(subset(Dest, AMOVA <= 40)),
                                       as.numeric(subset(AMOVA, AMOVA <= 40)),
                                       method = correlation_method)
Dest_Amova_correlation_p_value <- Dest_Amova_correlation_test$p.value
Dest_Amova_correlation_coefficient <- Dest_Amova_correlation_test$estimate

Dest_Overlaps_correlation_test <- cor.test(as.numeric(Dest), as.numeric(EcoSim), method = correlation_method)
Dest_Overlaps_correlation_p_value <- Dest_Overlaps_correlation_test$p.value
Dest_Overlaps_correlation_coefficient <- Dest_Overlaps_correlation_test$estimate

Dest_AvTD_correlation_test <- cor.test(as.numeric(Dest), as.numeric(AvTD), method = correlation_method)
Dest_AvTD_correlation_p_value <- Dest_AvTD_correlation_test$p.value
Dest_AvTD_correlation_coefficient <- Dest_AvTD_correlation_test$estimate


########################## Test correlations with Ne Estimator ############################################
# Test the correlation between Ne Estimator results and lambda corrected for sample size
Ne_diversity_correlation_test <- cor.test(as.numeric(all_results_with_Ne_final$Ne), as.numeric(all_results_with_Ne_final$allelic_diversity), method = correlation_method)
# save p-value
Ne_diversity_correlation_p_value <- Ne_diversity_correlation_test$p.value
# save the estimated correlation coefficient
Ne_diversity_correlation_coefficient <- Ne_diversity_correlation_test$estimate

# Test the correlation between Ne Estimator results and AMOVA
Ne_Amova_correlation_test <- cor.test(as.numeric(subset(all_results_with_Ne_final, all_results_with_Ne_final$Difference <= 40)$Ne),
                                      as.numeric(subset(all_results_with_Ne_final, all_results_with_Ne_final$Difference <= 40)$Difference),
                                      method = correlation_method) # 
# save p-value
Ne_Amova_correlation_p_value <- Ne_Amova_correlation_test$p.value
# save the estimated correlation coefficient
Ne_Amova_correlation_coefficient <- Ne_Amova_correlation_test$estimate

# Test the correlation between Ne Estimator results and observed overlap
Ne_Overlaps_correlation_test <- cor.test(as.numeric(all_results_with_Ne_final$Ne),as.numeric(all_results_with_Ne_final$observed_overlap), method = correlation_method)
# save p-value
Ne_Overlaps_correlation_p_value <- Ne_Overlaps_correlation_test$p.value
# save the estimated correlation coefficient
Ne_Overlaps_correlation_coefficient <- Ne_Overlaps_correlation_test$estimate

# Test the correlation between Ne Estimator results and AvTD 
Ne_AvTD_correlation_test <- cor.test(as.numeric(all_results_with_Ne_final$Ne),as.numeric(all_results_with_Ne_final$AvTD), method = correlation_method)
# save p-value
Ne_AvTD_correlation_p_value <- Ne_AvTD_correlation_test$p.value
# save the estimated correlation coefficient
Ne_AvTD_correlation_coefficient <- Ne_AvTD_correlation_test$estimate


########################### Test correlations without Ne calculations ############################
# Test the correlation between allele diversity and shared alleles
diversity_Overlaps_correlation_test <- cor.test(as.numeric(lambda), as.numeric(EcoSim), method = correlation_method)
# save p-value
diversity_Overlaps_correlation_p_value <- diversity_Overlaps_correlation_test$p.value
# save the estimated correlation coefficient
diversity_Overlaps_correlation_coefficient <- diversity_Overlaps_correlation_test$estimate

# Test the correlation between allele diversity and rare alleles
diversity_Amova_correlation_test <- cor.test(as.numeric(subset(lambda, AMOVA <= 40)), 
                                             as.numeric(subset(AMOVA, AMOVA <= 40)),
                                             method = correlation_method)
# save p-value
diversity_Amova_correlation_p_value <- diversity_Amova_correlation_test$p.value
# save the estimated correlation coefficient
diversity_Amova_correlation_coefficient <- diversity_Amova_correlation_test$estimate

# Test the correlation between rare alleles and shared alleles
# Here I find it difficult to evaluate what is the independent variable and what is the dependent variable. 
# You could probably look at it the other way around too.
Amova_Overlaps_correlation_test <- cor.test(as.numeric(subset(AMOVA, AMOVA <= 40)),
                                            as.numeric(subset(EcoSim, AMOVA <= 40)),
                                            method = correlation_method) 
# save p-value
Amova_Overlaps_correlation_p_value <- Amova_Overlaps_correlation_test$p.value
# save the estimated correlation coefficient
Amova_Overlaps_correlation_coefficient <- Amova_Overlaps_correlation_test$estimate

# Test the correlation between AvTD and AMOVA looking only at sites with Ne Estimator calculated
AvTD_AMOVA_correlation_test <- cor.test(as.numeric(subset(AvTD, AMOVA <= 40)),
                                        as.numeric(subset(AMOVA, AMOVA <= 40)),
                                        method = correlation_method)
# save p-value
AvTD_AMOVA_correlation_p_value <- AvTD_AMOVA_correlation_test$p.value
# save the estimated correlation coefficient
AvTD_AMOVA_correlation_coefficient <- AvTD_AMOVA_correlation_test$estimate

# Test the correlation between genetic diversity and AvTD looking only at sites with Ne Estimator calculated
diversity_AvTD_correlation_test <- cor.test(as.numeric(lambda), as.numeric(AvTD), method = correlation_method)
# save p-value
diversity_AvTD_correlation_p_value <- diversity_AvTD_correlation_test$p.value
# save the estimated correlation coefficient
diversity_AvTD_correlation_coefficient <- diversity_AvTD_correlation_test$estimate

# Test the correlation between AvTD and observed overlap looking only at sites with Ne Estimator calculated
AvTD_overlap_correlation_test <- cor.test(as.numeric(AvTD), as.numeric(EcoSim) , method = correlation_method)
# save p-value
AvTD_overlap_correlation_p_value <- AvTD_overlap_correlation_test$p.value
# save the estimated correlation coefficient
AvTD_overlap_correlation_coefficient <- AvTD_overlap_correlation_test$estimate
# Thank you Lukas Knob for improving my Script on correlations!


################# Create a correlations plot ################################################################
# Create a table with correlation test results
correlation_results <- data.frame(
  tested_methods = c("Ne_diversity", "Ne_Amova", "Ne_Overlaps", "diversity_Overlaps", "diversity_Amova", "Amova_Overlaps", 
                     "Ne_Dest", "Dest_diversity", "Dest_Amova", "Dest_Overlaps", "Dest_AvTD",
                     "AvTD_AMOVA", "diversity_AvTD", "AvTD_Overlaps", "Ne_AvTD"),
  p_value = c(Ne_diversity_correlation_p_value, Ne_Amova_correlation_p_value, Ne_Overlaps_correlation_p_value, 
              diversity_Overlaps_correlation_p_value, diversity_Amova_correlation_p_value, Amova_Overlaps_correlation_p_value, 
              Ne_Dest_correlation_p_value, Dest_diversity_correlation_p_value, Dest_Amova_correlation_p_value, 
              Dest_Overlaps_correlation_p_value, Dest_AvTD_correlation_p_value , 
              AvTD_AMOVA_correlation_p_value, diversity_AvTD_correlation_p_value, AvTD_overlap_correlation_p_value, Ne_AvTD_correlation_p_value),  
  correlation_coefficient = c(Ne_diversity_correlation_coefficient, Ne_Amova_correlation_coefficient, Ne_Overlaps_correlation_coefficient, 
                              diversity_Overlaps_correlation_coefficient, diversity_Amova_correlation_coefficient, Amova_Overlaps_correlation_coefficient, 
                              Ne_Dest_correlation_coefficient, Dest_diversity_correlation_coefficient, Dest_Amova_correlation_coefficient, 
                              Dest_Overlaps_correlation_coefficient, Dest_AvTD_correlation_coefficient, Dest_Amova_correlation_coefficient, 
                              diversity_AvTD_correlation_coefficient, AvTD_overlap_correlation_coefficient, Ne_AvTD_correlation_coefficient)
)

# Create matrices for correlation results with Ne
# Create a correlation matrix
correlation_matrix <- data.frame(
  AMOVA = c(1, correlation_results[5,3], correlation_results[6,3], correlation_results[2,3], correlation_results[9,3], correlation_results[12,3]),
  lambda = c(correlation_results[5,3], 1, correlation_results[4,3], correlation_results[1,3], correlation_results[8,3], correlation_results[13,3]),
  Overlaps = c(correlation_results[6,3], correlation_results[4,3], 1, correlation_results[3,3], correlation_results[10,3], correlation_results[14,3]), 
  Ne = c(correlation_results[2,3], correlation_results[1,3], correlation_results[3,3], 1, correlation_results[7,3], correlation_results[15,3]), 
  Dest = c(correlation_results[9,3], correlation_results[8,3], correlation_results[10,3], correlation_results[7,3], 1, correlation_results[11,3]),
  AvTD = c(correlation_results[12,3], correlation_results[13,3], correlation_results[14,3], correlation_results[15,3], correlation_results[11,3], 1)
)
colnames(correlation_matrix) <- c("AMOVA", ":λ[cor]", "Allelic Overlap", ":N[e]", ":D[est]", ":Δ^'+'")
rownames(correlation_matrix) <- c("AMOVA", ":λ[cor]", "Allelic Overlap", ":N[e]", ":D[est]", ":Δ^'+'")
correlation_matrix <- as.matrix(correlation_matrix)

# Create a p-value matrix
p_matrix <- data.frame(
  AMOVA = c(1, correlation_results[5,2], correlation_results[6,2], correlation_results[2,2], correlation_results[9,2], correlation_results[12,2]),
  lambda = c(correlation_results[5,2], 1, correlation_results[4,2], correlation_results[1,2], correlation_results[8,2], correlation_results[13,2]),
  Overlaps = c(correlation_results[6,2], correlation_results[4,2], 1, correlation_results[3,2], correlation_results[10,2], correlation_results[14,2]), 
  Ne = c(correlation_results[2,2], correlation_results[1,2], correlation_results[3,2], 1, correlation_results[7,2], correlation_results[15,2]), 
  NeS = c(correlation_results[9,2], correlation_results[8,2], correlation_results[10,2], correlation_results[7,2], 1, correlation_results[11,2]),
  AvTD = c(correlation_results[12,2], correlation_results[13,2], correlation_results[14,2], correlation_results[15,2], correlation_results[11,2], 1)
)
colnames(p_matrix) <- c("AMOVA", ":λ[cor]", "Allelic Overlap", ":N[e]", ":D[est]", ":Δ^'+'")
rownames(p_matrix) <- c("AMOVA", ":λ[cor]", "Allelic Overlap", ":N[e]", ":D[est]", ":Δ^'+'")
p_matrix <- as.matrix(p_matrix)

# requirement to use plot function:
# for being able to execute adjust_points_corrplot() you have to load the corrplot() environment, otherwise many functions are missing to successfully run the function.
environment(adjust_points_corrplot) <- asNamespace('corrplot')
assignInNamespace("corrplot", adjust_points_corrplot, ns = "corrplot")

dev.off()
# prepare a new plot
plot.new()
x_position_point <- 0.19 
y_position_point <- 0.2
# Display results
adjust_points_corrplot(correlation_matrix, method= "square", type="upper", order="hclust",
                       addCoef.col = "black", # Add coefficient of correlation
                       tl.col="black", tl.srt=45,  #color and rotation
                       p.mat = p_matrix, 
                       sig.level = c(0.001, 0.01, 0.05), pch.cex = 1.5,
                       insig = 'label_sig', pch.col = "black",
                       col = COL2('RdBu', 10),
                       diag=FALSE )$corrPos -> p1
                       text(p1$x, p1$y, round(p1$corr, 2))
cat( " *** = p-value is below 0.001, ** = p-value is below 0.01, * = p-value is below 0.05, the numbers show the correlation coefficient. " )
# save results
dev.copy(png, file.path("correlations.png"))
dev.off()




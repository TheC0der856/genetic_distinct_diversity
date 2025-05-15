library(tidyverse)
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
  AvTD_results <- head(AvTD_results, -1) # delete last row
  AvTD <- c(AvTD, AvTD_results$Delta..1)
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
  # Prepare data for correlation calculations with Ne:
  # For analyses including Ne rows of the other methods most likely must be removed, because it might have been impossible to calculate Ne for all sites.
  # remove information about confidence interval to keep it simple
  all_results_with_Ne <- Ne_results[, -c(3, 4, 5)]
  # create new columns for every calculation method
  all_results_with_Ne$allelic_diversity <- NA
  all_results_with_Ne$observed_overlap <- NA
  all_results_with_Ne$Difference <- NA
  all_results_with_Ne$AvTD <- NA
  all_results_with_Ne$data <- name
  # Append Ne table, add results of other calculations. 
  # Thereby unnecessary rows are removed automatically from the data set. 
  for(site in all_results_with_Ne$site) {
    all_results_with_Ne$allelic_diversity[which(all_results_with_Ne$site == site)] <- diversity_results$X.N..N...1.....lambda[which(diversity_results$Pop == site)]
    all_results_with_Ne$observed_overlap[which(all_results_with_Ne$site == site)] <- Overlaps_results$observed_overlap[which(Overlaps_results$site == site)]
    all_results_with_Ne$Difference[which(all_results_with_Ne$site == site)] <- amova_results$Difference [which(amova_results$site == site)]
    all_results_with_Ne$AvTD[which(all_results_with_Ne$site == site)] <- AvTD_results$Delta..1[which(AvTD_results$X == site)]
  }
  all_results_with_Ne_final <- rbind(all_results_with_Ne_final, all_results_with_Ne)
}



par(mfrow = c(1, 2))

# Boxplot für Ne
boxplot(all_results_with_Ne_final$Ne,
        main = "Ne",
        col = "skyblue",
        outline = TRUE)

# Boxplot für Delta+
boxplot(all_results_with_Ne_final$AvTD,
        main = "Delta+",
        col = "skyblue",
        outline = TRUE)

# Layout zurücksetzen (optional)
par(mfrow = c(1, 1))

#irrelevant if this is about why ne might be an alternative
# boxplot(all_results_with_Ne_final$Difference,
#         main = "AMOVA",
#         col = "skyblue",
#         outline = TRUE)

# noone cares, unsuitable because all values one:
# boxplot(all_results_with_Ne_final$allelic_diversity,
#         main = "lambda",
#         col = "skyblue",
#         outline = TRUE)
# noone cares, unsuitable because useless:
# boxplot(all_results_with_Ne_final$observed_overlap,
#         main = "overlap",
#         ylab = "Ne",
#         col = "skyblue",
#         outline = TRUE)


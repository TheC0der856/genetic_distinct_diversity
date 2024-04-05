#How many datasets have significant AvTD and AMOVA results?

# which data sets should be looped?
describe_results_variables <- c("_Ambystoma_bishopi", 
                                "_Avicennia_marina", 
                                "_Cameraria_ohridella", 
                                "_Carcinus_meanas", 
                                "_Cercidiphyllum_japonicum", 
                                "_Cymodocea_nodosa", 
                                "_Cystoseira_amentaceae",
                                "_Euphydryas_aurina" ,
                                "_Mytilus_galloprovincialis",
                                "_Pagophila_eburnea", 
                                "_Panthera_leo", 
                                "_Posidonia_oceanica",
                                "_Pyura_chilensis",
                                "_Syncerus_caffer",
                                "_Varroa_jacobsoni", 
                                "_Abies_alba", 
                                "_Argiope_bruennichi", 
                                "_Atriophallophorus_winterbourni", 
                                "_Dracocephalum_ruyschiana",
                                "_Entosphenus_tridentatus",
                                "_Frangula_alnus",
                                "_Gadus_morhua", 
                                "_Melitaea_cinxia",
                                "_Nilparvata_lugens",
                                "_Oncorhynchus_mykiss",
                                "_Oncorhynchus_tshawytscha",
                                "_Physeter_macrocephalus",
                                "_Pinus_halepensis",
                                "_Salmo_trutta",
                                "_Tectona_grandis")

#### load results into R #########
all_AMOVA <- data.frame()
all_AvTD <- data.frame()
# Fill data frames with AMOVA results of all data sets
for (describe_results_variable in describe_results_variables) {
  AMOVA <- read.csv(paste("Results", describe_results_variable, "/AMOVA/amova_results.csv", sep = "", quote = ""))  
  all_AMOVA <- rbind(all_AMOVA, AMOVA)
  
  AvTD_results <- read.csv(paste("Results", describe_results_variable, "/AvTD/AvTD_results.csv", sep = ""))
  AvTD_results <- head(AvTD_results, -1) # delete last row
  all_AvTD <- rbind(all_AvTD, AvTD_results)
}  

# determine significance levels
significance_threshold1 <- 0.05 # *
significance_threshold2 <-0.01 # **
significance_threshold3 <-0.001 # ***

# Looking at AMOVA
# calculate the percentage of all populations which fit into different significance levels
significant_AMOVA_results <- sum(all_AMOVA$Significance < significance_threshold1)
percentage_of_significant1_AMOVA_results <- significant_AMOVA_results/nrow(all_AMOVA) *100
significant_AMOVA_results <- sum(all_AMOVA$Significance < significance_threshold2)
percentage_of_significant2_AMOVA_results <- significant_AMOVA_results/nrow(all_AMOVA) *100
significant_AMOVA_results <- sum(all_AMOVA$Significance < significance_threshold3)
percentage_of_significant3_AMOVA_results <- significant_AMOVA_results/nrow(all_AMOVA) *100

# Looking at AvTD
# calculate the percentage of all population fitting into different significance levels
significant_AvTD_results <- sum(all_AvTD$Pr...z.. < significance_threshold1, na.rm = TRUE)
percentage_of_significant1_AvTD_results <- significant_AvTD_results/nrow(all_AvTD) *100
significant_AvTD_results <- sum(all_AvTD$Pr...z.. < significance_threshold2, na.rm = TRUE)
percentage_of_significant2_AvTD_results <- significant_AvTD_results/nrow(all_AvTD) *100
significant_AvTD_results <- sum(all_AvTD$Pr...z.. < significance_threshold3, na.rm = TRUE)
percentage_of_significant3_AvTD_results <- significant_AvTD_results/nrow(all_AvTD) *100

# create a data frame with all results:
significances <- data.frame(
  AMOVA = c(round(percentage_of_significant1_AMOVA_results),round(percentage_of_significant2_AMOVA_results),round(percentage_of_significant3_AMOVA_results) ),
  AvTD = c(round(percentage_of_significant1_AvTD_results),round(percentage_of_significant2_AvTD_results),round(percentage_of_significant3_AvTD_results) ), 
  significance = c("*", "**", "***" )
)



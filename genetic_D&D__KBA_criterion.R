
datasets <- c("Ambystoma_bishopi",
             "Avicennia_marina",
             "Cameraria_ohridella", 
             "Carcinus_meanas", 
             "Cercidiphyllum_japonicum", 
             "Cymodocea_nodosa", 
             "Cystoseira_amentaceae", 
             "Euphydryas_aurina",
             "Mytilus_galloprovincialis", 
             "Pagophila_eburnea", 
             "Panthera_leo", 
             "Posidonia_oceanica", 
             "Pyura_chilensis", 
             "Syncerus_caffer", 
             "Varroa_jacobsoni", 
             "Abies_alba",
             "Argiope_bruennichi", 
             "Atriophallophorus_winterbourni", 
             "Dracocephalum_ruyschiana", 
             "Entosphenus_tridentatus", 
             "Frangula_alnus",
             "Gadus_morhua", 
             "Melitaea_cinxia", 
             "Oncorhynchus_mykiss", 
             "Oncorhynchus_tshawytscha", 
             "Physeter_macrocephalus", 
             "Pinus_halepensis", 
             "Salmo_trutta", 
             "Tectona_grandis")

all_thresholds_met <- data.frame()

for (dataset in datasets) {
  
  amova <- read.csv(file= paste("Results_", dataset, "/AMOVA/amova_results.csv", sep = ""))
  # create a table like in the descriptions of the KBA standard
  # calculate percentages of variance between sites 
  sum_Difference <- sum(amova$Difference)
  amova$percentage <- NA
  for (i in 1:length(amova$Difference)) {
    amova$percentage[i] <- (amova$Difference[i] / sum_Difference) * 100
  }
  # add a row to the table if the threshold was met
  amova$threshold_met <- ifelse(amova$percentage > 10, "B1, A1b", ifelse(amova$percentage > 1, "A1b", "no protection"))
  
  
  lamda <- read.csv(file= paste("Results_", dataset, "/diversity/diversity_results.csv", sep = ""))
  # create a table like in the descriptions of the KBA standard
  lamda <- lamda %>%
    slice(1:(n() - 1))
  # calculate percentages of variance between sites 
  sum <- sum(lamda$X.N..N...1.....lambda)
  lamda$percentage <- NA
  for (i in 1:length(lamda$X.N..N...1.....lambda)) {
    lamda$percentage[i] <- (lamda$X.N..N...1.....lambda[i] / sum) * 100
  }
  # add a row to the table if the threshold was met
  lamda$threshold_met <- ifelse(lamda$percentage > 10, "B1, A1b", ifelse(lamda$percentage > 1, "A1b", "no protection"))
  
  
  Eco <- read.csv(file= paste("Results_",dataset,"/Overlaps/Overlaps_results.csv", sep=""))
  # create a table like in the descriptions of the KBA standard
  # add a row to the table if the threshold was met
  Eco$threshold_met <- ifelse(Eco$observed_overlap_percentages > 10, "B1, A1b", ifelse(Eco$observed_overlap_percentages > 1, "A1b", "no protection"))
  
  
  # Combine threshold met information!
  thresholds_met <- data.frame(
    sites = amova$site,
    amova = amova$threshold_met,
    Eco = Eco$threshold_met,
    lamda = lamda$threshold_met, 
    NeS = NA, 
    NeE = NA, 
    dataset = dataset
  )
  
  
  if (file.exists(paste("Results_", dataset, "/NeEstimator/Ne_results.csv", sep = ""))) {
    NeE <- read.csv(file= paste("Results_", dataset, "/NeEstimator/Ne_results.csv", sep = ""))
    
    # # create a table like in the descriptions of the KBA standard
    # # calculate percentages of variance between sites 
    # sum <- sum(NeE$Ne)
    # NeE$percentage <- NA
    # for (i in 1:length(NeE$Ne)) {
    #   NeE$percentage[i] <- (NeE$Ne[i] / sum) * 100
    # }
    # # add a row to the table if the threshold was met
    # NeE$threshold_met <- ifelse(NeE$percentage > 10, "B1, A1b", ifelse(NeE$percentage > 1, "A1b", ""))
    # 
    # # add NeE information
    # thresholds_met$NeE[amova$site %in% NeE$site] <- NeE$threshold_met[match(amova$site[amova$site %in% NeE$site], NeE$site)]
    
    # Since in most datasets except of Syncerus caffer and Pagophila eburnea there are missing sites, 
    # calculating the percentage would create biased results: 
    # the remaining sites would be worth a higher protection status as if all sites would have been calculated.
    # Therefore the proposed thresholds of Garnier et al. 2020, Hoban et al. 2021 and Leike et al. 2020 of
    # Ne < 50 (VI = very important) and Ne < 500 (I = important) should be used. 
    
    NeE$threshold_met <- ifelse(NeE$Ne < 50, "Ne < 50",
                                ifelse(NeE$Ne < 500, "Ne < 500" , "no protection"))
    # add the thresholds to the dataframe:
    thresholds_met$NeE[amova$site %in% NeE$site] <- NeE$threshold_met[match(amova$site[amova$site %in% NeE$site], NeE$site)]
  }
  
  if(file.exists(paste("Results_",dataset ,"/NeSpeed/Ne_results.csv", sep = ""))) {
    NeS <- read.csv(file= paste("Results_",dataset ,"/NeSpeed/Ne_results.csv", sep = ""))
    
    # # create a table like in the descriptions of the KBA standard
    # # calculate percentages of variance between sites 
    # sum <- sum(NeS$Ne)
    # NeS$percentage <- NA
    # for (i in 1:length(NeS$Ne)) {
    #   NeS$percentage[i] <- (NeS$Ne[i] / sum) * 100
    # }
    # # add a row to the table if the threshold was met
    # NeS$threshold_met <- ifelse(NeS$percentage > 10, "B1, A1b", ifelse(NeS$percentage > 1, "A1b", ""))
    # 
    # Since in most datasets except of Syncerus caffer and Pagophila eburnea there are missing sites, 
    # calculating the percentage would create biased results: 
    # the remaining sites would be worth a higher protection status as if all sites would have been calculated.
    # Therefore the proposed thresholds of Garnier et al. 2020, Hoban et al. 2021 and Leike et al. 2020 of
    # Ne < 50 (VI = very important) and Ne < 500 (I = important) should be used. 
    NeS$threshold_met <- ifelse(NeS$Ne < 50,  "Ne < 50", 
                                ifelse(NeS$Ne < 500, "Ne < 500", "no protection"))
    
    # add NeS information
    thresholds_met$NeS[amova$site %in% NeS$site] <- NeS$threshold_met[match(amova$site[amova$site %in% NeS$site], NeS$site)]
  }

  all_thresholds_met <- rbind(all_thresholds_met, thresholds_met)

}

# # save as xlsx -file
# if (!requireNamespace("openxlsx", quietly = TRUE)) {
#   install.packages("openxlsx")
# }
# library(openxlsx)
# 
# write.xlsx(all_thresholds_met, file = "all_thresholds_met.xlsx", rowNames = FALSE, na = "NA")


# how often is there A1b / B1, A1b / <NA> / or empty field? 
# table(all_thresholds_met$amova) # nicht nur amova sondern auch die anderen Spalten:

#amova: Nothing: 16/ A1b: 234/ B1, A1b: 89
#Eco: A1b: 242/ B1, A1b: 97
#lamda: Nothing: 7/ A1b: 235/ B1, A1b: 97
#NeS: I: 59/ Nothing: 11/ VI: 116/ NA: 153
#NeE: I: 116 / Nothing: 37 / VI: 99 / NA: 87
par(mfrow = c(2, 3))

pie(table(all_thresholds_met$amova),
    main = "AMOVA", 
    labels = rep("", length(table(all_thresholds_met$NeE))),
    col=c("cornsilk1", "lightskyblue1", "salmon3"))

# legend("topright", legend = c("A1b > 1%" , "B1 > 10%", "no protection"), 
#        fill = c("cornsilk1", "lightskyblue1", "salmon3"))


pie(table(all_thresholds_met$Eco),
    main = "EcoSim", 
    labels = rep("", length(table(all_thresholds_met$NeE))),
    col=c("cornsilk1", "lightskyblue1"))

#legend("topright", legend = c("A1b > 1%" , "B1 > 10%"))


pie(table(all_thresholds_met$lamda),
    main = "λ", 
    labels = rep("", length(table(all_thresholds_met$NeE))),
    col=c("cornsilk1", "lightskyblue1", "salmon3"))

#legend("topright", legend = c("A1b > 1%" , "B1 > 10%"))


pie(table(all_thresholds_met$NeE, useNA = "ifany"),
    #labels = c(expression(italic(N)[italic("e")] < 50), 
    #           expression(italic(N)[italic("e")] < 500), 
    #           "no protection",
    #           "missing information"),
    main = "Ne Estimator", 
    labels = rep("", length(table(all_thresholds_met$NeE))),
    col=c( "royalblue4", "lightgoldenrod", "salmon3", "white"))


pie(table(all_thresholds_met$NeS, useNA = "ifany"),
    #labels = c(expression(italic(N)[italic("e")] < 50), 
    #           expression(italic(N)[italic("e")] < 500), 
    #           "no protection",
    #           "missing information"),
    main = "Speed Ne",
    labels = rep("", length(table(all_thresholds_met$NeE))),
    col=c("royalblue4", "lightgoldenrod", "salmon3", "white"))

plot.new()
legend("center",
       legend = c("> 1% (A1b)" ,
                  "> 10% (B1, A1b)",
                  "no protection",
                  expression(italic(N)[italic("e")] < 500),
                  expression(italic(N)[italic("e")] < 50),
                  "missing information"), 
        fill = c("cornsilk1", "lightskyblue1", "salmon3", "lightgoldenrod","royalblue4", "white"), 
        cex = 1.3)


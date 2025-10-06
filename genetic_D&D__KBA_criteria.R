

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
  # data sets can be only successfully compared if there is data from all methods and programs
  # There should not be a data set which consists of NA only, because otherwise an average to replace missing values with cannot be calculated
  if (file.exists(paste("Results_", dataset, "/NeEstimator/Ne_results.csv", sep = ""))) {
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
      # add a row to the table if the threshold was met
      Eco$threshold_met <- ifelse(Eco$observed_overlap_percentages > 10, "B1, A1b", ifelse(Eco$observed_overlap_percentages > 1, "A1b", "no protection"))
      
      AvTD <- read.csv(file= paste("Results_", dataset, "/AvTD/AvTD_results.csv", sep = ""))
      # add a row to the table if the threshold was met
      AvTD$threshold_met <- ifelse(AvTD$Dplus_percent > 10, "B1, A1b", ifelse(AvTD$Dplus_percent > 1, "A1b", "no protection"))
      
    
      NeE <- read.csv(file= paste("Results_", dataset, "/NeEstimator/Ne_results.csv", sep = ""))
      #check if there are missing information that can be added
      if (length(setdiff(amova$site, NeE$site)) != 0) {  
        # add missing data to the table
        missing_sites <- setdiff(amova$site, NeE$site) # find missing sites
        missing_data <- data.frame(site = missing_sites, 
                                    Ne = NA,
                                    lower_jackknife_confidence_interval = NA,
                                    upper_jackknife_confidence_interval = NA,
                                    lower_parametric_confidence_interval = NA,
                                    upper_parametric_confidence_interval = NA)
        NeE <- rbind(NeE, missing_data)
      }
      # create a table like in the descriptions of the KBA standard
      # test wheater there are missing values
      if (anyNA(NeE$Ne)) { # if there are NA
        # calculate the median without NA
        median_Ne <- median(NeE$Ne, na.rm = TRUE)
        # replace missing values with the mean
        NeE$Ne <- ifelse(is.na(NeE$Ne), median_Ne, NeE$Ne)
        # calculate percentages of variance between sites 
        sum <- sum(NeE$Ne)
        NeE$percentage <- NA
        for (i in 1:length(NeE$Ne)) {
          NeE$percentage[i] <- (NeE$Ne[i] / sum) * 100
        }
      } else { # There are no NA
        # calculate percentages of variance between sites 
        sum <- sum(NeE$Ne)
        NeE$percentage <- NA
        for (i in 1:length(NeE$Ne)) {
          NeE$percentage[i] <- (NeE$Ne[i] / sum) * 100
        }
      }
        
      # add a row to the table if the threshold was met
      NeE$threshold_met <- ifelse(NeE$percentage > 10, "B1, A1b",
                                  ifelse(NeE$percentage > 1, "A1b", "no protection"))
      
      Dest <- read.csv(file= paste("Results_", dataset, "/Dest/Dest_results.csv", sep = ""))
      Dest$threshold_met <- ifelse(Dest$Percent_of_Total > 10, "B1, A1b", ifelse(Dest$Percent_of_Total > 1, "A1b", "no protection"))
      
      
      # Combine threshold met information!
      thresholds_met <- data.frame(
        sites = amova$site,
        amova = amova$threshold_met,
        Eco = Eco$threshold_met,
        lamda = lamda$threshold_met, 
        AvTD = AvTD$threshold_met,
        NeE = NeE$threshold_met, 
        Dest = Dest$threshold_met,
        dataset = dataset
      )
      
  } else {next}
all_thresholds_met <- rbind(all_thresholds_met, thresholds_met)
}

# prepare data
plot_data <- all_thresholds_met %>%
  pivot_longer(
    cols = c(Eco, amova, AvTD, Dest, lamda, NeE), 
    names_to = "method", 
    values_to = "criteria"
  ) %>%
  count(method, criteria) %>%
  group_by(method) %>%
  mutate(prop = n / sum(n) * 100)

# fix order
plot_data$method <- factor(plot_data$method, 
                           levels = c("Eco", "amova", "AvTD", "Dest", "lamda", "NeE"))

# create plot
ggplot(plot_data, aes(x = method, y = prop, fill = criteria)) +
  geom_bar(stat = "identity", color = "black", size = 0.5, width = 0.8) +
  
  # colors and legend
  scale_fill_manual(
    values = c("cornsilk1", "lightskyblue1", "salmon3"),
    name = "KBA criteria", 
    labels = c("≥ 1% (A1b)", "≥ 10% (B1 & A1b)", "no protection")
  ) +
  
  # Labels
  xlab("method") + 
  ylab("proportion of fulfilled criteria [%]") +
  scale_x_discrete(labels = c(
    Eco = "Allelic Overlap",
    amova = "AMOVA",
    AvTD = expression(Delta^"+"),
    Dest = expression("D"["est"]),
    lamda = expression("λ"["cor"]),
    NeE = expression("N"["e"])
  )) +

  # Design 
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks.length = unit(0.25, "cm"),
    axis.title.x = element_text(
      size = 19, face = "bold", 
      margin = margin(t = 20, r = 0, b = 0, l = 0)
    ),
    axis.title.y = element_text(
      size = 19, face = "bold", 
      margin = margin(t = 0, r = 20, b = 0, l = 0)
    ),
    axis.text.x = element_text(
      angle = 90, 
      hjust = 0.5, 
      vjust = 0.5, 
      size = 15
    ),
    axis.text.y = element_text(size = 15),
    plot.margin = margin(20, 20, 10, 5),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

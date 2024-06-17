
distance_method_folders <- list.dirs(path = ".", full.names = TRUE, recursive = FALSE)

# Save all distance method results from AvTD in a list 
AvTD_all_dist <- list()
all_results_with_Ne_final_all_dist <- list()
all_results_with_NeS_final_all_dist <- list()
corr_all_dist <- list()

for (Index in 2:length(distance_method_folders)) {
  dir_name_distance_method <- (distance_method_folders[Index])
  
  
  
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
    diversity_results <- read.csv(paste(dir_name_distance_method, "/Results", describe_results_variable, "/diversity/diversity_results.csv", sep = "")) 
    diversity_results <- subset(diversity_results, Pop != "Total")
    lambda <- c(lambda, diversity_results$X.N..N...1.....lambda)  
    # Load AMOVA results
    amova_results <- read.csv(paste(dir_name_distance_method, "/Results", describe_results_variable, "/AMOVA/amova_results.csv", sep = "")) 
    AMOVA <- c(AMOVA, amova_results$Difference) 
    # Load EcoSim results
    Overlaps_results <- read.csv(paste(dir_name_distance_method, "/Results", describe_results_variable, "/Overlaps/Overlaps_results.csv", sep = ""))
    EcoSim <- c(EcoSim, Overlaps_results$observed_overlap)
    # Load AvTD results
    AvTD_results <- read.csv(paste(dir_name_distance_method, "/Results", describe_results_variable, "/AvTD/AvTD_results.csv", sep = ""))
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
    diversity_results <- read.csv(paste(dir_name_distance_method, "/Results", describe_results_variable, "/diversity/diversity_results.csv", sep = "")) 
    #diversity_results <- subset(diversity_results, Pop != "Total")
    # Load the Overlaps table into R
    Overlaps_results <- read.csv(paste(dir_name_distance_method, "/Results", describe_results_variable, "/Overlaps/Overlaps_results.csv", sep = ""))
    # Load the Amova table into R
    amova_results <- read.csv(paste(dir_name_distance_method, "/Results", describe_results_variable, "/AMOVA/amova_results.csv", sep = "", quote = ""))
    # Load the Ne table into R
    Ne_results <- read.csv(paste(dir_name_distance_method, "/Results", describe_results_variable, "/NeEstimator/Ne_results.csv", sep = "", quote = ""))   # falls diese Tabelle nicht existiert soll Ne aus den Kalkulationen weggelassen werden
    # Load AvTD
    AvTD_results <- read.csv(paste(dir_name_distance_method, "/Results", describe_results_variable, "/AvTD/AvTD_results.csv", sep = "", quote = ""))
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
  
  ################ Load results with Speed Ne ####################################
  ################################################################################
  all_results_with_NeS_final <- data.frame()
  for (name in all_names_with_SpeedNe_results) {
    file_path <- name 
    # This code helps to structure the results, especially if you have several data sets.
    describe_results_variable <- paste("_", tools::file_path_sans_ext(file_path), sep = "")
    diversity_results <- read.csv(paste(dir_name_distance_method, "/Results", describe_results_variable, "/diversity/diversity_results.csv", sep = "")) 
    #diversity_results <- subset(diversity_results, Pop != "Total")
    # Load the Overlaps table into R
    Overlaps_results <- read.csv(paste(dir_name_distance_method, "/Results", describe_results_variable, "/Overlaps/Overlaps_results.csv", sep = ""))
    # Load the Amova table into R
    amova_results <- read.csv(paste(dir_name_distance_method, "/Results", describe_results_variable, "/AMOVA/amova_results.csv", sep = "", quote = ""))
    # Load the Ne table into R
    Ne_results <- read.csv(paste(dir_name_distance_method, "/Results", describe_results_variable, "/NeEstimator/Ne_results.csv", sep = "", quote = ""))   
    NeS_results <- read.csv(paste(dir_name_distance_method, "/Results", describe_results_variable, "/NeSpeed/Ne_results.csv", sep = "", quote = ""))   
    # Load AvTD
    AvTD_results <- read.csv(paste(dir_name_distance_method, "/Results", describe_results_variable, "/AvTD/AvTD_results.csv", sep = "", quote = ""))
    if (Ne_results[[1]][1] <= NeS_results[[1]][1]) {
      # Prepare data for correlation calculations with Ne:
      # For analyses including Ne rows of the other methods most likely must be removed, because it might have been impossible to calculate Ne for all sites.
      # remove information about confidence interval to keep it simple
      all_results_with_Ne <- NeS_results[, -c(3, 4, 5, 6)]
      colnames(all_results_with_Ne)[2] <- "NeS"
      # create new columns for every calculation method
      all_results_with_Ne$allelic_diversity <- NA
      all_results_with_Ne$observed_overlap <- NA
      all_results_with_Ne$Difference <- NA
      all_results_with_Ne$Ne <- NA
      all_results_with_Ne$AvTD <- NA
      all_results_with_Ne$data <- name
      # Append Ne table, add results of other calculations. 
      # Thereby unnecessary rows are removed automatically from the data set. 
      for(site in all_results_with_Ne$site) {
        all_results_with_Ne$allelic_diversity[which(all_results_with_Ne$site == site)] <- diversity_results$X.N..N...1.....lambda[which(diversity_results$Pop == site)]
        all_results_with_Ne$observed_overlap[which(all_results_with_Ne$site == site)] <- Overlaps_results$observed_overlap[which(Overlaps_results$site == site)]
        all_results_with_Ne$Difference[which(all_results_with_Ne$site == site)] <- amova_results$Difference [which(amova_results$site == site)]
        all_results_with_Ne$Ne[which(all_results_with_Ne$site == site)] <- Ne_results$Ne[which(NeS_results$site == site)]
        all_results_with_Ne$AvTD[which(all_results_with_Ne$site == site)] <- AvTD_results$Delta..1[which(AvTD_results$X == site)]
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
      all_results_with_Ne$AvTD <- NA
      all_results_with_Ne$data <- name
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
        all_results_with_Ne$AvTD[i] <- AvTD_results$Delta..1[which(AvTD_results$X == site)]
        # Increment the index for the next run
        i <- i + 1
      }
    }
    all_results_with_NeS_final <- rbind(all_results_with_NeS_final, all_results_with_Ne)
  }
  # delete rows that include NA
  all_results_with_NeS_final <- all_results_with_NeS_final %>%
    na.omit()
  
  
  
  ############################# check for correlations ################################################
  #####################################################################################################
  correlation_method <- "kendall" # can be "pearson", "kendall" or "spearm"
  
  # Test the correlation between Ne calculated with Speed-Ne and AvTD
  NeS_AvTD_correlation_test <- cor.test(as.numeric(all_results_with_NeS_final$NeS), as.numeric(all_results_with_NeS_final$observed_overlap), method = correlation_method)
  # save p-value
  NeS_AvTD_correlation_p_value <- NeS_AvTD_correlation_test$p.value
  # save the estimated correlation coefficient
  NeS_AvTD_correlation_coefficient <- NeS_AvTD_correlation_test$estimate
  
  # Test the correlation between Ne Estimator results and AvTD 
  Ne_AvTD_correlation_test <- cor.test(as.numeric(all_results_with_Ne_final$Ne),as.numeric(all_results_with_Ne_final$AvTD), method = correlation_method)
  # save p-value
  Ne_AvTD_correlation_p_value <- Ne_AvTD_correlation_test$p.value
  # save the estimated correlation coefficient
  Ne_AvTD_correlation_coefficient <- Ne_AvTD_correlation_test$estimate
  
  
  # Test the correlation between AvTD and AMOVA looking only at sites with Ne Estimator calculated
  AvTD_AMOVA_correlation_test <- cor.test(as.numeric(AvTD[AMOVA <= 40]), as.numeric(AMOVA[AMOVA <= 40]) , method = correlation_method)
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
  
  
  # Create a table with correlation test results
  correlation_results <- data.frame(
    tested_methods = c("AMOVA", "lambda", "Ne",  "NeS", "Overlaps"),
    p_value = c(AvTD_AMOVA_correlation_p_value, 
                diversity_AvTD_correlation_p_value, 
                Ne_AvTD_correlation_p_value, 
                NeS_AvTD_correlation_p_value, 
                AvTD_overlap_correlation_p_value),  
    correlation_coefficient = c(AvTD_AMOVA_correlation_coefficient,
                                diversity_AvTD_correlation_coefficient,
                                Ne_AvTD_correlation_coefficient,
                                NeS_AvTD_correlation_coefficient,
                                AvTD_overlap_correlation_coefficient)
  )

  
  ####### save important information for every distance method #################
  newname_AvTD <- paste("AvTD_", basename(dir_name_distance_method), sep = "")
  AvTD_all_dist[[newname_AvTD]] <- AvTD
  
  newname_all_results_with_Ne <- paste("all_results_with_Ne_", basename(dir_name_distance_method), sep = "")
  all_results_with_Ne_final_all_dist[[newname_all_results_with_Ne]] <- all_results_with_Ne_final
  
  newname_all_results_with_NeS <- paste("all_results_with_NeS_", basename(dir_name_distance_method), sep = "")
  all_results_with_NeS_final_all_dist[[newname_all_results_with_NeS]] <- all_results_with_NeS_final
  
  newname_corr <- paste("corr_", basename(dir_name_distance_method), sep = "")
  corr_all_dist[[newname_corr]] <- correlation_results
  
}
#### Correlations
corr_all_dist <- rbind(
  mutate(corr_all_dist$corr_Edwards, tested_methods = paste(tested_methods, "E", sep = "")),
  mutate(corr_all_dist$corr_Nei, tested_methods = paste(tested_methods, "N", sep = "")),
  mutate(corr_all_dist$corr_pairwise, tested_methods = paste(tested_methods, "P", sep = "")),
  mutate(corr_all_dist$corr_taxa2dist, tested_methods = paste(tested_methods, "T", sep = ""))
)



par(mfrow = c(4, 5), # print several plots in one
    mar = c(0, 0, 0, 0), # the correlation squares should not have margin
    oma = c(6, 7, 6, 5)) # there should be a margin around all correlations

counter <- 0 # for y labels
for(i in 1:nrow(corr_all_dist))  {
  
  corr_val <- corr_all_dist[i,3]
  p_val <- corr_all_dist[i,2]
  
  # column names
  DnD_methods_without_Delta <- c(expression(bold("AMOVA")), 
                                 expression(bold("λ"["cor"])),
                                 expression(bold("Ne Estimator")), 
                                 expression(bold("Speed Ne")), 
                                 expression(bold("Allelic Overlap")))
  plot(1,
       type="n",
       xlim=c(0, 1),
       ylim=c(0, 1),
       xlab="", 
       ylab= "",
       axes=FALSE,
       main = title(ifelse(i < 6, DnD_methods_without_Delta[i], ""), # only print the methods for the upper row of plots
                    xpd =NA, # allows main to show up outside of the plot margins
                    line = 1, # moves main outside the plot
                    cex.main = 1.2), # font size
       asp = 1) # the correlations will be shown as a square
  
  plot_frame <- par("usr")  # aktuelle Grenzen des Plotrahmens erhalten
  rect(plot_frame[1], plot_frame[3], plot_frame[2], plot_frame[4], border = "gray", lwd = 0.5)
  
  # Koordinaten für das innere Quadrat berechnen
  inner_size <- sqrt(abs(corr_val))  # Größe des inneren Quadrats, um 21% der Fläche des äußeren Quadrats einzunehmen
  inner_x <- (plot_frame[2] + plot_frame[1]) / 2 - inner_size / 2  # x-Koordinate des inneren Quadrats
  inner_y <- (plot_frame[4] + plot_frame[3]) / 2 - inner_size / 2  # y-Koordinate des inneren Quadrats
  
  #find colors
  # if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  #   install.packages("RColorBrewer")
  # }
  # library(RColorBrewer)
  # palette <- colorRampPalette(brewer.pal(11, "RdBu"))
  # num_colors <- 10
  # farben <- palette(num_colors)
  
  # Color for the inner square
  if (corr_val < 0) {
    if (corr_val > -0.2) {
      corr_color <- "#FAE7DC"
    } else if (corr_val > -0.4) {
      corr_color <- "#F7B698"
    } else if (corr_val > -0.6) {
      corr_color <- "#DC6F58"
    } else if (corr_val > -0.8) {
      corr_color <- "#B51F2E"
    } else {
      corr_color <- "#67001F"
    }
  } else {
    if (corr_val < 0.2) {
      corr_color <- "#E1EDF3"
    } else if (corr_val < 0.4) {
      corr_color <- "#A7CFE4"
    } else if (corr_val < 0.6) {
      corr_color <- "#549EC9"
    } else if (corr_val < 0.8) {
      corr_color <- "#246BAE" 
    } else if (corr_val < 1) {
      corr_color <- "#053061" 
    }
  }
  # symbol for p-value
  if (p_val < 0.001) {
    p_val <- "***"
  } else if (p_val < 0.01) {
    p_val <- "**"
  } else if (p_val < 0.05) {
    p_val <- "*"
  }
  
  # Inneres Quadrat zeichnen
  rect(inner_x, inner_y, inner_x + inner_size, inner_y + inner_size, border="transparent", col= corr_color)
  # Text in die Mitte des Plots schreiben
  text(x = 0.5, y = 0.5, labels = round(corr_val, 2), cex = 1.5, font = 2)
  # Text für p-Wert hochgestellt hinter dem korrigierten Wert schreiben
  text(x = 0.5, y = 0.5, labels = p_val, cex = 1.5, font = 2, adj = c(-1, -0.3))
  
  distance_methods <- c(expression(bold("Edwards")), 
                        expression(bold("Nei")),
                        expression(bold("pairwise")), 
                        expression(bold("taxa2dist()")))
  
  # Add y axis labels
  if (i %% 5 == 1) {
    counter <- counter +1
    text(x = -0.5, y = 0.5,
         distance_methods[counter],
         xpd = NA, 
         cex = 1.2)
  }  
}

# Create the legend explaining the color code: 
colors <- c("#053061", "#246BAE", "#549EC9", "#A7CFE4", "#E1EDF3", "#FAE7DC",  "#F7B698", "#DC6F58", "#B51F2E", "#67001F")
num_rects <- 10 # number of colors/rectangles that should be created

# coordinates of the legend
x1 <- -4.34
y1 <- -0.5
x2 <- 1.04
y2 <- -0.3

rect_width <- (x2 - x1) / num_rects  # width of every rectangle

# draw one rectangle for every color
for (i in 1:num_rects) {
  rect(x1 + (i - 1) * rect_width, y1, x1 + i * rect_width, y2,
       border = "transparent", col = colors[i], xpd = NA)
}

# Create a frame for the legend
rect(x1 , y1, x2, y2, 
     border = "black",
     xpd = NA)

# Add an axis
axis(side = 1, at = seq(x1, x2, length.out = num_rects + 1),
      labels = FALSE,
      pos = -0.5, 
      xpd = NA)

#Add axis labels
text(x = seq(x1, x2, length.out = num_rects + 1),
      y = -0.7,
      labels = c("1", "0.8", "0.6", "0.4", "0.2", "0", "-0.2", "-0.4", "-0.6", "-0.8", "-1"),
      xpd = NA)



















######################### plot y ~ x ###########################################################
################################################################################################
# combine several plots as one plot
par(mfrow = c(3, 4),
    mar = c(5, 5, 4, 2) + 0.1)


###Edwards
plot(AvTD_all_dist$AvTD_Edwards[AMOVA <= 40],   # plot AMOVA ~ AvTD
     AMOVA[AMOVA <= 40],
     xlab = expression(Delta["j"]^"+"),
     ylab = "AMOVA", 
     cex.lab = 1.2,               
     cex.axis = 1)
plot(AvTD_all_dist$AvTD_Edwards,               # plot EcoSim ~ AvTD
     EcoSim,
     xlab = expression(Delta["j"]^"+"),
     ylab = "Allelic Overlap", 
     cex.lab = 1.2,               
     cex.axis = 1)
plot(lambda,             # plot AvTD ~ lambda
     AvTD_all_dist$AvTD_Edwards,
     xlab = expression("λ"["cor"]),
     ylab = expression(Delta["j"]^"+"), 
     cex.lab = 1.2,               
     cex.axis = 1)
# Ne und AvTD
plot(subset(all_results_with_Ne_final_all_dist$all_results_with_Ne_Edwards, Ne <= 2000)$Ne,
     subset(all_results_with_Ne_final_all_dist$all_results_with_Ne_Edwards, Ne <= 2000)$AvTD,
     xlab = expression(italic("N")[italic("e")]),
     ylab = expression(Delta["j"]^"+"), 
     cex.lab = 1.2,               
     cex.axis = 1, 
     col= "darkgoldenrod") 
points(all_results_with_NeS_final_all_dist$all_results_with_NeS_Edwards$NeS, 
       all_results_with_NeS_final_all_dist$all_results_with_NeS_Edwards$AvTD,
       col = "cadetblue4")
colors <- c("darkgoldenrod", "cadetblue4")
labels <- c("Ne Estimator", "Speed Ne")
legend(x = "bottomright", legend = labels, fill = colors)

### Nei 
plot(AvTD_all_dist$AvTD_Nei[AMOVA <= 40],   # plot AMOVA ~ AvTD
     AMOVA[AMOVA <= 40],
     xlab = expression(Delta["j"]^"+"),
     ylab = "AMOVA", 
     cex.lab = 1.2,               
     cex.axis = 1)
plot(AvTD_all_dist$AvTD_Nei,               # plot EcoSim ~ AvTD
     EcoSim,
     xlab = expression(Delta["j"]^"+"),
     ylab = "Allelic Overlap", 
     cex.lab = 1.2,               
     cex.axis = 1)
plot(lambda,             # plot AvTD ~ lambda
     AvTD_all_dist$AvTD_Nei,
     xlab = expression("λ"["cor"]),
     ylab = expression(Delta["j"]^"+"), 
     cex.lab = 1.2,               
     cex.axis = 1)
# Ne und AvTD
plot(subset(all_results_with_Ne_final_all_dist$all_results_with_Ne_Nei, Ne <= 2000)$Ne,
     subset(all_results_with_Ne_final_all_dist$all_results_with_Ne_Nei, Ne <= 2000)$AvTD,
     xlab = expression(italic("N")[italic("e")]),
     ylab = expression(Delta["j"]^"+"), 
     cex.lab = 1.2,               
     cex.axis = 1, 
     col= "darkgoldenrod") 
points(all_results_with_NeS_final_all_dist$all_results_with_NeS_Nei$NeS, 
       all_results_with_NeS_final_all_dist$all_results_with_NeS_Nei$AvTD,
       col = "cadetblue4")
colors <- c("darkgoldenrod", "cadetblue4")
labels <- c("Ne Estimator", "Speed Ne")
legend(x = "bottomright", legend = labels, fill = colors)

### pairwise 
plot(AvTD_all_dist$AvTD_pairwise [AMOVA <= 40],   # plot AMOVA ~ AvTD
     AMOVA[AMOVA <= 40],
     xlab = expression(Delta["j"]^"+"),
     ylab = "AMOVA", 
     cex.lab = 1.2,               
     cex.axis = 1)
plot(AvTD_all_dist$AvTD_pairwise ,               # plot EcoSim ~ AvTD
     EcoSim,
     xlab = expression(Delta["j"]^"+"),
     ylab = "Allelic Overlap", 
     cex.lab = 1.2,               
     cex.axis = 1)
plot(lambda,             # plot AvTD ~ lambda
     AvTD_all_dist$AvTD_pairwise ,
     xlab = expression("λ"["cor"]),
     ylab = expression(Delta["j"]^"+"), 
     cex.lab = 1.2,               
     cex.axis = 1)
# Ne und AvTD
plot(subset(all_results_with_Ne_final_all_dist$all_results_with_Ne_pairwise , Ne <= 2000)$Ne,
     subset(all_results_with_Ne_final_all_dist$all_results_with_Ne_pairwise , Ne <= 2000)$AvTD,
     xlab = expression(italic("N")[italic("e")]),
     ylab = expression(Delta["j"]^"+"), 
     cex.lab = 1.2,               
     cex.axis = 1, 
     col= "darkgoldenrod") 
points(all_results_with_NeS_final_all_dist$all_results_with_NeS_pairwise $NeS, 
       all_results_with_NeS_final_all_dist$all_results_with_NeS_pairwise $AvTD,
       col = "cadetblue4")
colors <- c("darkgoldenrod", "cadetblue4")
labels <- c("Ne Estimator", "Speed Ne")
legend(x = "bottomright", legend = labels, fill = colors)
#StructureD&D: Oncorhynchus K4, Panthera K4, Pinus  K7 
#(number of clusters was choosen by reading the papers belonging to the datasets)

################################### PACKAGES ##################################################
packages <- c("readxl", "dplyr", "tidyr")
# install packages if they are not installed yet
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)          
  }
  library(package, character.only = TRUE)
}


# import assignment of individuals to populations
setwd("C:/Users/Gronefeld/Desktop/D&D_examples/Clumpak_results")
Oncorhynchus_q_matrix <- read.csv("Oncorynchus_tschawytscha/K=4/MajorCluster/CLUMPP.files/ClumppIndFile.output", header = FALSE, sep = "", fill = TRUE)
Panthera_q_matrix <- read.csv("Panthera_leo/K=4/MajorCluster/CLUMPP.files/ClumppIndFile.output", header = FALSE, sep = "", fill = TRUE)
Pinus_q_matrix <- read.csv("Pinus_halepensis/K=7/MajorCluster/CLUMPP.files/ClumppIndFile.output", header = FALSE, sep = "", fill = TRUE)


# import coordinates and site names
setwd("C:/Users/Gronefeld/Desktop/D&D_examples")
coordinates_Oncorhynchus <- as.data.frame(read_excel("coordinates.xlsx", sheet = "Oncorynchus_tshawytscha"))
coordinates_Panthera <- as.data.frame(read_excel("coordinates.xlsx", sheet = "Panthera_leo"))
coordinates_Pinus <- as.data.frame(read_excel("coordinates.xlsx", sheet = "Pinus_halepensis"))

# import site names and names individual
setwd("C:/Users/Gronefeld/Desktop/D&D_examples/Structure_and_plink/str_files")
Oncorhynchus_str <- read.csv("Oncorhynchus_tshawytscha.str", header = FALSE, sep = "", fill = TRUE)
Oncorhynchus_str <- Oncorhynchus_str[-1, ]
Panthera_str <- read.csv("Panthera_leo.str", header = FALSE, sep = "", fill = TRUE)
Panthera_str <- Panthera_str[-1, ]
Panthera_str$V1 <- rep(1:length(unique(Panthera_q_matrix$V1)), each = 2)
Pinus_str <- read.csv("Pinus_halepensis.str", header = FALSE, sep = "", fill = TRUE)
Pinus_str <- Pinus_str[-1, ]

# combine coordinates with site names and individual names
Oncorhynchus_str <- merge(Oncorhynchus_str, coordinates_Oncorhynchus, by.x = "V2", by.y = "site", all.x = TRUE)
site_names_Panthera <- data.frame(names = c("Kruger_NP", 
                                            "Welgevonden_GR", 
                                            "HluhluweiMfolozi_Park", 
                                            "Mun", 
                                            "Ukutula_Lion_Park"), 
                                  nr_names = c(9, 19, 13, 18, 24))
# Merge `Panthera_str` with `site_names_Panthera` to add `nr_names`
Panthera_str <- merge(Panthera_str, site_names_Panthera, by.x = "V2", by.y = "names", all.x = TRUE)
# Add coordinates (Lat and Long) based on `nr_names`
Panthera_str <- merge(Panthera_str, coordinates_Panthera, by.x = "nr_names", by.y = "site", all.x = TRUE)
Pinus_str <- merge(Pinus_str, coordinates_Pinus, by.x = "V2", by.y = "site", all.x = TRUE)



############################# organize structure results #######################################

Oncorhynchus_cluster_probs <- Oncorhynchus_q_matrix[, 6:9]
colnames(Oncorhynchus_cluster_probs) <- c("Cluster1", "Cluster2", "Cluster3", "Cluster4")
Oncorhynchus_cluster_probs$id <- Oncorhynchus_q_matrix$V1

prob_col <- c()
for (ind_number in 1:nrow(Oncorhynchus_cluster_probs)){  
  for (cluster_number in 1:4){
    cluster_prob <- Oncorhynchus_cluster_probs[ind_number,cluster_number]
    prob_col <- c(prob_col, cluster_prob)
  }
}
cluster_col <- c()
for(ind_number in 1:nrow(Oncorhynchus_cluster_probs)) {
  cluster_name <- c(1, 2, 3, 4)
  cluster_col <- c(cluster_col, cluster_name)
}
ind_col <- c()
for(ind_number in 1:nrow(Oncorhynchus_cluster_probs)) {
  ind_name <- c(rep(Oncorhynchus_cluster_probs[ind_number, 5], 4))
  ind_col <- c(ind_col, ind_name)
}
Oncorhynchus_organised_cluster_probs <- data.frame(id = ind_col, 
                                               prob = prob_col, 
                                               cluster = cluster_col)





Panthera_cluster_probs <- Panthera_q_matrix[, 6:9]
colnames(Panthera_cluster_probs) <- c("Cluster1", "Cluster2", "Cluster3", "Cluster4")
Panthera_cluster_probs$id <- Panthera_q_matrix$V1

prob_col <- c()
for (ind_number in 1:nrow(Panthera_cluster_probs)){  
  for (cluster_number in 1:4){
    cluster_prob <- Panthera_cluster_probs[ind_number,cluster_number]
    prob_col <- c(prob_col, cluster_prob)
  }
}
cluster_col <- c()
for(ind_number in 1:nrow(Panthera_cluster_probs)) {
  cluster_name <- c(1, 2, 3, 4)
  cluster_col <- c(cluster_col, cluster_name)
}
ind_col <- c()
for(ind_number in 1:nrow(Panthera_cluster_probs)) {
  ind_name <- c(rep(Panthera_cluster_probs[ind_number, 5], 4))
  ind_col <- c(ind_col, ind_name)
}
Panthera_organised_cluster_probs <- data.frame(id = ind_col, 
                                      prob = prob_col, 
                                      cluster = cluster_col)





Pinus_cluster_probs <- Pinus_q_matrix[, 6:12]
colnames(Pinus_cluster_probs) <- c("Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5", "Cluster6", "Cluster7")
Pinus_cluster_probs$id <- Pinus_q_matrix$V1

prob_col <- c()
for (ind_number in 1:nrow(Pinus_cluster_probs)){  
  for (cluster_number in 1:7){
    cluster_prob <- Pinus_cluster_probs[ind_number,cluster_number]
    prob_col <- c(prob_col, cluster_prob)
  }
}
cluster_col <- c()
for(ind_number in 1:nrow(Pinus_cluster_probs)) {
  cluster_name <- c(1, 2, 3, 4, 5, 6, 7)
  cluster_col <- c(cluster_col, cluster_name)
}
ind_col <- c()
for(ind_number in 1:nrow(Pinus_cluster_probs)) {
  ind_name <- c(rep(Pinus_cluster_probs[ind_number, 8], 7))
  ind_col <- c(ind_col, ind_name)
}
Pinus_organised_cluster_probs <- data.frame(id = ind_col, 
                                              prob = prob_col, 
                                              cluster = cluster_col)


############ combine clusters with coordinates, calculate average of clusters for one site ################
Oncorhynchus_str <- merge(Oncorhynchus_organised_cluster_probs, Oncorhynchus_str, by.x = "id", by.y = "V1", all.x = TRUE)
Oncorhynchus_coordinate_cluster <- aggregate(prob ~ V2 + cluster, data = Oncorhynchus_str, mean)
Oncorhynchus_coordinates <- unique(Oncorhynchus_str[, c("V2", "Lat", "Long")])
Oncorhynchus_coordinate_cluster <- merge(Oncorhynchus_coordinate_cluster, Oncorhynchus_coordinates, by = "V2", all.x = TRUE)

Panthera_str <- merge(Panthera_organised_cluster_probs, Panthera_str, by.x = "id", by.y = "V1", all.x = TRUE)
Panthera_coordinate_cluster <- aggregate(prob ~ V2 + cluster, data = Panthera_str, mean)
Panthera_coordinates <- unique(Panthera_str[, c("V2", "Lat", "Long")])
Panthera_coordinate_cluster <- merge(Panthera_coordinate_cluster, Panthera_coordinates, by = "V2", all.x = TRUE)

Pinus_str <- merge(Pinus_organised_cluster_probs, Pinus_str, by.x = "id", by.y = "V1", all.x = TRUE)
Pinus_coordinate_cluster <- aggregate(prob ~ V2 + cluster, data = Pinus_str, mean)
Pinus_coordinates <- unique(Pinus_str[, c("V2", "Lat", "Long")])
Pinus_coordinate_cluster <- merge(Pinus_coordinate_cluster, Pinus_coordinates, by = "V2", all.x = TRUE)

####### create a df with Clusters one to final Cluster as column headers #####

Oncorhynchus_coordinate_cluster <- as.data.frame(Oncorhynchus_coordinate_cluster %>%
                                                pivot_wider(names_from = cluster, values_from = prob, names_prefix = "Cluster") %>%
                                                arrange(V2))
Panthera_coordinate_cluster <- as.data.frame(Panthera_coordinate_cluster %>%
                                               pivot_wider(names_from = cluster, values_from = prob, names_prefix = "Cluster") %>%
                                               arrange(V2))
Pinus_coordinate_cluster <- as.data.frame(Pinus_coordinate_cluster %>%
                                              pivot_wider(names_from = cluster, values_from = prob, names_prefix = "Cluster") %>%
                                              arrange(V2))

#### add coordinate frequency
Oncorhynchus_unique_ids_per_location <- as.data.frame(Oncorhynchus_str %>%
                                                     group_by(V2) %>%
                                                     summarise(unique_ids_count = n_distinct(id)))
Oncorhynchus_coordinate_cluster <- Oncorhynchus_coordinate_cluster %>%
  left_join(Oncorhynchus_unique_ids_per_location, by = "V2")

Panthera_unique_ids_per_location <- as.data.frame(Panthera_str %>%
  group_by(V2) %>%
  summarise(unique_ids_count = n_distinct(id)))
Panthera_coordinate_cluster <- Panthera_coordinate_cluster %>%
  left_join(Panthera_unique_ids_per_location, by = "V2")

Pinus_unique_ids_per_location <- as.data.frame(Pinus_str %>%
                                                   group_by(V2) %>%
                                                   summarise(unique_ids_count = n_distinct(id)))
Pinus_coordinate_cluster <- Pinus_coordinate_cluster %>%
  left_join(Pinus_unique_ids_per_location, by = "V2")



#### add KBA info ####
setwd("C:/Users/Gronefeld/Desktop/D&D_examples")
KBA <- read.csv("KBA.csv")
KBA$sites <- gsub(" ", "_", KBA$sites)

KBA$sites <- gsub("Mun-ya-wana_GR", "Mun", KBA$sites)
KBA$sites <- gsub("Hluhluwe-iMfolozi_Park", "HluhluweiMfolozi_Park", KBA$sites)
Panthera_all <- merge(
  Panthera_coordinate_cluster,       
  KBA[KBA$dataset == "Panthera_leo", c("sites", "amova", "Eco", "lamda", "AvTD", "NeS", "NeE")],  
  by.x = "V2",                       
  by.y = "sites",                    
  all.x = TRUE                       
)
Panthera_all <- Panthera_all %>%
  filter(!is.na(amova))
Panthera_all[] <- lapply(Panthera_all, function(x) {
  if (is.character(x)) { 
    x <- gsub("B1, A1b", "B1", x) 
    x <- gsub("^A1b$", "0", x)   
  }
  x
})

#KBA$sites[KBA$sites == "Gerska_Reka_Lazaropole"] <- "Gerska"
KBA_Ocorhynchus <- data.frame(
  V2 = c("Caterina", "Cobarde", "Petrohue", "Prat", "Tolten"),
  amova = c(14.5973534362971, 3.32639424228111, 5.67007709573407, 3.14257541638862, 5.64520787067718),
  Eco = c(0.98932, 0.98505, 0.92603, 0.99263, 0.94589), 
  lamda = c(0.999000999000999, 1, 1, 1, 1),
  AvTD = c(97.3304603656214, 95.4557058487331, 98.5577480543752, 97.4474720199293, 98.3361018826135),
  NeE = c(715.3, 466.5, 183.3, 56.4, 38.8)
)
cols_to_calculate <- c("amova", "Eco", "lamda", "AvTD", "NeE")
for (col in cols_to_calculate) {
  KBA_Ocorhynchus[[paste0(col)]] <- (KBA_Ocorhynchus[[col]] / sum(KBA_Ocorhynchus[[col]])) * 100
}
KBA_Ocorhynchus[] <- lapply(KBA_Ocorhynchus, function(col) {
  if (is.numeric(col)) { 
    col <- ifelse(col < 10, 0, "B1") 
  }
  return(col)
})
Oncorhynchus_all <- merge(KBA_Ocorhynchus, Oncorhynchus_coordinate_cluster, by = "V2", all = TRUE)
Oncorhynchus_all <- Oncorhynchus_all %>%
  filter(!is.na(amova))


KBA_Pinus <- data.frame(
  V2 = c("Alzira", "Cabanes", "Calderona", "Eslida", "Montan", "Otricoli", "Serra_Irta", "Sinarcas", "Titaguas"),
  amova = c(0.622557484389811, 3.45756049056777, 0.437741367862861, 2.46913154235739, 1.05685418197282, 23.0293979251689, 1.62283158578719, 1.00269554289293, 0.650397406942982),
  Eco = c(0.99808, 0.93291, 0.98289, 0.99509, 0.99523, 0.99869, 0.99278, 0.9957, 0.99667), 
  lamda = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
  AvTD = c(98.0447388434563, 97.1680289483286, 97.487607787756, 97.9515128503954, 97.6202530612578, 98.0619220915191, 98.0505626902728, 97.9760408167243, 97.940118619233),
  NeE = c(7.4, 144.8, 269.4, 123.0, 97.8, 166.8, 215.2, 89.7, 260.1)
)
cols_to_calculate <- c("amova", "Eco", "lamda", "AvTD", "NeE")
for (col in cols_to_calculate) {
  KBA_Pinus[[paste0(col)]] <- (KBA_Pinus[[col]] / sum(KBA_Pinus[[col]])) * 100
}
KBA_Pinus[] <- lapply(KBA_Pinus, function(col) {
  if (is.numeric(col)) { 
    col <- ifelse(col < 10, 0, "B1") 
  }
  return(col)
})
Pinus_all <- merge(KBA_Pinus, Pinus_coordinate_cluster, by = "V2", all = TRUE)
Pinus_all <- Pinus_all %>%
  filter(!is.na(amova))

##### save results as .csv
write.csv(Oncorhynchus_all , "Oncorhynchus_coordinate_cluster.csv", row.names = FALSE)
write.csv(Panthera_all, "Panthera_coordinate_cluster.csv", row.names = FALSE)
write.csv(Pinus_all, "Pinus_coordinate_cluster.csv", row.names = FALSE)


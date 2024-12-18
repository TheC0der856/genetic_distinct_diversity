################################### PACKAGES ##################################################
# packages needed from CRAN:
packages <- c("dplyr", "tidyr")
# install packages if they are not installed yet
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)          
  }
  library(package, character.only = TRUE)
}

#############################################################################################
################################# IMPORT DATA ###############################################
#############################################################################################

############################## import structure results ####################################
# import assignment of individuals to populations
q_matrix <- read.csv("C:/Users/Gronefeld/Desktop/D&D_examples/Clumpak_results/Ariagona/1733331390/K=3/MajorCluster/CLUMPP.files/ClumppIndFile.output", 
                     header = FALSE, 
                     sep = "", 
                     fill = TRUE)
# import names of individuals from structure input
names <- read.csv("C:/Users/Gronefeld/Desktop/D&D_examples/Ariagona_example/populationsV6.str", 
                  header = FALSE, 
                  sep = "", 
                  fill = TRUE)
names <- unique(names$V1[-1])

############################ import coordinate and collection data ############################
site_coordinates <- read.csv("C:/Users/Gronefeld/Desktop/D&D_examples/coordinates/Ariagona_coordinates.csv")

################################################################################################
############################### ORGANIZE DATA ##################################################
################################################################################################

############################# organize structure results #######################################
# summarize important structure results as "cluster_probs"
cluster_probs <- q_matrix[, 6:8]
colnames(cluster_probs) <- c("Cluster1", "Cluster2", "Cluster3")
cluster_probs$id <- names
# new data frame is needed (have a look at. d3)
#d3 <- data.frame(id = rep(tr$tip.label, each=2),
#                 value = abs(rnorm(60, mean=100, sd=50)),
#                 category = rep(LETTERS[1:2], 30))
prob_col <- c()
for (ind_number in 1:nrow(cluster_probs)){  
  for (cluster_number in 1:3){
    cluster_prob <- cluster_probs[ind_number,cluster_number]
    prob_col <- c(prob_col, cluster_prob)
  }
}
cluster_col <- c()
for(ind_number in 1:nrow(cluster_probs)) {
  cluster_name <- c(1, 2, 3)
  cluster_col <- c(cluster_col, cluster_name)
}
ind_col <- c()
for(ind_number in 1:nrow(cluster_probs)) {
  ind_name <- c(rep(cluster_probs[ind_number, 4], 3))
  ind_col <- c(ind_col, ind_name)
}
organised_cluster_probs <- data.frame(id = ind_col, 
                                      prob = prob_col, 
                                      cluster = cluster_col)



############### combine coordinates with structure results ##################################

# to facilitate working with the table WGS84_X & WGS84_Y coordinates were combined
site_coordinates$combined_coordinates <- paste(site_coordinates$WGS84_X, site_coordinates$WGS84_Y, sep = "_")
# analyze all west Anaga coordinates as one place
westAnaga <- c("Pista del Frontón (west Anaga)", 
               "Llanos del Río (west Anaga)", 
               "Lomo de Las Cuevas (west Anaga)", 
               "Las Brimberas (west Anaga)")
westAnanga_new_place <- "Llanos del Río (west Anaga)"
new_coordinate_westA <- site_coordinates %>%
  filter(place_name == westAnanga_new_place) %>%
  pull(combined_coordinates) %>%
  unique()
site_coordinates <- site_coordinates %>%
  mutate(combined_coordinates = if_else(place_name %in% westAnaga, new_coordinate_westA, combined_coordinates))
# analyze all Las Lagunetas coordinates as one place
lasLagunetas <- c("cerca de las Lagunetas (las Lagunetas)", 
                  "Fuente de Guillén (las Lagunetas)", 
                  "Huerta Bicho (las Lagunetas)", 
                  "Los Charcos (Las Lagunetas)")
lasL_new_place <- "cerca de las Lagunetas (las Lagunetas)"
new_coordinate_lasL <- site_coordinates %>%
  filter(place_name == lasL_new_place) %>%
  pull(combined_coordinates) %>%
  unique()
site_coordinates <- site_coordinates %>%
  mutate(combined_coordinates = if_else(place_name %in% lasLagunetas, new_coordinate_lasL, combined_coordinates))
# create a new dataframe 
coordinate_cluster <- data.frame(
  coordinate = character(),
  Cluster1 = numeric(),
  Cluster2 = numeric(),
  Cluster3 = numeric(),
  stringsAsFactors = FALSE
)
# which individuals were found at which sites?
ids_per_coordinates <- aggregate(ID ~ combined_coordinates, data = site_coordinates, FUN = toString)
# calculate the average of cluster percentages per coordinate 
for (i in seq(ids_per_coordinates$combined_coordinates)) {
  coordinate <- unlist(strsplit(ids_per_coordinates[i, "combined_coordinates"], ", "))
  ids_per_coordinate <- unlist(strsplit(ids_per_coordinates[i, "ID"], ", "))
  cluster_probs_data <- cluster_probs[cluster_probs$id %in% ids_per_coordinate,  ]
  average_cluster_probs <- colMeans(cluster_probs_data[,1:3])
  # save results for every coordinate
  coordinate_cluster <- rbind(coordinate_cluster ,data.frame(
    coordinate = coordinate,
    Cluster1 = average_cluster_probs["Cluster1"],
    Cluster2 = average_cluster_probs["Cluster2"],
    Cluster3 = average_cluster_probs["Cluster3"]
  ))
}
# better row names, to avoid confusion
rownames(coordinate_cluster) <- NULL
# add another column with the frequency of coordinates for having the possibility to adjust for pie chart size
freq_coordinates <- table(site_coordinates$combined_coordinates)
coordinate_cluster$freq_coordinates <- freq_coordinates[coordinate_cluster$coordinate]
# separate coordinates again 
coordinate_cluster <- coordinate_cluster %>%
  separate(coordinate, into = c("WGS84_X", "WGS84_Y"), sep = "_", convert = TRUE)


# add KBAs information to Ariagona_cluster
KBA <- data.frame(
  WGS84_X = c(-16.1570, # east Anaga
              -16.2771, # west Anaga
              -16.4023, # las Lagunetas
              -16.4930, # Aguamansa
              -16.7018, # Icod
              -16.7851, # San José de los Llanos
              -16.8812, # Teno Alto
              -17.9182, # eastern El Hierro
              -18.0750, # Derrabado
              -18.1204),# western El Hierro
  amova = c(4769.26148981956, 4601.89951554838, 6601.67818863055, 4379.79938718167, 6000.06217430043,
            5549.56560172666, 5893.93230942145, 13589.2140489209, 22395.5376386461, 13853.7658827775),
  lamda = c(1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1),
  AvTD = c(87.0765119664933, 86.3898959381409, 88.5618134555291, 88.29389856533, 87.4807115441051,
           88.1168349293304, 86.400351274029, 86.8613313433588, 88.2103181008796, 86.3423454289828),
  NeE = c(-21.3, -111.2, 2.9, 32.7, -29.8, 
          2.7, 1.6, -17.7, -11.0, -17.4)
)

# change all NeE into positive values!
for (i in 1:nrow(KBA)) {
  KBA$NeE[i] <- KBA$NeE[i] + 112
}

cols_to_calculate <- c("amova", "lamda", "AvTD", "NeE")
for (col in cols_to_calculate) {
  KBA[[paste0(col)]] <- (KBA[[col]] / sum(KBA[[col]])) * 100
}
KBA[] <- lapply(KBA, function(col) {
  if (is.numeric(col)) { 
    col <- ifelse(col < 10, 0, "B1") 
  }
  return(col)
})
all <- merge(KBA, coordinate_cluster, by = "WGS84_X", all = TRUE)



write.csv(coordinate_cluster, "C:/Users/Gronefeld/Desktop/D&D_examples/Ariagona_example/Ariagona_KBA_cluster.csv")

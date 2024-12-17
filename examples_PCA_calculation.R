# Load packages
if (!requireNamespace("adegenet", quietly = TRUE)) {
  install.packages("adegenet")
  library(adegenet)
} 
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
if (!requireNamespace("radiator", quietly = TRUE)) {
  devtools::install_github("thierrygosselin/radiator")
  library(radiator)
}
library(radiator)
# if (!requireNamespace("readr", quietly = TRUE)) {
#   install.packages("readr")
#   library(readr)
# }
# library(readr)
# if (!requireNamespace("ggplot2", quietly = TRUE)) {
#   install.packages("ggplot2")
#   library(ggplot2)
# }
# library(ggplot2)
if (!requireNamespace("dartR", quietly = TRUE)) {
  install.packages("dartR")
  library(dartR)
}
#damit dartR funktioniert: 
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("SNPRelate")
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("gdsfmt")


# Define functions
runPLINK <- function(PLINKoptions = "") {
  system(paste('"C:/Program Files/plink_win64_20241022/plink"', PLINKoptions))
}



########################################################################################################

setwd("C:/Users/Gronefeld/Desktop/StructureD&D/Oncorhynchus_tshawytscha")

path_to_Oncor_str <- "Oncorhynchus_tshawytscha.str"
genind <- read.structure(
  file = path_to_Oncor_str,
  n.ind = 341,          # Anzahl der Individuen
  n.loc = 172,          # Anzahl der Loci
  onerowperind = FALSE, # FALSE, weil ein Individuum mehrere Zeilen nutzt
  col.lab = 1,          # Spalte mit Labels für Genotypen
  col.pop = 2,          # Spalte mit Populationsfaktor 
  col.others = NULL,    # Andere optionale Spalten (NULL für keine)
  row.marknames = 1     # Zeile mit Markernamen
)

genomic_converter(genind, output= "plink")


list.files()
file.rename("-1_radiator_genomic_converter_20241128@1210", "Oncorhynchus_tshawytscha_plink")
setwd("Oncorhynchus_tshawytscha_plink")
list.files()
file.rename("radiator_data_20241128@1210.tfam", "Oncorhynchus_tshawytscha.tfam")
file.rename("radiator_data_20241128@1210.tped", "Oncorhynchus_tshawytscha.tped")


dir.create("results")
runPLINK("--tfile Oncorhynchus_tshawytscha --pca --out results/Oncorhynchus_tshawytscha")

# # read in PCA result files
# eigenValues <- read_delim("results/Oncorhynchus_tshawytscha.eigenval", delim = " ", col_names = F)
# eigenVectors <- read_delim("results/Oncorhynchus_tshawytscha.eigenvec", delim = " ", col_names = F)
# 
# # Proportion of variation captured by each vector
# eigen_percent <- round((eigenValues/ (sum(eigenValues))*100), 2)
# 
# ggplot(data = eigenVectors)  +
#   geom_point(mapping = aes(x = X3, y = X4, color = X1, shape = X1), size = 2, show.legend = T) +
#   scale_color_manual(values = c("Tolten" = "grey40", "Petrohue" = "chartreuse3", "Pichicolo" = "chartreuse4",
#                                 "Cobarde" = "cyan4", "Vargas" = "cyan3", "Serrano" = "darkcyan",
#                                 "Prat" = "deepskyblue4", "Santa_Cruz" = "coral3", "Caterina" = "chocolate1")) +
#   scale_shape_manual(values = c("Tolten" = 16, "Petrohue" = 16, "Pichicolo" = 16,
#                                 "Cobarde" = 16, "Vargas" = 16, "Serrano" = 16,
#                                 "Prat" = 16, "Santa_Cruz" = 16, "Caterina" = 16)) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   geom_vline(xintercept = 0, linetype = "dotted") + 
#   labs(title = expression(paste("PCA of ", italic("Oncorhynchus tschawytscha"))), 
#        x = paste0("Principal component 1 (",eigen_percent[1,1]," %)"),
#        y = paste0("Principal component 2 (",eigen_percent[2,1]," %)"),
#        colour = "sites", shape = "sites") +                                                                      # eventuell noch ändern
#   theme_minimal()


########################################################################################################
setwd("C:/Users/Gronefeld/Desktop/StructureD&D/Pinus_halepensis/Pinus_halepensis2")

genind <- read.structure(
  file = "Pinus_halepensis.str",
  n.ind = 1199,          # Anzahl der Individuen
  n.loc = 294,           # Anzahl der Loci
  onerowperind = FALSE, # FALSE, weil ein Individuum mehrere Zeilen nutzt
  col.lab = 1,          # Spalte mit Labels für Genotypen
  col.pop = 2,          # Spalte mit Populationsfaktor 
  col.others = NULL,    # Andere optionale Spalten (NULL für keine)
  row.marknames = 1     # Zeile mit Markernamen
)

genomic_converter(genind, output= "plink")


list.files()
file.rename("02_radiator_genomic_converter_20241128@1413", "Pinus_halepensis_plink")
setwd("Pinus_halepensis_plink")
list.files()
file.rename("radiator_data_20241128@1413.tfam", "Pinus_halepensis.tfam")
file.rename("radiator_data_20241128@1413.tped", "Pinus_halepensis.tped")


dir.create("results")
runPLINK("--tfile Pinus_halepensis --pca --out results/Pinus_halepensis")

# # read in PCA result files
# eigenValues <- read_delim("results/Pinus_halepensis.eigenval", delim = " ", col_names = F)
# eigenVectors <- read_delim("results/Pinus_halepensis.eigenvec", delim = " ", col_names = F)
# 
# # Proportion of variation captured by each vector
# eigen_percent <- round((eigenValues/ (sum(eigenValues))*100), 2)
# 
# ggplot(data = eigenVectors)  +
#   geom_point(mapping = aes(x = X3, y = X4, color = X1, shape = X1),
#              size = 2, show.legend = T, shape = 16) +
#   scale_color_manual(values = c(#left edge:
#                                 "Cabanes" = "mediumpurple", "Palma_de_Mallorca"= "mediumpurple1",
#                                 "Montan" ="mediumpurple2", "Titaguas" = "mediumpurple4", 
#                                 
#                                 "Benicàssim" = "plum1",
#                                 
#                                 #center:
#                                 "Sinarcas"= "hotpink2", "Santiago_de_la_Espada"= "hotpink3",
#                                 "Eslida" = "hotpink","Santanyi"= "hotpink1",
#                                 "Tuéjar" = "hotpink4","Bicorp" = "violetred3",
#                                 "Monovar"= "violetred2", "Vilajoiosa"= "violetred1",  
#                                 "Alzira" = "violetred", "Alcotx"=  "deeppink3", 
#                                 
#                                 #right edge:
#                                 "Tibi" = "pink4", 
#                                 "Carratraca"= "rosybrown1",
#                                 "Benamaurel" = "pink3",
#                                 "Alhama_de_Murcia" = "pink", 
#                                 "Frigiliana"= "lightpink3", 
#                                 "Carlo_Forte" = "lightpink2", 
#                                 "Imperia"  = "pink1",
#                                 "Calderona"= "peachpuff", 
#                                 
#                                 #widely dispersed
#                                 "Tivissa"= "firebrick",
#                                 "Alcantud" = "sienna1",
#                                 "Cabanellas" = "tomato1",
#                                 "Atalix"= "red", 
#                                 
# 
#                                 "Amfilohia"= "forestgreen", "Kassandra"= "olivedrab", 
#                                 "Litorale_Tarantino"= "darkgreen", "Gargano_Marzini" = "palegreen4", 
#                                 "Gargano_Monte_Pucci"= "palegreen3", "Mont_Carmel" = "palegreen2",
#                                 
#                                 "Zuera"= "yellowgreen",
#                                 
#                                 "Thala"= "gold", "Tabarka"= "goldenrod", 
#                                 "Shaharia"= "lightgoldenrod", "Zaouia_Ifrane"= "yellow", 
#                                 
#                                 "Otricoli"= "steelblue", "Quercianella"= "darkturquoise"
#                                 )) +
# 
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   geom_vline(xintercept = 0, linetype = "dotted") + 
#   labs(title = expression(paste("PCA of ", italic("Pinus halepensis"))), 
#        x = paste0("Principal component 1 (",eigen_percent[1,1]," %)"),
#        y = paste0("Principal component 2 (",eigen_percent[2,1]," %)"),
#        colour = "sites", shape = "sites") +                                                                      # eventuell noch ändern
#   theme_minimal()


##########################################################################################################
setwd("C:/Users/Gronefeld/Desktop/StructureD&D/Panthera_leo/Panthera_leo2")

path_to_Panthera_str <- "Panthera_leo.str"
genind <- read.structure(
  file = path_to_Panthera_str,
  n.ind = 359,          # Anzahl der Individuen
  n.loc = 28,           # Anzahl der Loci
  onerowperind = FALSE, # FALSE, weil ein Individuum mehrere Zeilen nutzt
  col.lab = 1,          # Spalte mit Labels für Genotypen
  col.pop = 2,          # Spalte mit Populationsfaktor (0 bedeutet keine)
  col.others = NULL,    # Andere optionale Spalten (NULL für keine)
  row.marknames = 1     # Zeile mit Markernamen
)
g <- dartR::gi2gl(genind)
genomic_converter(g, output= "plink")

list.files()
file.rename("08_radiator_genomic_converter_20241129@1105", "Panthera_leo_plink")
setwd("Panthera_leo_plink")
list.files()
file.rename("radiator_data_20241129@1105.tfam", "Panthera_leo.tfam")
file.rename("radiator_data_20241129@1105.tped", "Panthera_leo.tped")

dir.create("results")
runPLINK("--tfile Panthera_leo --pca --out results/Panthera_leo")

# # read in PCA result files
# eigenValues <- read_delim("results/Panthera_leo.eigenval", delim = " ", col_names = F)
# eigenVectors <- read_delim("results/Panthera_leo.eigenvec", delim = " ", col_names = F)
# 
# # Proportion of variation captured by each vector
# eigen_percent <- round((eigenValues/ (sum(eigenValues))*100), 2)
# 
# ggplot(data = eigenVectors)  +
#   geom_point(mapping = aes(x = X3, y = X4, color = X1, shape = X1), size = 2, show.legend = T) +
#   scale_color_manual(values = c("9" = "green", "19" = "yellow", "13" = "red",
#                                 "18" = "cyan", "24" = "orange")) +
#   scale_shape_manual(values = c("9" = 16, "19" = 16, "13" = 16,
#                                 "18" = 16, "24" = 16)) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   geom_vline(xintercept = 0, linetype = "dotted") + 
#   labs(#title = expression(paste("PCA of ", italic("Panthera leo"))), 
#     x = paste0("Principal component 1 (",eigen_percent[1,1]," %)"),
#     y = paste0("Principal component 2 (",eigen_percent[2,1]," %)"),
#     colour = "sites", shape = "sites") +                                                                      # eventuell noch ändern
#   theme_minimal()

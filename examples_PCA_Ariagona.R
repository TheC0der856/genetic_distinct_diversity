if (!requireNamespace("adegenet", quietly = TRUE)) {
  install.packages("adegenet")
} 
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
if (!requireNamespace("radiator", quietly = TRUE)) {
  devtools::install_github("thierrygosselin/radiator")
}
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

library(adegenet)
library(radiator)
library(readr)
library(ggplot2)

# Define functions
runPLINK <- function(PLINKoptions = "") {
  system(paste('"C:/Program Files/plink_win64_20241022/plink"', PLINKoptions))
}


setwd("C:/Users/Gronefeld/Desktop/D&D_examples/Ariagona_example")

genind <- read.structure(
  file = "populationsV6.str",
  n.ind = 108,          # Anzahl der Individuen
  n.loc = 5198,          # Anzahl der Loci
  onerowperind = FALSE, # FALSE, weil ein Individuum mehrere Zeilen nutzt
  col.lab = 1,          # Spalte mit Labels für Genotypen
  col.pop = 2,          # Spalte mit Populationsfaktor 
  col.others = NULL,    # Andere optionale Spalten (NULL für keine)
  row.marknames = 1     # Zeile mit Markernamen
)

genomic_converter(genind, output= "plink")

list.files()
file.rename("01_radiator_genomic_converter_20241129@1517", "Ariagona_plink")
setwd("Ariagona_plink")
list.files()
file.rename("radiator_data_20241129@1517.tfam", "Ariagona.tfam")
file.rename("radiator_data_20241129@1517.tped", "Ariagona.tped")


dir.create("results")
runPLINK("--tfile Ariagona --pca --out results/Ariagona")

eigenValues <- read_delim("results/Ariagona.eigenval", delim = " ", col_names = F)
eigenVectors <- read_delim("results/Ariagona.eigenvec", delim = " ", col_names = F)
eigen_percent <- round((eigenValues/ (sum(eigenValues))*100), 2)


PCA_plot_Ariagona <- ggplot(data = eigenVectors)  +
  geom_point(mapping = aes(x = X3, y = X4, color = X1, shape = X1), size = 4, show.legend = T) +
  scale_color_manual(values = c("Aguamansa" = "grey",
                                "Derrabado"= "forestgreen", 
                                "eastAnaga" = "navy", 
                                "eastHierro" = "palegreen", 
                                "Icod" = "darkgoldenrod4" ,
                                "Lagunetas" = "skyblue", 
                                "SanJose_Llanos" = "darkgoldenrod1", 
                                "TenoAlto" = "yellow2", 
                                "westAnaga" = "royalblue", 
                                "westHierro" = "darkolivegreen3"),
                     labels = c("Aguamansa", "Derrabado", "Anaga (east)", 
                                "El Hierro (east)", "Icod", "las Lagunetas", 
                                "San José de los Llanos", "Teno Alto" , "Anaga (west)", 
                                "El Hierro (west)")) +
  scale_shape_manual(values = c("Aguamansa" = 16,
                                "Derrabado"= 16, 
                                "eastAnaga" = 16, 
                                "eastHierro" = 16, 
                                "Icod" = 16, 
                                "Lagunetas" = 16, 
                                "SanJose_Llanos" = 16, 
                                "TenoAlto" = 16, 
                                "westAnaga" = 16, 
                                "westHierro" = 16),
                     labels = c("Aguamansa", "Derrabado", "Anaga (east)", 
                                "El Hierro (east)", "Icod", "las Lagunetas", 
                                "San José de los Llanos", "Teno Alto" , "Anaga (west)", 
                                "El Hierro (west)")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") + 
  labs(#title = expression(paste("PCA of ", italic("Ariagona m..."))), 
       x = paste0("Principal component 1 (",eigen_percent[1,1]," %)"),
       y = paste0("Principal component 2 (",eigen_percent[2,1]," %)"),
       colour = "sites", shape = "sites") +                                                                      # eventuell noch ändern
  theme_minimal()+
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "white", color = NA),  
        plot.background = element_rect(fill = "white", color = NA),
        axis.title = element_text(size = 22, face = "bold"), 
        axis.text = element_text(size = 18, face = "bold"),                 
        plot.margin = margin(t = 20, r = 20, b = 18, l = 50), # Mehr Platz um den Plot
        panel.spacing = unit(2, "lines"))
  
ggsave("C:/Users/Gronefeld/Desktop/D&D_examples/Structure_pic/Ariagona/PCA.jpg", 
       plot = PCA_plot_Ariagona  , 
       width = 7.1, 
       height = 5.83, 
       dpi = 300) 

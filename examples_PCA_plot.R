if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
  library(readr)
}
library(readr)
library(ggplot2)



# read in PCA result files
eigenValues <- read_delim("C:/Users/Gronefeld/Desktop/D&D_examples/PCA_results/Panthera_leo.eigenval", delim = " ", col_names = F)
eigenVectors <- read_delim("C:/Users/Gronefeld/Desktop/D&D_examples/PCA_results/Panthera_leo.eigenvec", delim = " ", col_names = F)

# Proportion of variation captured by each vector
eigen_percent <- round((eigenValues/ (sum(eigenValues))*100), 2)

plot_Panthera <- ggplot(data = eigenVectors)  +
  geom_point(mapping = aes(x = X3, y = X4, color = X1, shape = X1), size = 4, show.legend = T) +
  scale_color_manual(values = c("9" = "steelblue2", "19" = "gold", "13" = "darkseagreen2",
                                "18" = "darkolivegreen", "24" = "midnightblue")) +
  scale_shape_manual(values = c("9" = 16, "19" = 16, "13" = 16,
                                "18" = 16, "24" = 16)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") + 
  labs(#title = expression(paste("PCA of ", italic("Panthera leo"))), 
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

ggsave("C:/Users/Gronefeld/Desktop/D&D_examples/Structure_pic/Panthera/PCA.jpg", 
       plot = plot_Panthera , 
       width = 7.1, 
       height = 5.83, 
       dpi = 300) 


# read in PCA result files
eigenValues <- read_delim("C:/Users/Gronefeld/Desktop/D&D_examples/PCA_results/Pinus_halepensis.eigenval", delim = " ", col_names = F)
eigenVectors <- read_delim("C:/Users/Gronefeld/Desktop/D&D_examples/PCA_results/Pinus_halepensis.eigenvec", delim = " ", col_names = F)

# Proportion of variation captured by each vector
eigen_percent <- round((eigenValues/ (sum(eigenValues))*100), 2)

Pinus_plot <- ggplot(data = eigenVectors)  +
  geom_point(mapping = aes(x = X3, y = X4, color = X1, shape = X1),
             size = 4, show.legend = T) +
  scale_color_manual(values = c(
    "Cabanes" = "forestgreen",
    "Montan" = "grey", 
    "Titaguas" = "navy", 
    "Serra_Irta" = "steelblue",
    "Sinarcas"= "aquamarine3", 
    "Eslida" = "palegreen",
    "Alzira" = "yellow", 
    "Calderona"= "peachpuff", 
    "Otricoli"= "sienna" ), 
    labels = c("Alzira", "Cabanes", "Calderona", "Eslida", 
               "Montan", "Otricoli", "Serra Irta", "Sinarcas", "Titaguas")) +
  scale_shape_manual(values = c("Cabanes" = 16, "Montan" = 16, "Titaguas" = 16,
                                "Serra_Irta" = 16, "Sinarcas" = 16, "Eslida" = 16, 
                                "Alzira" = 16, "Calderona"= 16, "Otricoli"= 16), 
                     labels = c("Alzira", "Cabanes", "Calderona", "Eslida", 
                                "Montan", "Otricoli", "Serra Irta", "Sinarcas", "Titaguas")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") + 
  labs(#title = expression(paste("PCA of ", italic("Pinus halepensis"))), 
    x = paste0("Principal component 1 (",eigen_percent[1,1]," %)"),
    y = paste0("Principal component 2 (",eigen_percent[2,1]," %)"),
    colour = "sites", shape = "sites") + 
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "white", color = NA),  
        plot.background = element_rect(fill = "white", color = NA),
        axis.title = element_text(size = 22, face = "bold"), 
        axis.text = element_text(size = 18, face = "bold"),                 
        plot.margin = margin(t = 20, r = 10, b = 18, l = 50), # Mehr Platz um den Plot
        panel.spacing = unit(2, "lines"))

ggsave("C:/Users/Gronefeld/Desktop/D&D_examples/Structure_pic/Pinus/PCA.jpg", 
       plot = Pinus_plot, 
       width = 7.1, 
       height = 5.83, 
       dpi = 300) 



# read in PCA result files
eigenValues <- read_delim("C:/Users/Gronefeld/Desktop/D&D_examples/PCA_results/Oncorhynchus_tshawytscha.eigenval", delim = " ", col_names = F)
eigenVectors <- read_delim("C:/Users/Gronefeld/Desktop/D&D_examples/PCA_results/Oncorhynchus_tshawytscha.eigenvec", delim = " ", col_names = F)

# Proportion of variation captured by each vector
eigen_percent <- round((eigenValues/ (sum(eigenValues))*100), 2)

plot_Oncorhynchus <- ggplot(data = eigenVectors)  +
  geom_point(mapping = aes(x = X3, y = X4, color = X1, shape = X1), size = 4, show.legend = T) +
  scale_color_manual(values = c("Tolten" = "grey62", "Petrohue" = "chartreuse3", 
                                "Cobarde" = "steelblue1", 
                                "Prat" = "royalblue4", "Caterina" = "gold")) +
  scale_shape_manual(values = c("Tolten" = 16, "Petrohue" = 16, 
                                "Cobarde" = 16, 
                                "Prat" = 16,  "Caterina" = 16)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") + 
  labs(#title = expression(paste("PCA of ", italic("Oncorhynchus tschawytscha"))), 
       x = paste0("Principal component 1 (",eigen_percent[1,1]," %)"),
       y = paste0("Principal component 2 (",eigen_percent[2,1]," %)"),
       colour = "sites", shape = "sites") +                                                                      # eventuell noch ändern
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "white", color = NA),  
        plot.background = element_rect(fill = "white", color = NA),
        axis.title = element_text(size = 28, face = "bold"), 
        axis.text = element_text(size = 22, face = "bold"),                 
        plot.margin = margin(t = 20, r = 50, b = 20, l = 50), # Mehr Platz um den Plot
        panel.spacing = unit(2, "lines"))

ggsave("C:/Users/Gronefeld/Desktop/D&D_examples/Structure_pic/Oncorhynchus/10.jpg", 
       plot = plot_Oncorhynchus, 
       width = 8.27, 
       height = 6.5, 
       dpi = 300) 

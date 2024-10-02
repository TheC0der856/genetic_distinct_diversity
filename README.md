Main Calculations:


To finally calculate correlations and identify KBAs and display the ratios of KBAs identified between different methods different scripts were used that all start with genetic_D&D. 
If you want to execute these scripts you should start with downloading files and format them like the example files (originally downloaded from Dryad, but in a different formats): 
Cymodocea nodosa: Arnaud-Haond, Sophie et al. (2014). Data from: Disentangling the influence of mutation and migration in clonal seagrasses using the Genetic Distance Spectrum for microsatellites [Dataset]. Dryad. https://doi.org/10.5061/dryad.3b8k6 
Salmo trutta: Andersson, Anastasia; Karlsson, Sten; Ryman, Nils; Laikre, Linda (2022). Monitoring genetic diversity with new indicators applied to an alpine freshwater top predator [Dataset]. Dryad. https://doi.org/10.5061/dryad.fbg79cnx5 

The next step is tp create an R project and start with opening genetic_D&D__Main_Script.R. The function show_menu() in the end of genetic_D&D__Main_Script.R will guide you through the calculation methods: lambda, AMOVA, allelic overlap, Ne calculations and AvTD. If you want to use the genetic_D&D__Overlaps.R Script you should install EcoSimR into your R project first (https://www.uvm.edu/~ngotelli/EcoSim/EcoSim.html). genetic_D&D_Speed_Ne_split_input.R was used to create a single table for each location so that they can be read into Speed Ne individually. You can run the script to calculate correlations and the script to identify KBAs only after having results for each method for several data sets. genetic_D&D__correlations_Delta_dist.R requires another folder structure: this script should be in a folder together with genetic_D&D_functions.R and genetic_D&D_libraries.R and four folders namend Edwards, Nei, pairwise and taxa2dist. Within each of the four folders are all genetic_D&D scripts accept the correlations_Delta_dist script together with the results of all methods. The only results that change between the folders are the AvTD/Delta+ results. Within the Edwards, the Nei, and the pairwise folder genetic_D&D__AvTD.R should be executed again for all datasets with a defined distance method (distance_method <- "Nei" or "Edward" or "pairwise").

R Markdown (help page): 


The script creating the help page is called How_to_AvTD.R



Distinct Genetic Diversity App to identify KBAs:




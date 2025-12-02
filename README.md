Main Calculations:


To calculate correlations, identify KBAs, and display the ratios of KBAs identified between different methods different scripts were used that all start with genetic_D&D. 
If you want to execute these scripts you should start with downloading a file and format it. The scripts are based on xlsx files. The first column of the table is suppose to be the individual ID, the second column of the table is the name of the area. There is just one row per individual, but two columns per genetic marker. The genetic markers start at the third column. There is no header required for the indiviudal column, the area column, and the columns for every second allele for a locus. The sheet including the table whould have the name welcomeR. Example files, which are in the correct format can be downloaded here. They were originally downloaded from Dryad: 


Cymodocea nodosa: Arnaud-Haond, Sophie et al. (2014). Data from: Disentangling the influence of mutation and migration in clonal seagrasses using the Genetic Distance Spectrum for microsatellites [Dataset]. Dryad. https://doi.org/10.5061/dryad.3b8k6 


Salmo trutta: Andersson, Anastasia; Karlsson, Sten; Ryman, Nils; Laikre, Linda (2022). Monitoring genetic diversity with new indicators applied to an alpine freshwater top predator [Dataset]. Dryad. https://doi.org/10.5061/dryad.fbg79cnx5 

The next step is to download the entire github repository. Some scipts are depending on others and will produce errors if they are executed without the associated scipts on the correct location in the folder. Then you should create an R project. If you want to use the "genetic_D&D__Overlaps.R" Script you should install EcoSimR into your R project first (https://www.uvm.edu/~ngotelli/EcoSim/EcoSim.html). Start with opening "genetic_D&D__Main_Script.R". This script needs to be executed before calculating each of the six methods. After all six methods were calculated for each dataset you can execute the script to calculate correlations and the script to identify KBAs.







#########################################################################


All scripts starting with "examples_" contain calculations to create four pictures that display the genetic structure and areas selected as KBAs for different methods. There are four example data sets, of which Ariagona was calculated separatly. The Ariagona Scripts can be used immidialtly on Supp_Ariagona_dataset.str or the raw reads can be processed into Supp_Ariagona_dataset using "Stacksscript.txt". "examples_PCA_calculation.R" is a script to conduct a PCA, "examples_PCA_plot.R" creats plots of the conducted analyses, "examples_PCA_Ariagona.R" calculates and creates a PCA plot only for the Araigona data set. There are two scripts to convert file formats from .str to .xlsx ("examples_convert_structure_to_xlsx.R") and vice versa ("examples_convert_xlsx_to_structure.R") to calculate the distinct genetic diversity with different methods and run STRUCTURE to find out about genetic clusters. Identified KBAs and structure results are sorted and combined within the script "examples_Ariagona_create_structure_df.R" and "examples_create_Structure_df_for_GIS.R". To calculate the effective population size a new script was written for Ariagona, because RLDNe was updated ("examples_Ariagona_genetic_D&D__NeEstimator.R"). 







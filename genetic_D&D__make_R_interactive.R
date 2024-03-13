show_menu <- function() {
  while (TRUE) {
    cat("=========================== Menu ===========================\n")
    cat("This menu shows you possible methods to calculate genetic diversity and uniqueness.\n")
    cat("Type in the number that precedes the calculation method you want to use to get the results.\n")
    cat("1. Inform myself about the options \n")
    cat("2. Calculate rarefy, rarecurve and rareslope \n")
    cat("3. Calculate AMOVA \n")
    cat("4. Calculate overlaps \n")
    cat("     The prerequisite for calculating overlaps is the use of EcoSim-R. First download EcoSim-R: \n")
    cat("     https://www.uvm.edu/~ngotelli/EcoSim/EcoSim.html \n")
    cat("     After downloading EcoSim-R, save it in the folder of this R project, so that the further code can be executed. \n")
    cat("5. Calculate all and compare all methods\n")
    cat("6. Calculate all methods of several data sets \n")
    cat("     If you want to read in several data sets simultaniously make shure your .xlsx -sheet is called >>welcomeR<<. \n")
    cat("     Otherwise R will not load your input- data. \n")
    cat("7. Compare results of different data sets, without calculating them (again). \n")
    cat("8. Show citations\n")
    cat("0. Exit\n")
    
    choice <- readline("Choose an option: ")
    
    if (choice == "0") {
      cat("The program is terminated.\n")
      break
    } else if (choice == "1") {
      
      cat(" \n")
      cat(" \n") 
      cat("================= rarefy(), rarecurve() and rareslope() =================.\n")
      cat(" \n")
      cat(" \n")
      cat("These three functions are designed to study species diversity.")
      cat("They can be used to compare species diversity between different groups, e.g. different ecological niches, climatic zones, etc.")
      cat("This R-Script applies these three functions to compare allele diversity between different sites.\n")
      cat(" \n")
      cat("rarefy() gives information about the diversity at a certain sample size and shows the standard error for this value.\n")
      cat("rarefy() is based on: \n")
      cat("Heck, K.L., van Belle, G. & Simberloff, D. (1975). Explicit calculation of the rarefaction diversity measurement and the determination of sufficient sample size. Ecology 56, 1459-1461.\n")
      cat("Hurlbert, S.H. (1971). The nonconcept of species diversity: a critique and alternative parameters. Ecology 52, 577-586.\n")
      cat(" \n")
      cat("rarecurve() displays a graph showing how much diversity increases when the sample size is increased.\n")
      cat("How rarecurve() is calculated: \n")
      cat("rarecurve() draws rarefaction curves evaluated by using the interval of step sample sizes, always including 1 and total sample size.")
      cat("The underlying table with the data points for the functions can be viewed with rarecurve( tidy = TRUE).\n")
      cat(" \n")
      cat("rareslope() forms the derivative of the rarefraction curves for a given sample size and displays the slope for that sample size.\n")
      cat(" \n")
      cat("For more questions use ?rarefy() , ?rarecurve() and ?rareslope(). \n")
      cat(" \n")
      cat(" \n")
      cat(" \n")
      cat("This R-script calculates allele diversity for all sites with the largest sample size, which still allows conclusions to be drawn about how allele diversity would increase further with a larger sample. ")
      cat("Because the slopes of the rarefaction curves are to be compared across all locations, only the sample size for which there is still information available at all locations can be chosen. ")
      cat("If the location with the smallest maximum sample size has a significantly smaller maximum sample size than other locations, interesting information about the other locations may be lost. ")
      cat("Whether there are large differences between the locations can be read in the graph created by rarecurve. ")
      cat("The allele diversity and the slope for all locations are entered into a dataframe. ")
      cat("Moreover the script creates a graph with confidence intervals based on the standard errors. ")
      cat("This graph allows to determine whether there may be overlaps in the allelic diversity of the locations.")
      cat(" \n")
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat("================= AMOVA (Analyses of Molecular Variance) using poppr.amova() =================.\n")
      cat(" \n")    
      cat(" \n")   
      cat("AMOVA is a method applied to SNP and microsatellite datasets to determine whether there are significant differences in the genetic composition of a species at different locations. In this R-script, not all sites are compared with each other, but only one site is compared with the rest. The value indicating how significantly different this location is from the other locations is noted in a data frame, so that at the end this significance value can be compared between the locations.\n")
      cat(" \n")   
      cat("There are many ways to calculate an AMOVA. In this case, poppr.amova() from the package poppr is used. ")
      cat("Read more about poppr in this article:\n")
      cat("Kamvar, Z. N., Tabima, J. F., & Grünwald, N. J. (2014). Poppr: an R package for genetic analysis of populations with clonal, partially clonal, and/or sexual reproduction. PeerJ, 2, e281.\n")
      cat("Use the command ?poppr::poppr.amova to read more about poppr.amova().\n")
      cat(" \n")
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat(" \n")    
      cat(" \n")  
      cat("================= Calculating overlaps with EcoSimR using the Pianka Metric =================.\n")
      cat(" \n")    
      cat(" \n")   
      cat(" EcoSimR is a program designed to calculate niche overlaps of species in R. Instead of niche overlaps this R project calculates overlaps of allele occurences at different study sites. \n")
      cat(" Similar to the AMOVA calculation alleles occuring at one site are compared with alleles occuring at all the other sites for beeing able to assess which site shares most alleles with other sites and which site shares the least alleles with other sites. Only the observed overlap value is further processed and the simulated overlap value is left aside. This project kept the default settings. \n")
      cat(" \n")
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat(" \n")    
      cat(" \n")  
      cat("================= Calculating Ne, the effective population size =================.\n")
      cat(" \n")    
      cat(" \n")   
      cat(" The effective population size is the minimum size of a population to keep the observed variation of alleles. This calculation method is not included in this R project, but it includes some Scripts preparing input files for Speed-Ne and processing output files from Speed-Ne. Speed-Ne is an application for Matlab based on the LD- Method. \n")
      cat(" \n")
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat(" \n")    
      cat(" \n")  
      cat("================= Calculating correlations =================.\n")
      cat(" \n")    
      cat(" \n") 
      cat("  The correlation calculation shows whether there is a correlation and how strong it is. Although some data are normally distributed and suitable for a Pearson correlation, this project calculates the correlations with the Kendall method, because also the option is offered to calculate several correlations with each other and the comparability between the correlations suffers if different methods are used. The Kendall method is more precise than the Spearman method, which is why it was chosen.\n") 
      cat(" \n")
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat("================= Calculating linear models =================.\n")
      cat(" \n")    
      cat(" \n") 
      cat(" If there is a significant correlation this project offers an option to detect if any linear model describes the correlation well. A suitable model provides insight how much the results of one method can be explained by the other. \n") 
      #cat(" Furthermore predictions can be done for data, which is not yet existing. \n ")
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      cat(" \n")    
      cat(" \n")
      
    } else if (choice == "2") { 
      
      source("genetic_D&D__rarefy.R")
      
      
    } else if (choice == "3") {
      
      source("genetic_D&D__poppr_AMOVA.R")
      
      
    } else if (choice == "4") {
      
      source("genetic_D&D__Overlaps.R") 
      
    } else if (choice == "5") {
      
      scripts_to_source_list <- list("genetic_D&D__Speed_Ne_split_input.R" , "genetic_D&D__rarefy.R", "genetic_D&D__poppr_AMOVA.R", "genetic_D&D__Overlaps.R", "genetic_D&D__R_NeEstimator.R", "genetic_D&D__Speed_Ne_structure_results.R", "genetic_D&D__correlations.R")
      
      for (Index in 1:length(scripts_to_source_list)) {
        if (Index < 5) {
          # source("genetic_D&D__Speed_Ne_split_input.R")
          # source("genetic_D&D__rarefy.R")
          # source("genetic_D&D__poppr_AMOVA.R")
          # source("genetic_D&D__Overlaps.R")  
          # source("genetic_D&D__R_NeEstimator.R")
          source(scripts_to_source_list[Index][[1]])
        } else if (list.files(path = paste("Results", describe_results_variable, "/Speed_Ne/input/", sep = ""), pattern = "\\.txt$", full.names = TRUE)){
          # source("genetic_D&D__Speed_Ne_structure_results.R")
          # source("genetic_D&D__correlations.R")
          # #source("genetic_D&D__regression.R")
          source(scripts_to_source_list[Index][[1]])
        }
      }

    } else if (choice == "6") {
      
      xlsx_files <- list.files(path = "./", pattern = "\\.xlsx$", full.names = TRUE)
      for (xlsx_file in xlsx_files){
        file_path <- xlsx_file
        sheet <- "welcomeR"
        original_file <- read_xlsx(file_path, sheet = sheet)
        # This code helps keeping an overview of the results.
        describe_results_variable <- paste("_", tools::file_path_sans_ext(file_path), sep = "")
        # specify the name of the folder where the results will be saved
        output_folder_path <- paste("Results", describe_results_variable, sep = "")
        # If it does not exist already
        if (!file.exists(output_folder_path)) {
        # create folder
          dir.create(output_folder_path, recursive = TRUE)
        }
        
        source("genetic_D&D__rarefy.R")
        source("genetic_D&D__poppr_AMOVA.R")
        source("genetic_D&D__Overlaps.R")  
        source("genetic_D&D__correlations.R")
        source("genetic_D&D__regression.R")
      }
      
      source("genetic_D&D_combine_correlations.R")
      
    } else if (choice == "7") {
      
      source("genetic_D&D_combine_correlations.R")
      
    } else if (choice == "8") { 
      
      cat("enables working with genetic data\n")
      print(citation("adegenet"))
      
      cat("simplifies working with tables\n")
      print(citation("dplyr"))
      print(citation("tidyr"))
      
      cat("enables reading .xlsx -files \n")
      print(citation("readxl"))
      #print(citation("openxlsx"))
      
      cat("Creation of nice plots \n")
      print(citation("ggplot2"))
      print(citation("viridis"))
      
      cat("rarefunctions\n")
      print(citation("vegan"))
      # citation("lattice"), is a precondition for vegan
      # citation("permute"), is a precondition for vegan
      cat("rarefy()\n")
      cat("Heck, K.L., van Belle, G. & Simberloff, D. (1975). Explicit calculation of the rarefaction diversity measurement and the determination of sufficient sample size. Ecology 56, 1459-1461.\n")
      cat("Hurlbert, S.H. (1971). The nonconcept of species diversity: a critique and alternative parameters. Ecology 52, 577-586.\n")
      
      cat("AMOVA\n")
      print(citation("poppr"))
      
      cat("Overlaps \n")
      cat( "Nicholas J. Gotelli, Edmund M. Hart and Aaron M. Ellison (2015) EcoSimR: Null model analysis for ecological data. R package version 0.1.0. http://github.com/gotellilab/EcoSimR doi:10.5281/zenodo.16522 \n ")
      cat( "Corresponding BibTeX entry:
        
        @Manual{,
          title = {EcoSimR: Null model analysis for ecological data},
          author = {Nicholas J. Gotelli and Edmund M. Hart and Aaron M.
            Ellison},
          year = {2015},
          note = {R package version 0.1.0},
          url = {http://github.com/gotellilab/EcoSimR},
          doi = {10.5281/zenodo.16522},
        }
       \n" )
      
      cat("Ne, cite Matlab and Speed-Ne or any other program you used.\n")
      
      cat ("The MathWorks Inc. (2022). MATLAB version: 9.13.0 (R2022b), Natick, Massachusetts: The MathWorks Inc. https://www.mathworks.com
            @Manual{,
              year = {2022},
              author = {The MathWorks Inc.},
              title = {Statistics and machine learning toolbox},
              publisher = {The MathWorks Inc.},
              address = {Natick, Massachusetts, United States},
              url = {https://www.mathworks.com/help/stats/index.html}
              }
           \n")
      cat("https://doi.org/10.1111/1755-0998.12759")
      
      cat("correlations \n")
      print(citation("corrplot"))
      
      cat("regression \n")
      print(citation("car"))
      print(citation("gvlma"))
      
    } else {
      cat("This option is invalid. Select an option from the menu. \n")
    }
    
    cat("\n")
  }
}
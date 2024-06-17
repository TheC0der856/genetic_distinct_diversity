

################ Create a data frame with results of all data sets #####################

# which data sets should be looped?
describe_results_variables <- c("_Ambystoma_bishopi", 
                                "_Avicennia_marina", 
                                "_Cameraria_ohridella", 
                                "_Carcinus_meanas", 
                                "_Cercidiphyllum_japonicum", 
                                "_Cymodocea_nodosa", 
                                "_Cystoseira_amentaceae",
                                "_Euphydryas_aurina" ,
                                "_Mytilus_galloprovincialis",
                                "_Pagophila_eburnea", 
                                "_Panthera_leo", 
                                "_Posidonia_oceanica",
                                "_Pyura_chilensis",
                                "_Syncerus_caffer",
                                "_Varroa_jacobsoni", 
                                "_Abies_alba", 
                                "_Argiope_bruennichi", 
                                "_Atriophallophorus_winterbourni", 
                                "_Dracocephalum_ruyschiana",
                                "_Entosphenus_tridentatus",
                                "_Frangula_alnus",
                                "_Gadus_morhua", 
                                "_Melitaea_cinxia",
                                "_Nilparvata_lugens",
                                "_Oncorhynchus_mykiss",
                                "_Oncorhynchus_tshawytscha",
                                "_Physeter_macrocephalus",
                                "_Pinus_halepensis",
                                "_Salmo_trutta",
                                "_Tectona_grandis")

# Create dataframes with all information for both methods
# initialize data frames
NeS_results <- data.frame()
Ne_results <- data.frame()
NeS_errors_all_datasets <- data.frame(
  errors_in_datasets = c(),
  micro_or_SNP = c(),
  describe_results_variable = c(), 
  size_dataset = c()
)
# Fill data frames with Ne results of all data sets
for (describe_results_variable in describe_results_variables) {
  
  ############## microsatellite or SNP data set? ################
  microsatellite_data_sets <- c("_Ambystoma_bishopi", 
                                "_Avicennia_marina", 
                                "_Cameraria_ohridella", 
                                "_Carcinus_meanas", 
                                "_Cercidiphyllum_japonicum", 
                                "_Cymodocea_nodosa", 
                                "_Cystoseira_amentaceae",
                                "_Euphydryas_aurina" ,
                                "_Mytilus_galloprovincialis",
                                "_Pagophila_eburnea", 
                                "_Panthera_leo", 
                                "_Posidonia_oceanica",
                                "_Pyura_chilensis",
                                "_Syncerus_caffer",
                                "_Varroa_jacobsoni")
  SNP_data_sets <- c("_Abies_alba", 
                     "_Argiope_bruennichi", 
                     "_Atriophallophorus_winterbourni", 
                     "_Dracocephalum_ruyschiana",
                     "_Entosphenus_tridentatus",
                     "_Frangula_alnus",
                     "_Gadus_morhua", 
                     "_Melitaea_cinxia",
                     "_Nilparvata_lugens",
                     "_Oncorhynchus_mykiss",
                     "_Oncorhynchus_tshawytscha",
                     "_Physeter_macrocephalus",
                     "_Pinus_halepensis",
                     "_Salmo_trutta",
                     "_Tectona_grandis")
  
  if (describe_results_variable %in% SNP_data_sets) {
    micro_or_SNP <- "SNP"
  } else {
    micro_or_SNP <- "microsatellite"
  }
  
  # The AMOVA file always has the exact number of rows of how many sites the data set has
  # Through a comparison with the AMOVA file we know how many files are missing
  AMOVA <- read.csv(paste("Results", describe_results_variable, "/AMOVA/amova_results.csv", sep = "", quote = ""))  
  
  ############## add Speed Ne data #################################
  if (file.exists(paste("Results", describe_results_variable, "/NeSpeed/Ne_results.csv", sep = "", quote = "")) ) {
    # Load the Ne table into R (Speed-Ne)
    NeS_results_new <- read.csv(paste("Results", describe_results_variable, "/NeSpeed/Ne_results.csv", sep = "", quote = "")) 
    NeS_results_new$describe_results_variable <- describe_results_variable
    NeS_results_new$micro_or_SNP <- micro_or_SNP
    
    # Overlaps must be calculated for every data set individually otherwise its nonsense
    # Calculate how many CIs do not overlap with other CIs excluding CIs = Inf [%]
    # Calculation for parametric CI:
    # inizialize an empty vector for the row names 
    overlapping_rows <- c()
    # compare CIs of all sites with each other
    for (rownumber_CI1 in 1:nrow(NeS_results_new)) {
      for (rownumber_CI2 in 1:nrow(NeS_results_new)) {
        # ignore the confidence interval if one of the interval limits extends to infinity
        # ignore the confidence interval if one of the interval limits is missing
        if (!(is.infinite(NeS_results_new[rownumber_CI1, 5]) ||
              is.infinite(NeS_results_new[rownumber_CI1, 6]) || 
              is.na(NeS_results_new[rownumber_CI1, 5]) ||
              is.na(NeS_results_new[rownumber_CI1, 6]) ||
              is.infinite(NeS_results_new[rownumber_CI2, 5]) ||
              is.infinite(NeS_results_new[rownumber_CI2, 6]) ||
              is.na(NeS_results_new[rownumber_CI2, 5]) ||
              is.na(NeS_results_new[rownumber_CI2, 6]) )
        ) {
          # Check if the row number of the CIs compared is not equal
          if (rownumber_CI2 != rownumber_CI1) {  
            # if an overlap was found, remember the row name
            # to have an overlap CI1_end needs to be >= CI2_start and CI1_start <= CI2_end
            # start = lower end, column 5
            # end = upper end, column 6
            if (NeS_results_new[rownumber_CI1, 6] >= NeS_results_new[rownumber_CI2, 5] && NeS_results_new[rownumber_CI1, 5] <= NeS_results_new[rownumber_CI2, 6]) {
              overlapping_rows <- c(overlapping_rows, rownumber_CI1, rownumber_CI2)
            }
          }
        }
      }
    }
    # tell how many unique overlapping rows exist
    overlapping_rows <- unique(overlapping_rows)
    num_overlaps <- length(overlapping_rows)
    # How many CIs are not overlapping? 
    no_overlap_para_CI_NeS <- nrow(NeS_results_new) - num_overlaps
    # [%]
    no_overlap_para_CI_NeS <- no_overlap_para_CI_NeS/nrow(NeS_results_new) *100
    
    # Calculation for percentile CI:
    # inizialize an empty vector for the row names 
    overlapping_rows <- c()
    # compare CIs of all sites with each other
    for (rownumber_CI1 in 1:nrow(NeS_results_new)) {
      for (rownumber_CI2 in 1:nrow(NeS_results_new)) {
        # ignore the confidence interval if one of the interval limits extends to infinity
        if (!is.infinite(NeS_results_new[rownumber_CI1, 3]) |
            !is.infinite(NeS_results_new[rownumber_CI1, 4]) |
            !is.infinite(NeS_results_new[rownumber_CI2, 3]) |
            !is.infinite(NeS_results_new[rownumber_CI2, 4]) 
        ) {
          # Check if the row number of the CIs compared is not equal
          if (rownumber_CI2 != rownumber_CI1) {  
            # if an overlap was found, remember the row name
            # to have an overlap CI1_end needs to be >= CI2_start and CI1_start <= CI2_end
            # start = lower end, column 3
            # end = upper end, column 4
            if (NeS_results_new[rownumber_CI1, 4] >= NeS_results_new[rownumber_CI2, 3] && NeS_results_new[rownumber_CI1, 3] <= NeS_results_new[rownumber_CI2, 4]) {
              overlapping_rows <- c(overlapping_rows, rownumber_CI1, rownumber_CI2) 
            }
          }
        }
      }
    }
    # tell how many unique overlapping rows exist
    overlapping_rows <- unique(overlapping_rows)
    num_overlaps <- length(overlapping_rows)
    # How many CIs are not overlapping? 
    no_overlap_perc_CI_NeS <- nrow(NeS_results_new) - num_overlaps
    # [%]
    no_overlap_perc_CI_NeS <- no_overlap_perc_CI_NeS/nrow(NeS_results_new) *100
    
    NeS_results_new$no_overlap_perc_CI_NeS <- no_overlap_perc_CI_NeS
    NeS_results_new$no_overlap_para_CI_NeS <- no_overlap_para_CI_NeS
    
    # add the results
    NeS_results <- rbind(NeS_results, NeS_results_new)
    
    # Calculate how many sites were removed from error messages 
    # specify location were information is saved
    NeS_output_folder <- paste("Results", describe_results_variable, "/NeSpeed/input_files/", sep = "", quote = "")
    NeS_txt_files <- list.files(path = NeS_output_folder, pattern = "\\.txt$", full.names = TRUE)
    # Count how many files do not contain errors
    no_errors_counter <- 0
    for(NeS_txt_file in NeS_txt_files) {
      # remember the name of the site
      site_name <- sub("\\.txt$", "", basename(NeS_txt_file))
      # read in the  file
      NeS_output <- readLines(NeS_txt_file)
      # If this line "Estimates with Weir's (1979) S/(S-1) weighting of r^2 for bias correction." is present, count the file.
      if (length(grep("Estimates with Weir's", NeS_output)) > 0) {
        no_errors_counter <- no_errors_counter + 1
      } 
    }
    errors_this_datasets <- data.frame(
      errors_in_datasets = nrow(AMOVA) - no_errors_counter,
      micro_or_SNP = micro_or_SNP,
      describe_results_variable = describe_results_variable,
      size_data_sets = nrow(AMOVA)
    )
    
    NeS_errors_all_datasets <- rbind(NeS_errors_all_datasets, errors_this_datasets)
    
  } else {
    errors_this_datasets <- data.frame(
      errors_in_datasets = nrow(AMOVA),
      micro_or_SNP = micro_or_SNP,
      describe_results_variable = describe_results_variable, 
      size_data_sets = nrow(AMOVA)
    )
    NeS_errors_all_datasets <- rbind(NeS_errors_all_datasets, errors_this_datasets)
  }
  
  ############ add Ne Estimator data ###############################
  if (file.exists(paste("Results", describe_results_variable, "/NeEstimator/Ne_results.csv", sep = "", quote = "")  )) {
    # Load the Ne table into R (Ne-Estimator)
    Ne_results_new <- read.csv(paste("Results", describe_results_variable, "/NeEstimator/Ne_results.csv", sep = "", quote = ""))
    Ne_results_new$describe_results_variable <- describe_results_variable
    Ne_results_new$micro_or_SNP <- micro_or_SNP
    
    # Overlaps must be calculated for every data set individually otherwise its nonsense
    
    # Calculate how many CIs do not overlap with other CIs excluding CIs = Inf [%]
    # Calculation for parametric CI:
    # inizialize an empty vector for the row names 
    overlapping_rows <- c()
    # compare CIs of all sites with each other
    for (rownumber_CI1 in 1:nrow(Ne_results_new)) {
      for (rownumber_CI2 in 1:nrow(Ne_results_new)) {
        # ignore the confidence interval if one of the interval limits extends to infinity
        if (!is.infinite(Ne_results_new[rownumber_CI1, 5]) |
            !is.infinite(Ne_results_new[rownumber_CI1, 6]) |
            !is.infinite(Ne_results_new[rownumber_CI2, 5]) |
            !is.infinite(Ne_results_new[rownumber_CI2, 6]) 
        ) {
          # Check if the row number of the CIs compared is not equal
          if (rownumber_CI2 != rownumber_CI1) {  
            # if an overlap was found, remember the row name
            # to have an overlap CI1_end needs to be >= CI2_start and CI1_start <= CI2_end
            # start = lower end, column 5
            # end = upper end, column 6
            if (Ne_results_new[rownumber_CI1, 6] >= Ne_results_new[rownumber_CI2, 5] && Ne_results_new[rownumber_CI1, 5] <= Ne_results_new[rownumber_CI2, 6]) {
              overlapping_rows <- c(overlapping_rows, rownumber_CI1, rownumber_CI2)  
            }
          }
        }
      }
    }
    # tell how many unique overlapping rows exist
    overlapping_rows <- unique(overlapping_rows)
    num_overlaps <- length(overlapping_rows)
    # How many CIs are not overlapping? 
    no_overlap_para_CI_NeE <- nrow(Ne_results_new) - num_overlaps
    # [%]
    no_overlap_para_CI_NeE <- no_overlap_para_CI_NeE/nrow(Ne_results_new) *100
    
    # Calculation for percentile CI:
    # inizialize an empty vector for the row names 
    overlapping_rows <- c()
    # compare CIs of all sites with each other
    for (rownumber_CI1 in 1:nrow(Ne_results_new)) {
      for (rownumber_CI2 in 1:nrow(Ne_results_new)) {
        # ignore the confidence interval if one of the interval limits extends to infinity
        if (!is.infinite(Ne_results_new[rownumber_CI1, 3]) |
            !is.infinite(Ne_results_new[rownumber_CI1, 4]) |
            !is.infinite(Ne_results_new[rownumber_CI2, 3]) |
            !is.infinite(Ne_results_new[rownumber_CI2, 4]) 
        ) {
          # Check if the row number of the CIs compared is not equal
          if (rownumber_CI2 != rownumber_CI1) {  
            # if an overlap was found, remember the row name
            # to have an overlap CI1_end needs to be >= CI2_start and CI1_start <= CI2_end
            # start = lower end, column 3
            # end = upper end, column 4
            if (Ne_results_new[rownumber_CI1, 4] >= Ne_results_new[rownumber_CI2, 3] && Ne_results_new[rownumber_CI1, 3] <= Ne_results_new[rownumber_CI2, 4]) {
              overlapping_rows <- c(overlapping_rows, rownumber_CI1, rownumber_CI2)  
            }
          }
        }
      }
    }
    # tell how many unique overlapping rows exist
    overlapping_rows <- unique(overlapping_rows)
    num_overlaps <- length(overlapping_rows)
    # How many CIs are not overlapping? 
    no_overlap_perc_CI_NeE <- nrow(Ne_results_new) - num_overlaps
    # [%]
    no_overlap_perc_CI_NeE <- no_overlap_perc_CI_NeE/nrow(Ne_results_new) *100
    
    
    Ne_results_new$no_overlap_perc_CI_NeE <- no_overlap_perc_CI_NeE
    Ne_results_new$no_overlap_para_CI_NeE <- no_overlap_para_CI_NeE
    
    
    # add the results
    Ne_results <- rbind(Ne_results, Ne_results_new)
  } 
  
}

############## Calculate the results for SNP, microsatellites and both data forms combined ##############

microsatellite_NeS_results <-  NeS_results[NeS_results$micro_or_SNP == "microsatellite", ]
SNP_NeS_results <- NeS_results[NeS_results$micro_or_SNP == "SNP", ]

microsatellite_Ne_results <-  Ne_results[Ne_results$micro_or_SNP == "microsatellite", ]
SNP_Ne_results <- Ne_results[Ne_results$micro_or_SNP == "SNP", ]

name_calculation <- c("combined" , "microsatellite", "SNP")

for (Index in 1:3) {
  
  if (name_calculation[Index] == "combined") {
    NeS_results_calculated <- NeS_results
    Ne_results_calculated <- Ne_results
    relevant_data_sets <- describe_results_variables
    
  } else if (name_calculation[Index] == "microsatellite") {
    NeS_results_calculated <- microsatellite_NeS_results
    Ne_results_calculated <- microsatellite_Ne_results
    relevant_data_sets <- microsatellite_data_sets
    
  } else if (name_calculation[Index] == "SNP") {
    NeS_results_calculated <- SNP_NeS_results
    Ne_results_calculated <- SNP_Ne_results
    relevant_data_sets <- SNP_data_sets
    
  }

  
  ############# calculate SpeedNe final results ###############################
  
  # calculate the median of al Ne estimates
  NeS <- median(NeS_results_calculated$Ne, na.rm = TRUE)
  # how many Ne could not be calculated [%]
  size_relevant_data_sets <- NeS_errors_all_datasets$size_data_sets[NeS_errors_all_datasets$describe_results_variable %in% relevant_data_sets]
  errors_relevant_data_sets <- NeS_errors_all_datasets$errors_in_datasets[NeS_errors_all_datasets$describe_results_variable %in% unique(relevant_data_sets)]
  
  NeS_errors_percent <-sum(errors_relevant_data_sets)/sum(size_relevant_data_sets) *100
  # This was calculated once for all data forms combined. (not tested if the code works for SNP or microsatellites only)
  # # Are there wrong results shown in Speed Ne without error messages?
  # for (describe_results_variable in describe_results_variables) {
  #   # difference_between_sites = all sites in data set - sites remaining
  #   difference_between_sites <- NeS_errors_all_datasets[NeS_errors_all_datasets$describe_results_variable == describe_results_variable, "size_data_sets"] - nrow(NeS_results_calculated[NeS_results_calculated$describe_results_variable == describe_results_variable, ])
  #   differnce_between_sites_not_caused_by_errors <- difference_between_sites - NeS_errors_all_datasets[NeS_errors_all_datasets$describe_results_variable == describe_results_variable, "errors_in_datasets"]
  #   if (!(differnce_between_sites_not_caused_by_errors=0)) {
  #     cat(differnce_between_sites_not_caused_by_errors) # "is the amount of false calculations in Speed Ne without error messages."
  #   }
  # }
  # There are no wrong results! If something went wrong the results are not in the output file.
  # Within the genetic_D&D__Speed_Ne_structure_results.R Script infinite Ne, missing Ne, or negative Ne would be removed if there were these wrong results in the line of the output that is relevant. Since nothing was deleted both remaining NeS results and no error messages have the same length. => Are 0 after substraction!
  
  # calculate the size of the confidence intervals
  NeS_results_calculated <- NeS_results_calculated %>%
    mutate(difference_percentile_CI = upper_97_5_interval_percentile - lower_97_5_interval_percentile)
  NeS_results_calculated <- NeS_results_calculated %>%
    mutate(difference_parametric_CI = upper_97_5_interval_parametric - lower_97_5_interval_parametric)
  
  # Calculate the median of CIs without Inf
  CI_percentile_NeS <- median(NeS_results_calculated$difference_percentile_CI[!is.infinite(NeS_results_calculated$difference_percentile_CI)], na.rm= TRUE)
  CI_parametric_NeS <- median(NeS_results_calculated$difference_parametric_CI[!is.infinite(NeS_results_calculated$difference_parametric_CI)], na.rm= TRUE)
  
  # Calculate how many Inf in %
  if ("Inf" %in% NeS_results_calculated$difference_parametric_CI) {
    inf_count_NeS_para <- sum(is.infinite(NeS_results_calculated$difference_parametric_CI)) + sum(is.na(NeS_results_calculated$difference_parametric_CI))
    perc_inf_para_NeS <- inf_count_NeS_para / nrow(NeS_results_calculated) * 100
  } else {
    perc_inf_para_NeS <- 0
  }
  if ("Inf" %in% NeS_results_calculated$difference_percentile_CI) {
    inf_count_NeS_perc <- sum(NeS_results_calculated$difference_percentile_CI == "Inf")
    perc_inf_perc_NeS <- inf_count_NeS_perc / nrow(NeS_results_calculated) * 100
  } else {
    perc_inf_perc_NeS <- 0
  }
  
  # Calculate the mean of CIs overlapping [%] of all datasets 
  # for percentile CIs: 
  # extract the CIs
  dublicated_no_overlap_perc_CIs_NeS <- data.frame(NeS_results_calculated$describe_results_variable, NeS_results_calculated$no_overlap_perc_CI_NeS)
  # free them from unnessercary information
  no_overlap_perc_CIs_NeS <- dublicated_no_overlap_perc_CIs_NeS[!duplicated(dublicated_no_overlap_perc_CIs_NeS[,1]),]
  # calculate mean
  no_overlap_perc_CI_NeS <- mean(no_overlap_perc_CIs_NeS$NeS_results_calculated.no_overlap_perc_CI_NeS)
  
  # for parametric CIs: 
  # extract CIs:
  dublicated_no_overlap_para_CIs_NeS <- data.frame(NeS_results_calculated$describe_results_variable, NeS_results_calculated$no_overlap_para_CI_NeS)
  # free them from unnessercary information
  no_overlap_para_CIs_NeS <- dublicated_no_overlap_para_CIs_NeS[!duplicated(dublicated_no_overlap_para_CIs_NeS[,1]),]
  # calculate mean
  no_overlap_para_CI_NeS <- mean(no_overlap_para_CIs_NeS$NeS_results_calculated.no_overlap_para_CI_NeS)
  
  
  
  ############## Ne Estimator ########################

  # Calculate the median of Ne
  NeE <- median(Ne_results_calculated$Ne, na.rm = TRUE)
  
  # Wrong results:
  size_relevant_data_sets <- NeS_errors_all_datasets$size_data_sets[NeS_errors_all_datasets$describe_results_variable %in% relevant_data_sets]
  difference_between_sites <- sum(size_relevant_data_sets) - nrow(Ne_results_calculated)
  NeE_wrong_percent <-  difference_between_sites/sum(size_relevant_data_sets) *100
  
  # calculate the size of the confidence intervals
  Ne_results_calculated <- Ne_results_calculated %>%
    mutate(difference_percentile_CI = upper_jackknife_confidence_interval - lower_jackknife_confidence_interval)
  Ne_results_calculated <- Ne_results_calculated %>%
    mutate(difference_parametric_CI = upper_parametric_confidence_interval - lower_parametric_confidence_interval)
  
  # Calculate the median of CIs without Inf
  CI_percentile_NeE <- median(Ne_results_calculated$difference_percentile_CI[!is.infinite(Ne_results_calculated$difference_percentile_CI)])
  CI_parametric_NeE <- median(Ne_results_calculated$difference_parametric_CI[!is.infinite(Ne_results_calculated$difference_parametric_CI)])
  
  # Calculate how many Inf in %
  if ("Inf" %in% Ne_results_calculated$difference_parametric_CI) {
    inf_count_NeE_para <- table(Ne_results_calculated$difference_parametric_CI == "Inf")[["TRUE"]]
    perc_inf_para_NeE <- inf_count_NeE_para/ nrow(Ne_results_calculated) *100
  } else {
    perc_inf_para_NeE <- 0
  }
  if ("Inf" %in% Ne_results_calculated$difference_percentile_CI) {
    inf_count_NeE_perc <- table(Ne_results_calculated$difference_percentile_CI == "Inf")[["TRUE"]]
    perc_inf_perc_NeE <- inf_count_NeE_perc/ nrow(Ne_results_calculated) *100
  } else {
    perc_inf_perc_NeE <- 0
  }
  
  # Calculate the mean of CIs overlapping [%] of all datasets 
  # for percentile CIs: 
  # extract the CIs
  dublicated_no_overlap_perc_CIs_Ne <- data.frame(Ne_results_calculated$describe_results_variable, Ne_results_calculated$no_overlap_perc_CI_NeE)
  # free them from unnessercary information
  no_overlap_perc_CIs_Ne <- dublicated_no_overlap_perc_CIs_Ne[!duplicated(dublicated_no_overlap_perc_CIs_Ne[,1]),]
  # calculate mean
  no_overlap_perc_CI_NeE <- mean(no_overlap_perc_CIs_Ne$Ne_results_calculated.no_overlap_perc_CI_NeE)
  
  # for parametric CIs: 
  # extract CIs:
  dublicated_no_overlap_para_CIs_Ne <- data.frame(Ne_results_calculated$describe_results_variable, Ne_results_calculated$no_overlap_para_CI_NeE)
  # free them from unnessercary information
  no_overlap_para_CIs_Ne <- dublicated_no_overlap_para_CIs_Ne[!duplicated(dublicated_no_overlap_para_CIs_Ne[,1]),]
  # calculate mean
  no_overlap_para_CI_NeE <- mean(no_overlap_para_CIs_Ne$Ne_results_calculated.no_overlap_para_CI_NeE)
  
  
  
  ########## save results in a table ###########################
  # Create results table:
  row_names <- c("median_Ne", 
                 "Ne_missing[%]",
                 "median_paraCI",
                 "Infnumber_paraCI[%]", 
                 "not_overlapping_paraCI[%]",
                 "median_percCI",
                 "Infnumber_percCI[%]",
                 "not_overlapping_percCI[%]")
  
  
  comparison <- data.frame(
    NeEstimator = c(round(NeE),
                    round(NeE_wrong_percent), 
                    round(CI_parametric_NeE), 
                    round(perc_inf_para_NeE), 
                    round(no_overlap_para_CI_NeE), 
                    round(CI_percentile_NeE), 
                    round(perc_inf_perc_NeE), 
                    round(no_overlap_perc_CI_NeE)),
    SpeedNe = c(round(NeS),
                round(NeS_errors_percent), 
                round(CI_parametric_NeS), 
                round(perc_inf_para_NeS), 
                round(no_overlap_para_CI_NeS), 
                round(CI_percentile_NeS), 
                round(perc_inf_perc_NeS), 
                round(no_overlap_perc_CI_NeS))
  )
  rownames(comparison) <- row_names
  
  # give every result table its own name (comparison_combined, comparison_microsatellite, comparison_SNP)
  table_name <- paste("comparison", name_calculation[Index], sep = "_")
  assign(table_name, comparison)
  
  
}

comparison_combined
comparison_microsatellite
comparison_SNP 

# only missing Ne differ between microsatllites and SNP. 
# For all other measurements both data set forms can be summarized.



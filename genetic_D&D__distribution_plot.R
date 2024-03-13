# combine several plots as one plot
par(mfrow = c(3, 3))

# microsatellite results
AMOVA <- c(10.4332773116146,4.74756779826703,1.9536753845165,3.00534703183226,0.72128314897334,4.18634090136504,2.93898999725331,2.15375736700932,1.54503615031007,0.886614136653729,3.50211737213986,6.62652315312839,7.48092013529428,10.7883093958614,11.6859551042122,3.65853739372199,4.71905492144927,9.22500765824151,9.19969199693997,5.36560734564278,4.3879773810838,11.2431049993073,12.7832117642991,7.24108687637913,3.55611643511354,2.20172003153003,3.37596403028123,2.75155985626684,0.393352253858525,0.283203981937455,2.37087859952632,3.13725677020606,1.93555199938328,4.39176896434126,2.42413385647205,1.48648443406424,0.481295347938726,1.45307483608598,1.65386737979627,1.34116818261478,4.44879049433132,2.50757072026639,1.5606917826638,0.516977281739591,1.96540489192251,0.507919473357942,6.2034498216584,1.51270532148083,0.884324966178915,1.29531954497135,2.61870584599216,0.398126568718713,5.07285006226606,3.38271845638082,15.2462037871365, 11.5982336942581, 13.4549928362385,10.7122611344041,16.4258687290238,25.6135902837315,21.2949278399961,18.255555060582,21.4393372542251,14.3725351548835,15.2530987303258, 18.4235023974083,17.1230453042329,9.23868029050983,13.8558602825155,16.0896539703479,17.1576657118756,21.9503132665706,19.8305285531886,18.4278943561353,24.8363206453386,23.5482326850613,25.3787676870191,12.8384415555781,28.4595151661358,15.7539710386032,14.9949880786095,19.973517268676,12.060014229028,19.5801810963358,25.3988811825528,23.6774616506834, 23.5187642431605,12.6804471488537,12.8384415555781,28.6841642938825,10.0004951923006,7.91003727309303, 6.76147816942498,6.5284059890499,15.5117455395138,27.3293471115335,8.52249265019362,12.699088447299,16.1048215077218,12.7836913583728, 9.13437276390286, 6.41894432145713, 5.05508397385438, -0.0247346266180414, -0.268005484756503, 0.798583079788704, -0.103073832773916, 0.926095542854737, 0.0953251849091775, 0.119831999607447, 0.436632726710118, 0.628597786057657, 0.636697968870795, -0.031532696350093, 0.230172772624434, 0.113816464477209, 0.133829829125024, 0.139022959239958, -0.337805666050223, 0.159480576840664, 0.395518554346523, 0.378516715826625, 0.347691675541966, 0.109133062914674, 6.37398444546242, 4.96444572643361, 6.90614738129199, 9.70807837036084,11.6793273522311,7.38563633844831,22.9952192427379, 22.0925012637534, 22.5818498305884,20.6674572378165,16.3339654857097,6.23262059876992,6.82538039238587,7.16354306831499,5.92437853920567,18.3855786391746,8.84966001520781,9.36327486160017,6.80834348450878,3.45981808437768,7.16437544694044,16.1957377231802,5.87038953648337, 5.62130710544518, 21.1788518335594,5.59889185746283,8.40818382519256,5.51119053215515,15.4137454067804,8.14708822903665,10.8360802989031,15.4113925267637,6.42018617435401,10.2697812100827,7.33419881132605,11.5115154261834,8.80126494901567,3.99928480571724,7.4777074490646,12.257700095864,6.97412836974908,8.5106967513063,7.1061701600267,1.09634023765228,0.824074633236944,1.60763699742476,1.19463190465732,0.268279200127232,1.44042169729194,2.99941730510697,1.69887063520984,1.39365604548707,1.04175958701217,0.987708851398046,3.22167349181695,0.536172564376822,1.26382544710957,1.17768846261702,1.17922067031394,1.20997578367503,3.34584921498415,1.2300017810165,11.924786694519,15.1286874870731, 12.8774954595699,12.5779885376445,2.96681874656336,13.5971192476877,12.686260935653,5.97169771649267, 10.5111940687469, 4.77130188601197,10.1554393145283,14.0139946940823)
EcoSim <- c(0.92353,0.94314,0.8036,0.88674,0.98366,0.90662,0.95598,0.91704,0.99115,0.97473,0.93132,0.87071,0.96889,0.94322,0.86852,0.86843,0.83935,0.99434,0.98141,0.95461,0.88684,0.98142,0.90635,0.96914,0.87125,0.96451, 0.96558,0.96939,0.9664,0.88592,0.92768,0.94053,0.99182,0.99589,0.96051,0.97548,0.99808,0.99645,0.95347,0.95164,0.97848,0.9896,0.97723,0.99551,0.9901,0.98666,0.99549,0.99458,0.97613,0.97136,0.96492,0.99059,0.99668,0.93904,0.97443, 0.9989, 0.99097,0.9807,0.96326,0.98474,0.97471,0.88209,0.96717,0.96646,0.98626,0.99575,0.98854,0.97153,0.99654,0.97855,0.98828,0.94119,0.99942,0.99596,0.98565,0.99897,0.98918,0.95654,0.98935,0.97955,0.96365,0.99056,0.99162,0.94912,0.99351,0.99702,0.9884,0.94239,0.95612,0.98172,0.99384,0.95219,0.90858,0.92943,0.78013,0.78601,0.8916,0.74424,0.82065,0.93445,0.92596,0.94335,0.94018,0.95094,0.98365,0.99597,0.99655,0.92032,0.96432,0.93711,0.96448,0.9959,0.98776,0.93811,0.99156,0.9981,0.98833,0.98928,0.99194,0.95019,0.9662,0.89182,0.9018,0.89524,0.94904,0.76923,0.91814,0.8515,0.88788,0.99762,0.97032,0.99137,0.92778,0.88918,0.99934,0.89805,0.88422,0.97401,0.98754,0.9928,0.9995,0.96655,0.99175,0.98457,0.99934,0.99947,0.99084,0.98429,0.95549,0.96474,0.99832,0.99737,0.99828,0.99135,0.95,0.97377,0.99824,0.97373,0.99973,0.98474,0.96279,0.99861,0.98446,0.99945,0.99974,0.99839,0.98473,0.98668,0.92294,0.98751,0.89044,0.90513,0.9094,0.91525,0.91403,0.98723,0.9146,0.90044,0.90845,0.98042,0.89185,0.91507,0.98069,0.98,0.89478,0.99495,0.96288,0.87529,0.91555,0.92629,0.92712,0.88893,0.96394,0.83172,0.80785,0.95652,0.80479,0.97354)
#plot(AMOVA,EcoSim)
# SNP results
E <- c()
SNP_names <- c("Abies_alba.xlsx", "Argiope_bruennichi.xlsx" ,"Atriophallophorus_winterbourni.xlsx",  "Dracocephalum_ruyschiana.xlsx", "Entosphenus_tridentatus.xlsx", "Frangula_alnus.xlsx", "Gadus_morhua.xlsx", "Melitaea_cinxia.xlsx", "Nilparvata_lugens.xlsx", "Oncorhynchus_mykiss.xlsx", "Oncorhynchus_tshawytscha.xlsx", "Physeter_macrocephalus.xlsx", "Pinus_halepensis.xlsx", "Salmo_trutta.xlsx", "Tectona_grandis.xlsx")
for (SNP_name in SNP_names) {
  file_path <- SNP_name 
  # This code helps to structure the results, especially if you have several data sets.
  describe_results_variable <- paste("_", tools::file_path_sans_ext(file_path), sep = "")
  Overlaps_results <- read.csv(paste("Results", describe_results_variable, "/Overlaps/Overlaps_results.csv", sep = ""))
  E <- c(E, Overlaps_results$observed_overlap)  
}
#remove outliers
#E <- E[-(9:10)]
# AMOVA SNP length(146)
A <- c( 7.30909410581387, 7.22697111427076,12.1103274234564,2.97155140087321,2.42505965060285,13.066073133641,4.69781251241423,1.72600927548033, 79.0885728451194, 40.2828179764773,2.33767234192315, 0.22711816414339, 1.58696594618478,1.42536624062374,4.61375759480407,2.79616337678633,5.4718811026349, 0.307549492946653, 0.421256968954146, 3.186019369047, 0.582324786145305, 0.703647151818918, 1.11618441759192,0.26015819077413,3.70232854545235,3.75341057021645,0.462309362456244,0.378159255182787,15.1873803643758,0.489626059313283,2.77464456923612,0.993022715311304,0.747949373012877,0.238544144497008, 6.57571425577465, 7.11965797805693, 6.26471592021161, 2.61754968493324, 3.42014531543782, 2.81050719898017, 0.595057589924735,11.7623737797293,0.42815292120516,0.609618456874669,0.128177239883405,0.344239171696365,1.80682307681684,3.47896851692209,1.30434115788323,0.597747027852427,0.779005464746937,0.315465261042345,0.150943023143932,0.414436385195564,0.869051274258407,-0.000317645220756444,0.117063224140621, 1.20309092394517,0.285754683108525, 8.83125929173721,6.47869625162448,17.5675159677149,13.7004717971038,0.150900335372305,4.18375526033293,5.08435258774867,0.584991588227718,0.456256063807391,0.209441234923221,0.301686864743133,0.623733195194171,0.745719446615999,0.390220123216881,0.90951193588641,0.976802299872381,0.62509982352786,5.06218358518282,6.75296286338522,1.48267789148037,14.5973534362971,3.32639424228111,5.67007709573407,3.14257541638862,5.64520787067718,0.0421684789474783, 0.783006725156851,0.641597995993399,0.381474073257617,0.622557484389811,3.45756049056777,0.437741367862861,2.46913154235739,1.05685418197282,23.0293979251689,1.62283158578719,1.00269554289293,0.650397406942982,17.1923523753097, 13.1849612734537, 11.3781165668357,18.6491990059337,19.9397133210884,21.9261482694858,24.9537714614656,11.4497187081817,11.9105062524756,11.8648379377714,11.8949493897238,12.0760269569829,10.6581739165532,9.13752812082899,11.0725667227257,13.4951551984084,10.9518763431986,21.462682682054,21.1351554162705,20.4944408752448,15.8207765165524,11.350631534358,10.2959439427427,20.4868515317469,12.307348652015,11.5336814791055,2.43759313174115,2.21220312695617,5.63904877364662,2.4631855850997,4.60593188453963,1.8975977877733,1.25475544900116,2.13595314539973,2.5848944455047,6.70618213281742,1.73570524679247,2.67910529975231,2.08160757215864,2.22887025869621,2.09962251053998,2.63408121116366,1.5410806007475,0.939942145834492,1.67324046633203,11.4647064072993,0.891384215286066,6.76423704567736,1.53288509003086)
#remove outliers
#A <- A[-(9:10)]
#plot(A,E)

AMOVA <- c(AMOVA, A)
EcoSim <- c(EcoSim, E)

plot(AMOVA[-(207:208)],
     EcoSim[-(207:208)],
     xlab = "AMOVA",
     ylab = "EcoSim", 
     cex.lab = 1.2,               
     cex.axis = 1)

######### plots with lamda

lamda <- c()
#plot(AMOVA,EcoSim)
all_names <- c("Ambystoma_bishopi.xlsx","Avicennia_marina.xlsx","Cameraria_ohridella.xlsx", "Carcinus_meanas.xlsx", "Cercidiphyllum_japonicum.xlsx", "Cymodocea_nodosa.xlsx", "Cystoseira_amentaceae.xlsx", "Euphydryas_aurina.xlsx", "Mytilus_galloprovincialis.xlsx", "Pagophila_eburnea.xlsx", "Panthera_leo.xlsx", "Posidonia_oceanica.xlsx", "Pyura_chilensis.xlsx", "Syncerus_caffer.xlsx", "Varroa_jacobsoni.xlsx", "Abies_alba.xlsx", "Argiope_bruennichi.xlsx" ,"Atriophallophorus_winterbourni.xlsx",  "Dracocephalum_ruyschiana.xlsx", "Entosphenus_tridentatus.xlsx", "Frangula_alnus.xlsx", "Gadus_morhua.xlsx", "Melitaea_cinxia.xlsx", "Nilparvata_lugens.xlsx", "Oncorhynchus_mykiss.xlsx", "Oncorhynchus_tshawytscha.xlsx", "Physeter_macrocephalus.xlsx", "Pinus_halepensis.xlsx", "Salmo_trutta.xlsx", "Tectona_grandis.xlsx")
for (name in all_names) {
  file_path <- name 
  # This code helps to structure the results, especially if you have several data sets.
  describe_results_variable <- paste("_", tools::file_path_sans_ext(file_path), sep = "")
  diversity_results <- read.csv(paste("Results", describe_results_variable, "/diversity/diversity_results.csv", sep = "")) 
  diversity_results <- subset(diversity_results, Pop != "Total")
  lamda <- c(lamda, diversity_results$X.N..N...1.....lambda)  
}
length(lamda)

plot(lamda,
     EcoSim,
     xlab = "λ",
     ylab= "EcoSim", 
     cex.lab = 1.2,               
     cex.axis = 1)

#plot(lamda, AMOVA, xlab = "λ", ylab = "AMOVA")

#remove outliers from AMOVA plot
#AMOVA <- AMOVA[-(207:208)]
#lamda <- lamda[-(207:208)]

plot(lamda[-(207:208)],
     AMOVA[-(207:208)],
     xlab = "λ",
     ylab = "AMOVA", 
     cex.lab = 1.2,               
     cex.axis = 1)

#############################################################################
############ Load data with Ne Estimator ####################################
#par(mfrow = c(2, 2))

all_results_with_Ne_final <- data.frame()

all_names <- c("Ambystoma_bishopi.xlsx","Avicennia_marina.xlsx","Cameraria_ohridella.xlsx", "Carcinus_meanas.xlsx", "Cercidiphyllum_japonicum.xlsx", "Cymodocea_nodosa.xlsx", "Cystoseira_amentaceae.xlsx", "Euphydryas_aurina.xlsx", "Mytilus_galloprovincialis.xlsx", "Pagophila_eburnea.xlsx", "Panthera_leo.xlsx", "Posidonia_oceanica.xlsx", "Pyura_chilensis.xlsx", "Syncerus_caffer.xlsx", "Varroa_jacobsoni.xlsx", "Abies_alba.xlsx", "Atriophallophorus_winterbourni.xlsx",  "Dracocephalum_ruyschiana.xlsx", "Entosphenus_tridentatus.xlsx", "Frangula_alnus.xlsx", "Gadus_morhua.xlsx", "Melitaea_cinxia.xlsx", "Nilparvata_lugens.xlsx", "Oncorhynchus_mykiss.xlsx", "Oncorhynchus_tshawytscha.xlsx", "Physeter_macrocephalus.xlsx", "Pinus_halepensis.xlsx", "Salmo_trutta.xlsx", "Tectona_grandis.xlsx")
for (name in all_names) {
  file_path <- name 
  # This code helps to structure the results, especially if you have several data sets.
  describe_results_variable <- paste("_", tools::file_path_sans_ext(file_path), sep = "")
  diversity_results <- read.csv(paste("Results", describe_results_variable, "/diversity/diversity_results.csv", sep = "")) 
  #diversity_results <- subset(diversity_results, Pop != "Total")
  # Load the Overlaps table into R
  Overlaps_results <- read.csv(paste("Results", describe_results_variable, "/Overlaps/Overlaps_results.csv", sep = ""))
  # Load the Amova table into R
  amova_results <- read.csv(paste("Results", describe_results_variable, "/AMOVA/amova_results.csv", sep = "", quote = ""))
  # Load the Ne table into R
  Ne_results <- read.csv(paste("Results", describe_results_variable, "/NeEstimator/Ne_results.csv", sep = "", quote = ""))   # falls diese Tabelle nicht existiert soll Ne aus den Kalkulationen weggelassen werden
  
  # Prepare data for correlation calculations with Ne:
  # For analyses including Ne rows of the other methods most likely must be removed, because it might have been impossible to calculate Ne for all sites.
  # remove information about confidence interval to keep it simple
  all_results_with_Ne <- Ne_results[, -c(3, 4, 5)]
  # create new columns for every calculation method
  all_results_with_Ne$allelic_diversity <- NA
  all_results_with_Ne$observed_overlap <- NA
  all_results_with_Ne$Difference <- NA
  # Append Ne table, add results of other calculations. 
  # Thereby unnecessary rows are removed automatically from the data set. 
  for(site in all_results_with_Ne$site) {
    all_results_with_Ne$allelic_diversity[which(all_results_with_Ne$site == site)] <- diversity_results$X.N..N...1.....lambda[which(diversity_results$Pop == site)]
    all_results_with_Ne$observed_overlap[which(all_results_with_Ne$site == site)] <- Overlaps_results$observed_overlap[which(Overlaps_results$site == site)]
    all_results_with_Ne$Difference[which(all_results_with_Ne$site == site)] <- amova_results$Difference [which(amova_results$site == site)]
  }
  all_results_with_Ne_final <- rbind(all_results_with_Ne_final, all_results_with_Ne)
}
# 
# # Ne und AMOVA
# plot(all_results_with_Ne_final$Ne, all_results_with_Ne_final$Difference, xlab = "Ne Estimator", ylab = "AMOVA")
# # Ne und EcoSim
# plot(all_results_with_Ne_final$Ne, all_results_with_Ne_final$observed_overlap, xlab = "Ne Estimator", ylab = "EcoSim")
# # Ne und lamda
# plot(all_results_with_Ne_final$Ne, all_results_with_Ne_final$allelic_diversity , xlab = "Ne Estimator", ylab = "λ")

########################################################################################
########### Load data with Ne Speed ##############################################
#par(mfrow = c(2, 2))

all_results_with_NeS_final <- data.frame()

all_names <- c("Ambystoma_bishopi.xlsx","Avicennia_marina.xlsx","Cameraria_ohridella.xlsx", "Carcinus_meanas.xlsx", "Cercidiphyllum_japonicum.xlsx", "Cymodocea_nodosa.xlsx", "Cystoseira_amentaceae.xlsx", "Mytilus_galloprovincialis.xlsx", "Pagophila_eburnea.xlsx", "Panthera_leo.xlsx", "Posidonia_oceanica.xlsx", "Pyura_chilensis.xlsx", "Syncerus_caffer.xlsx", "Varroa_jacobsoni.xlsx", "Abies_alba.xlsx","Argiope_bruennichi", "Atriophallophorus_winterbourni.xlsx",  "Dracocephalum_ruyschiana.xlsx", "Entosphenus_tridentatus.xlsx", "Gadus_morhua.xlsx", "Melitaea_cinxia.xlsx", "Oncorhynchus_mykiss.xlsx", "Physeter_macrocephalus.xlsx")
for (name in all_names) {
  file_path <- name 
  # This code helps to structure the results, especially if you have several data sets.
  describe_results_variable <- paste("_", tools::file_path_sans_ext(file_path), sep = "")
  diversity_results <- read.csv(paste("Results", describe_results_variable, "/diversity/diversity_results.csv", sep = "")) 
  #diversity_results <- subset(diversity_results, Pop != "Total")
  # Load the Overlaps table into R
  Overlaps_results <- read.csv(paste("Results", describe_results_variable, "/Overlaps/Overlaps_results.csv", sep = ""))
  # Load the Amova table into R
  amova_results <- read.csv(paste("Results", describe_results_variable, "/AMOVA/amova_results.csv", sep = "", quote = ""))
  # Load the Ne table into R
  Ne_results <- read.csv(paste("Results", describe_results_variable, "/NeEstimator/Ne_results.csv", sep = "", quote = ""))   
  NeS_results <- read.csv(paste("Results", describe_results_variable, "/NeSpeed/Ne_results.csv", sep = "", quote = ""))   
  
  
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
    # Append Ne table, add results of other calculations. 
    # Thereby unnecessary rows are removed automatically from the data set. 
    for(site in all_results_with_Ne$site) {
      all_results_with_Ne$allelic_diversity[which(all_results_with_Ne$site == site)] <- diversity_results$X.N..N...1.....lambda[which(diversity_results$Pop == site)]
      all_results_with_Ne$observed_overlap[which(all_results_with_Ne$site == site)] <- Overlaps_results$observed_overlap[which(Overlaps_results$site == site)]
      all_results_with_Ne$Difference[which(all_results_with_Ne$site == site)] <- amova_results$Difference [which(amova_results$site == site)]
      all_results_with_Ne$Ne[which(all_results_with_Ne$site == site)] <- Ne_results$Ne[which(NeS_results$site == site)]
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
      # remove rows from the loop: they should be NA
      if (nrow(all_results_with_Ne[rowSums(!is.na(all_results_with_Ne[, -1])) > 1 & rowSums(is.na(all_results_with_Ne[, -1])) > 0, ]) > 0) {
        all_results_with_Ne <- all_results_with_Ne[-which(rowSums(!is.na(all_results_with_Ne[, -1])) > 1 & rowSums(is.na(all_results_with_Ne[, -1])) > 0), ]
      }
      # Increment the index for the next run
      i <- i + 1
    }
  }
  
  all_results_with_NeS_final <- rbind(all_results_with_NeS_final, all_results_with_Ne)
}

# delete rows that include NA
all_results_with_NeS_final <- all_results_with_NeS_final %>%
  na.omit()
########################################################################################################

# create Plots

# Ne und AMOVA
plot(subset(all_results_with_Ne_final, Ne <= 2000)$Ne,  
     subset(all_results_with_Ne_final, Ne <= 2000)$Difference, 
     xlab = expression(italic("N")[italic("e")]),
     ylab = "AMOVA", 
     cex.lab = 1.2,               
     cex.axis = 1, 
     col= "darkgoldenrod")

points(subset(all_results_with_NeS_final, NeS <= 2000)$NeS, 
       subset(all_results_with_NeS_final, NeS <= 2000)$Difference, 
       col = "cadetblue4")

# define colors and labels
colors <- c("darkgoldenrod", "cadetblue4")
labels <- c("Ne Estimator", "Speed Ne")
# add legend
legend(x = "topright", legend = labels, fill = colors)


# Ne und EcoSim
plot(subset(all_results_with_Ne_final, Ne <= 800)$Ne,
     subset(all_results_with_Ne_final, Ne <= 800)$observed_overlap, 
     xlab = expression(italic("N")[italic("e")]),
     ylab = "EcoSim", 
     cex.lab = 1.2,               
     cex.axis = 1, 
     col= "darkgoldenrod", 
     lwd = 1.9) 

points(subset(all_results_with_NeS_final, NeS <= 800)$NeS,
      subset(all_results_with_NeS_final, NeS <= 800)$observed_overlap,   
      col = "cadetblue4", 
      lwd = 1.9) 

# define colors and labels
colors <- c("darkgoldenrod", "cadetblue4")
labels <- c("Ne Estimator", "Speed Ne")
# add legend
legend(x = "bottomright", legend = labels, fill = colors)



# Ne und lamda
plot(subset(all_results_with_Ne_final, Ne <= 100)$Ne,
     subset(all_results_with_Ne_final, Ne <= 100)$allelic_diversity ,
     xlab = expression(italic("N")[italic("e")]),
     ylab = "λ", 
     cex.lab = 1.2,               
     cex.axis = 1, 
     col= "darkgoldenrod", 
     xlim = c(0, 100))

points(subset(all_results_with_NeS_final, NeS <= 100)$NeS, 
     subset(all_results_with_NeS_final, NeS <= 100)$allelic_diversity,
     col = "cadetblue4")

# define colors and labels
colors <- c("darkgoldenrod", "cadetblue4")
labels <- c("Ne Estimator", "Speed Ne")
# add legend
legend(x = "bottomright", legend = labels, fill = colors)

# # NeS und AMOVA
# plot(subset(all_results_with_NeS_final, NeS <= 600)$NeS,
#      subset(all_results_with_NeS_final, NeS <= 600)$Difference,
#      xlab = "Speed Ne",
#      ylab = "AMOVA", 
#      cex.lab = 1.2,               # Schriftgröße der Achsenbeschriftungen
#      cex.axis = 1)  

# # NeS und EcoSim
# plot(subset(all_results_with_NeS_final, NeS <= 800)$NeS,
#      subset(all_results_with_NeS_final, NeS <= 800)$observed_overlap,
#      xlab = "Speed Ne",
#      ylab = "EcoSim", 
#      cex.lab = 1.2,               # Schriftgröße der Achsenbeschriftungen
#      cex.axis = 1)                # Schriftgröße der Achsenzahlen

# # NeS and lamda
# plot(subset(all_results_with_NeS_final, NeS <= 100)$NeS, 
#      subset(all_results_with_NeS_final, NeS <= 100)$allelic_diversity ,
#      xlab = "Speed Ne",
#      ylab = "λ", 
#      cex.lab = 1.2,               # Schriftgröße der Achsenbeschriftungen
#      cex.axis = 1)                # Schriftgröße der Achsenzahlen

#par(mfrow = c(1, 1))
#NeS and NeE
plot(subset(all_results_with_NeS_final, NeS <= 1000)$NeS, 
     subset(all_results_with_NeS_final, NeS <= 1000)$Ne , 
     xlab = "Speed Ne", 
     ylab = "Ne Estimator", 
     cex.lab = 1.2,               # size of axes labels
     cex.axis = 1,                # size of numbers on axis
     xlim = c(0, 200), 
     ylim = c(0, 400))               


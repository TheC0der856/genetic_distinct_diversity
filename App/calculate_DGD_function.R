calculate_DGD <- function(input, reactive_DGD_table, reactive_KBA_info) {
  req(input$file)
  file_ext <- tools::file_ext(input$file$name)
  
  # Laden der Daten basierend auf der Dateiendung
  if (file_ext == "csv") {
    df <- read.csv(input$file$datapath)
    pop <- as.vector(df[, 2])
    genind <- df2genind(df[2:ncol(df)], pop = pop, sep = ", ")
  } else if (file_ext %in% c("gen", "dat", "gtx")) {
    genind <- import2genind(input$file$datapath)
  } else if (file_ext %in% c("str", "stru")) {
    req(input$col_area, input$n_ind)
    genind <- import2genind(input$file$datapath,
                            onerowperind = FALSE, 
                            n.ind = input$n_ind, 
                            n.loc = input$n_loc, 
                            col.lab = input$col_geno, 
                            col.pop = input$col_area, 
                            ask = FALSE)
  } else {
    showNotification("Invalid file format. Please upload a valid file.", type = "error")
    return(NULL)
  }
  
  # Berechnungen durchführen
  genpop <- genind2genpop(genind)
  allele_abundances <- genpop@tab
  t_allele_abundances <- t(allele_abundances)
  taxdis <- taxa2dist(t_allele_abundances, varstep = TRUE)
  mod <- taxondive(allele_abundances, taxdis)
  
  # Ergebnisse berechnen
  sumDplus <- sum(mod$Dplus)
  ratios <- mod$Dplus / sumDplus * 100
  
  # Ergebnisse in reactive Datenrahmen speichern
  reactive_DGD_table$df_ratios <- data.frame(
    Area = names(ratios),
    `Distinct Genetic Diversity (Δ<sup>+</sup><i><sub>j</sub></i>)` = mod$Dplus,
    `Distinct Genetic Diversity (Δ<sup>+</sup><i><sub>j</sub></i>) [%]` = round(ratios, 2),
    check.names = FALSE
  )
  
  # KBA-Informationen aktualisieren
  reactive_KBA_info$A1a <- ratios[ratios >= 0.5]
  reactive_KBA_info$A1b <- ratios[ratios >= 1]
  reactive_KBA_info$A1c <- ratios[ratios >= 0.1]
  reactive_KBA_info$A1d <- ratios[ratios >= 0.2]
  reactive_KBA_info$B1  <- ratios[ratios >= 10]
}
if (!requireNamespace("vegan", quietly = TRUE)) {      
  install.packages("vegan")
}
if (!requireNamespace("adegenet", quietly = TRUE)) {   
  install.packages("adegenet")
}
if (!requireNamespace("shiny", quietly = TRUE)) {   
  install.packages("shiny")
}
if (!requireNamespace("shinyBS", quietly = TRUE)) {
  install.packages("shinyBS")
}

library("vegan")
library("adegenet")
library("shiny")
library("shinyBS")
source("calculate_DGD_function.R")


server <- function(input, output, session) {
  
  # Download csv. example: 
  # Beispiel-Daten, die in die CSV-Datei geschrieben werden
  data <- read.csv("df.csv")

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("nancycats-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  output$downloadGENETIX <- downloadHandler(
    filename = function() {
      paste("Supp_Mat_1_Pperr_24loci_3pop-", Sys.Date(), ".gtx", sep = "")
    },
    content = function(file) {
      file.copy("Supp_Mat_1_Pperr_24loci_3pop.gtx", file)  
    }
  )
  output$downloadGenpop <- downloadHandler(
    filename = function() {
      paste("Microsatellite_dataset-", Sys.Date(), ".gen", sep = "")
    },
    content = function(file) {
      file.copy("Microsatellite_dataset.gen", file)  
    }
  )
  output$downloadFstat <- downloadHandler(
    filename = function() {
      paste("138-", Sys.Date(), ".dat", sep = "")
    },
    content = function(file) {
      file.copy("138.dat", file)  
    }
  )
  output$downloadSTRUCTURE <- downloadHandler(
    filename = function() {
      paste("AllPetros122410-", Sys.Date(), ".str", sep = "")
    },
    content = function(file) {
      file.copy("AllPetros122410.str", file)  
    }
  )
  
  # Show Download-Buttons if example_button is pressed (rename!)
  observeEvent(input$show_example_files, {
    output$download_buttons <- renderUI({
      tagList(
        # margin between dowload buttons and "EXAMPLE BUTTON"
        div(style = "margin-top: 20px;"),
        
        # Buttons for files 
        div(style = "display: block;",  
            div(style = "display: flex; align-items: center; margin-bottom: 5px;",  
                downloadButton("downloadData", "Download .csv", style = "width: 150px"),
                icon("info-circle", id = "csv_info", class = "info-icon", style = "margin-left: 5px;")
            ),
            div(style = "display: flex; align-items: center; margin-bottom: 5px;",  
                downloadButton("downloadGENETIX", "Download .gtx", style = "width: 150px"),
                icon("info-circle", id = "gtx_info", class = "info-icon", style = "margin-left: 5px;")
            ),
            div(style = "display: flex; align-items: center; margin-bottom: 5px;",  
                downloadButton("downloadGenpop", "Download .gen", style = "width: 150px"),
                icon("info-circle", id = "gen_info", class = "info-icon", style = "margin-left: 5px;")
            ),
            div(style = "display: flex; align-items: center;margin-bottom: 5px;",
                downloadButton("downloadFstat", "Download .dat", style = "width: 150px"),
                icon("info-circle", id = "dat_info", class = "info-icon", style = "margin-left: 5px;")
            ), 
            div(style = "display: flex; align-items: center;margin-bottom: 5px;",
                downloadButton("downloadSTRUCTURE", "Download .str", style = "width: 150px"),
                icon("info-circle", id = "str_info_reference", class = "info-icon", style = "margin-left: 5px;"),
                icon("info-circle", id = "str_info_file_details", class = "info-icon", style = "margin-left: 5px;") # Zweiter Info-Icon
            )
           
        ),
        
        # Info texts
        bsPopover(id = "csv_info", 
                  title = "File Information", 
                  content = "This table is based on the data set nancycats included in the R package adegenet.", 
                  placement = "right", 
                  trigger = "hover"),
        bsPopover(id = "gtx_info", 
                  title = "Data Reference", 
                  content = "Salmona, Jordi; Heller, Rasmus; Quéméré, Erwan; Chikhi, Lounès (2017): Data from: Climate change and human colonization triggered habitat loss and fragmentation in Madagascar.  https://doi.org/10.5061/dryad.8f45n", 
                  placement = "right", 
                  trigger = "hover"),
        bsPopover(id = "gen_info", 
                  title = "Data Reference", 
                  content = "Millette, Katie L.; Gonzalez, Andrew; Cristescu, Melania E. (2019): Breaking ecological barriers: anthropogenic disturbance leads to habitat transitions, hybridization, and high genetic diversity. https://doi.org/10.5061/dryad.50557nm", 
                  placement = "right", 
                  trigger = "hover"), 
        bsPopover(id = "dat_info", 
                  title = "Data Reference", 
                  content = "DeFaveri, Jacquelin; Shikano, Takahito; Shimada, Yukinori; Merilä, Juha (2013): Data from: High degree of genetic differentiation in marine three-spined sticklebacks (Gasterosteus aculeatus). https://doi.org/10.5061/dryad.493jh", 
                  placement = "right", 
                  trigger = "hover"),
        bsPopover(id = "str_info_reference", 
                  title = "Data Reference", 
                  content = "Wagner, Catherine E.; McCune, Amy R.; Lovette, Irby J. (2012): Data from: Recent speciation between sympatric Tanganyikan cichlid color morphs. https://doi.org/10.5061/dryad.t6s441n0", 
                  placement = "right", 
                  trigger = "hover"),
        bsPopover(id = "str_info_file_details",  
                  title = "File Details",
                  content = "number of genotypes: 405, number of markers: 11, column containing genotypes: 1, column containing area names: 2.", 
                  placement = "right", 
                  trigger = "hover")


      )
    })
  })
  
  
  
  # Create reactive values to clear previous outputs when a new file is uploaded
  reactive_DGD_table <- reactiveValues(df_ratios = NULL)
  reactive_KBA_info <- reactiveValues(A1a = NULL, A1b = NULL, A1c = NULL, A1d = NULL, B1 = NULL)
  
  # Reactive value to control which output is displayed
  reactive_output <- reactiveValues(display = NULL)
  
  # Observe the file input and show settings for different file formats
  observeEvent(input$file, {
    req(input$file)
    
    # Clear previous outputs when a new file is uploaded
    reactive_DGD_table$df_ratios <- NULL
    reactive_KBA_info$A1a <- NULL
    reactive_KBA_info$A1b <- NULL
    reactive_KBA_info$A1c <- NULL
    reactive_KBA_info$A1d <- NULL
    reactive_KBA_info$B1  <- NULL
    reactive_output$display <- NULL  # Reset the display state
    
    file_ext <- tools::file_ext(input$file$name)
    
    if (file_ext %in% c("str", "stru")) {
      output$str_ui <- renderUI({
        tagList(
          numericInput("n_ind", label = h3("How many genotypes are there?"), value = "xxx", min = 2),
          numericInput("n_loc", label = h3("How many markers are there?"), value = "xxx", min = 1),
          numericInput("col_geno", label = h3("Which column contains labels for genotypes ('0' if absent)?"), value = "xxx", min = 0),
          numericInput("col_area", label = h3("Which column contains the area names?"), value = "xxx", min = 1)
        )
      })
    } else if (file_ext %in% c("gen", "dat", "gtx", "csv")) {
      output$str_ui <- renderUI({})
    } else if (file_ext == "structure") {
      output$str_ui <- renderUI({})
      showNotification("Invalid file format. Please upload a .str or .stru file.", type = "error")
      return(NULL)
    } else {
      output$str_ui <- renderUI({})
      showNotification("Invalid file format. Please upload a CSV, GENETIX (.gtx), Genpop (.gen), Fstat (.dat) or STRUCTURE (.str or .stru) file.", type = "error")
      return(NULL)
    }
  })
  
  # React only when the button "Display Distinct Genetic Diversity" is clicked
  observeEvent(input$display_genetic_diversity_btn, {
    req(input$file)
    
    calculate_DGD(input, reactive_DGD_table, reactive_KBA_info)
    
    reactive_output$display <- "table"  # Set to display the table
    
    output$DGD_table <- renderTable({
      reactive_DGD_table$df_ratios
    }, rownames = FALSE, colnames = TRUE, sanitize.text.function = function(x) x)
    
    output$KBA_identif <- renderText({
      NULL  # Clear KBA identif output
    })
  })
  
  # Trigger KBA identification for A1 buttons
  observeEvent(input$A1a_button, {
    calculate_DGD(input, reactive_DGD_table, reactive_KBA_info)
    reactive_output$display <- "KBA"  # Set to display KBA identif
    
    output$KBA_identif <- renderText({
      if (length(names(reactive_KBA_info$A1a)) == 0) {
        "None of the areas qualifies as KBA"
      } else {
        paste("Following areas qualify as KBA: ", paste(names(reactive_KBA_info$A1a), collapse = ", "))
      }
    })
    
    output$DGD_table <- renderTable({
      NULL  # Clear DGD table output
    })
  })
  
  observeEvent(input$A1b_button, {
    calculate_DGD(input, reactive_DGD_table, reactive_KBA_info)
    reactive_output$display <- "KBA"  # Set to display KBA identif
    
    output$KBA_identif <- renderText({
      if (length(names(reactive_KBA_info$A1b)) == 0) {
        "None of the areas qualifies as KBA"
      } else {
        paste("Following areas qualify as KBA: ", paste(names(reactive_KBA_info$A1b), collapse = ", "))
      }
    })
    
    output$DGD_table <- renderTable({
      NULL  # Clear DGD table output
    })
  })
  
  observeEvent(input$A1c_button, {
    calculate_DGD(input, reactive_DGD_table, reactive_KBA_info)
    reactive_output$display <- "KBA"  # Set to display KBA identif
    
    output$KBA_identif <- renderText({
      if (length(names(reactive_KBA_info$A1c)) == 0) {
        "None of the areas qualifies as KBA"
      } else {
        paste("Following areas qualify as KBA: ", paste(names(reactive_KBA_info$A1c), collapse = ", "))
      }
    })
    
    output$DGD_table <- renderTable({
      NULL  # Clear DGD table output
    })
  })
  
  observeEvent(input$A1d_button, {
    calculate_DGD(input, reactive_DGD_table, reactive_KBA_info)
    reactive_output$display <- "KBA"  # Set to display KBA identif
    
    output$KBA_identif <- renderText({
      if (length(names(reactive_KBA_info$A1d)) == 0) {
        "None of the areas qualifies as KBA"
      } else {
        paste("Following areas qualify as KBA: ", paste(names(reactive_KBA_info$A1d), collapse = ", "))
      }
    })
    
    output$DGD_table <- renderTable({
      NULL  # Clear DGD table output
    })
  })
  
  # Separate observeEvent for B1 button
  observeEvent(input$restricted_btn, {
    calculate_DGD(input, reactive_DGD_table, reactive_KBA_info)
    reactive_output$display <- "KBA"  # Set to display KBA identif
    
    output$KBA_identif <- renderText({
      if (length(names(reactive_KBA_info$B1)) == 0) {
        "None of the areas qualifies as KBA"
      } else {
        paste("Following areas qualify as KBA: ", paste(names(reactive_KBA_info$B1), collapse = ", "))
      }
    })
    
    output$DGD_table <- renderTable({
      NULL  # Clear DGD table output
    })
  })
  
  # Threatened species identification
  observeEvent(input$threatened_btn, {
    output$threatened_options <- renderUI({
      tagList(
        div(class = "header-with-image",
            h4("Critically endangered"),
            img(src = "CR.png", height = "25px"),
            h4("or Endangered"),
            img(src = "EN.png", height = "25px")
        ),
        actionButton("A1a_button", HTML('<div class="btn-with-image">
                                          A1a: <img src="CR.png" height="18px"/>
                                          <span style="font-size: 20px; margin: 0px;">/</span> 
                                          <img src="EN.png" height="18px"/>
                                          </div>')),
        actionButton("A1c_button", HTML('<div class="btn-with-image">
                                          A1c: <img src="CR.png" height="18px"/>
                                          <span style="font-size: 20px; margin: 0px;">/</span> 
                                          <img src="EN.png" height="18px" style="margin-right: 16px;"/>
                                          due only to past/current decline [Red List A only, but not A3 only]
                                        </div>')), 
        br(), br(),
        div(class = "header-with-image",
            h4("Vulnerable"),
            img(src = "VU.png", height = "25px")
        ),
        actionButton("A1b_button", HTML('<div class="btn-with-image">
                                        A1b:<img src="VU.png" height="18px"/></div>')),
        actionButton("A1d_button", HTML('<div class="btn-with-image">
                                        A1d:<img src="VU.png" height="18px" style="margin-right: 36px;"/>
                                          due only to past/current decline [Red List A only, but not A3 only]
                                        </div>')), 
      )
    })
    output$KBA_identif <- renderText({
      "Is your species Vulnerable or Critically endangered/Endangered?"
    }) 
    
    output$DGD_table <- renderTable({
      NULL  # Clear DGD table output
    })
  })
  
  # Dynamischer Inhalt basierend auf dem Wert von reactive_output$display
  output$main_content <- renderUI({
    if (is.null(reactive_output$display)) {
      NULL  # Keine Anzeige
    } else if (reactive_output$display == "table") {
      tableOutput("DGD_table")
    } else if (reactive_output$display == "KBA") {
      textOutput("KBA_identif")
    } else {
      NULL  # Keine Anzeige
    }
  })
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML('
      .header {
        display: flex;
        align-items: center;
        padding: 2rem;
        justify-content: space-between;
        align-items: center;
      }
      .header-with-image {
        display: flex;
        gap: 10px;
        align-items: center;
      }
      .logo {
        width: 100px; /* Größe des Logos anpassen */
        height: auto;
        margin-right: 20px; /* Abstand zwischen Logo und Titel */
      }
      .title {
        font-size: 24px; /* Größe des Titels anpassen */
      }
      .btn-with-image {
        display: flex;
        align-items: center;
        padding: 3px;
      }
      .btn-with-image img {
        margin-left: 3px; /* Bild rechts vom Text */
      }
      .btn {
        white-space: normal !important;
        text-align: left;
      }
      .info-icon {
        font-size: 16px;
        color: inherit; /* Keine blaue Farbe, übernimmt Standardtextfarbe */
        cursor: pointer;
        margin-left: 5px;
      }
    '))
  ),
  
  div(class = "header",
      div(
        img(src = "Shiny_logo.png", class = "logo"),
        div(class = "title", "Distinct Genetic Diversity"),
      ),
      div(
        #(src = "iucn.png", height = "100px"),
        img(src = "uni.png", height = "50px"),
      )
  ),
  
  fluidRow(
    column(6, 
           # Custom header with info icon next to "File input"
           div(
             h3(
               "File input", 
               span(id = "file_info", class = "info-icon", icon("info-circle"))  # Icon for popover
             )
           ),
  
           fileInput("file", label = NULL),  # No label here, as we added a custom one above
           bsPopover(id = "file_info", 
                     title = "Accepted Formats", 
                     content = "Accepted formats are: .csv, GENETIX (.gtx), Genpop (.gen), Fstat (.dat), or STRUCTURE (.str or .stru).", 
                     placement = "right", 
                     trigger = "hover"), 
           div(
             actionButton("show_example_files", "Download example files"),
             icon("info-circle", id = "download_info", class = "info-icon", style = "margin-left: 5px;") # Info icon for download button
           ),
           # Popover for download button info
           bsPopover(id = "download_info",
                     title = "File Information",
                     content = "The files are only suitable for illustrating the file format requirements, but not for identifying real KBAs.",
                     placement = "right",
                     trigger = "hover"),
           uiOutput("download_buttons") 
    ),
    column(6, 
           uiOutput("str_ui"))  # dynamic UI for structure input, will be rendered only if str is uploaded
  ),
  
  hr(),
  
  fluidRow(
    column(12, 
           verbatimTextOutput("output_info")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("display_genetic_diversity_btn", HTML("Display Distinct Genetic Diversity (Δ<sup>+</sup><i><sub>j</sub></i>)")),
      br(), br(),
      h3("Identify KBAs!"),
      actionButton("threatened_btn", "A1: Threatened species"),
      br(), br(),
      uiOutput("threatened_options"),
      br(), br(),
      actionButton("restricted_btn", "B1: Individual geographically restricted species")
    ),
    
    mainPanel(
      uiOutput("main_content")  # Use uiOutput to conditionally display content
    )
  )
)


shinyApp(ui = ui, server = server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


library(shiny)
library(here)
library(dplyr)
library(DT)
library(xgboost)
#library(kableExtra)

required_columns <- c('id', 'AWC', 'CTC', 'CVC', 'dur_min', 'meaningful_min',
                      'tv_min', 'noise_min', 'silence_min', 'distant_min')

# test data:
#raw <- read.csv(here("data_SOT_Stanford_withNAPS.csv"))

# load nap classifier
nap_dtree <- readRDS(file="models/nap_classifier.Rds")
mod <- nap_dtree$tree.model

# load CDS classifier
cds_xgb <- readRDS(file="models/final_rawLENA_xgb_model.Rds")

# select important columns and normalize to per-minute values
get_features <- function(tbl) {
  dat <- tbl %>% 
    mutate(AWC = AWC / dur_min, # per minute log(AWC+.01)
           CTC = CTC / dur_min, 
           CVC = CVC / dur_min,
           noise = noise_min / dur_min, 
           silence = silence_min / dur_min,
           distant = distant_min / dur_min,
           tv = tv_min / dur_min,
           meaningful = meaningful_min / dur_min
           #cds_ohs = ifelse(cds_ohs=="split" | cds_ohs=="nap", 0, cds_ohs), # re-classify "split" & "nap" as OHS
           ) %>%
    dplyr::select(id, AWC, CTC, CVC, noise, silence, distant, tv, meaningful)
  return(dat)
}

get_nap_predictions <- function(dat) {
  dat <- dat %>%
    mutate(nap_prob = predict(mod, dat)[,2], # probability of a segment being a nap
           nap_pred = ifelse(nap_prob > .5, 1, 0)) # binarized
  return(dat)
}

get_cds_predictions <- function(dat) {
  xdat <- xgb.DMatrix(data.matrix(dat), missing = NA)
  dat <- dat %>%
    mutate(cds_prob = predict(cds_xgb$model, xdat), # probability
           cds_pred = ifelse(cds_prob > .5, 1, 0)) # binarized
  return(dat)
}

# test
#dat <- get_features(raw)
#dat_naps <- get_nap_predictions(dat)
#dat_cds <- get_cds_predictions(dat) # Feature names stored in `object` and `newdata` are different!

# Define server logic 
function(input, output, session) {
  # now simply need to 1) validate that uploaded data has the correct column names (and data types)
  # and 2) run the model on the uploaded data, generate predictions, and make df available for download
  # optional: 3) make pretty plots
  # 4) should also write good documentation describing input and output
  
  # show classified_data tab when data is uploaded (or maybe when "Classify" button pressed?)
  observeEvent(input$dataset, {
    updateTabsetPanel(session, "tabset", selected = "classified_data")
    # showTab(inputId = "tabs", target = "Foo") # instead?
  })
  
  mydata <- reactive({
    # input$dataset will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame. The 'datapath'
    # column will contain the local filenames where the data can be found.
    inFile <- input$dataset
    
    if (is.null(inFile))
      return(NULL)
    
    tbl <- read.csv(inFile$datapath, 
                    header = input$header, 
                    sep = input$sep,  
                    dec = input$dec)
    
    return(tbl)
  })
  
  # mention if any required columns were not found
  output$validate <- renderUI({
    req(input$dataset, mydata())
    column_names <- colnames(mydata())
    print(column_names)
    
    shiny::validate(
      need(all(required_columns %in% column_names), "One or more required columns not found -- see instructions.")
    )
  })
  
  # display the normalized uploaded data -- with classifier predictions!
  output$contents <- DT::renderDataTable(
    DT::datatable({
      req(input$dataset, mydata())
      dat <- mydata() %>% select(required_columns)
      proc_dat <- get_features(dat) # style the table?
      dat_naps <- get_nap_predictions(proc_dat)
      #dat_cds <- get_cds_predictions(proc_dat) # 
      dat_naps # proc_dat # for just the uploaded data
    }) %>% formatRound(columns=c("AWC", "CTC", "CVC", "noise", "silence", "distant", "tv", "meaningful","nap_prob"), digits=1) %>%
      formatRound(columns=c("nap_prob"), digits=2) %>% # "cds_prob"
      formatRound(columns=c("nap_pred"), digits=0)
    
    )
  
  
  # only show the download button once they've uploaded data
  output$downloadData <- renderUI({
    req(input$dataset, mydata())
    downloadButton("downloadData")
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("nap_CDS_classifications", ".csv") # input$dataset
    },
    content = function(fname) {
      write.csv(mydata(), fname, row.names = FALSE)
    }
  )
  
  
  # show mean CVC/AWC/CTC etc for classifier-predicted OHS, CDS, and Naps
  output$summary_table <- DT::renderDataTable(DT::datatable({
    req(input$dataset, mydata())
    overall_stats = mydata() %>% 
      mutate(Type = case_when(
        cds_ohs=="0" ~ "Overheard Speech",
        cds_ohs=="1" ~ "Child-directed Speech",
        cds_ohs=='sleep' ~ "Sleep",
        TRUE ~ NA_character_,
      )) %>%
      group_by(Type) %>% 
      summarise(CVC=mean(CVC), 
                CTC=mean(CTC), 
                AWC=mean(AWC), 
                distant=mean(distant_min), 
                noise=mean(noise_min), 
                meaningful=mean(meaningful_min),
                tv=mean(tv_min),
                silence=mean(silence_min), N=n()) %>% 
      arrange(CVC)
    #apa_table(overall_stats, digits = 2, caption= "Means for LENA variables by category.")
  }))
  
}



# Run the application 
#shinyApp(ui = ui, server = server)

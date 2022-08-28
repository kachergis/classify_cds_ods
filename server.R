#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


library(shiny)
library(here)
library(tidyverse)
library(DT)
library(xgboost)
#library(kableExtra)

required_columns <- c('id', 'AWC', 'CTC', 'CVC', 'dur_min', 'meaningful_min',
                      'tv_min', 'noise_min', 'silence_min', 'distant_min')


# load nap classifier
nap.model <- readRDS(file="models/nap_classifier.Rds")

# load CDS classifier
cds.model <- xgb.load("models/final_rawLENA_xgb.model")

# select important columns and normalize to per-minute values
get_features <- function(raw) {
  dat <- raw %>% 
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
    mutate(nap_prob = predict(nap.model, dat)[,2], # probability of a segment being a nap
           nap_pred = ifelse(nap_prob > .5, 1, 0)) # binarized
  return(dat)
}

get_cds_predictions <- function(dat) {
  xdat <- xgb.DMatrix(data.matrix(dat %>% select(-id)), missing = NA)
  dat <- dat %>%
    mutate(cds_prob = predict(cds.model, xdat), # probability
           cds_pred = ifelse(cds_prob > .5, 1, 0)) # binarized
  return(dat)
}


# test 
run_test <- function() {
  raw <- read.csv(here("data_SOT_Stanford_withNAPS.csv"))
  dat <- get_features(raw) 
  table(raw$cds_ohs) # human raters: 427 sleep, 2028 CDS, 267 split, 768 ODS
  dat_naps <- get_nap_predictions(dat) %>% 
    mutate(segment = 1:n())
  table(dat_naps$nap_pred) # 350 sleep, 3140 awake
  dat_cds <- get_cds_predictions(dat) %>% # Feature names stored in `object` and `newdata` are different!
    mutate(segment = 1:n())
  table(dat_cds$cds_pred) # 833 ODS, 2657 CDS
  
  dat_proc <- dat_naps %>% left_join(dat_cds) %>%
    mutate(cds_pred = ifelse(nap_pred==1, NA, cds_pred))
  #table(dat_proc$nap_pred) # 350 sleep, 3140 awake
  #table(dat_proc$cds_pred) # 2606 CDS, 534 ODS
}
  
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
    req(input$dataset)
    inFile <- input$dataset
    
    if(is.null(inFile)) return(NULL)
    
    tbl <- read.csv(inFile$datapath, 
                    header = input$header, 
                    sep = input$sep,  
                    dec = input$dec)
    
    dat <- tbl %>% select(required_columns) # validate this?
    proc_dat <- get_features(dat) 
    dat_naps <- get_nap_predictions(proc_dat) %>%
      mutate(segment = 1:n())
    dat_cds <- get_cds_predictions(proc_dat) %>%
      mutate(segment = 1:n())
    dat_proc <- dat_naps %>% left_join(dat_cds) %>%
      mutate(cds_pred = ifelse(nap_pred==1, NA, cds_pred)) # if napping, then don't classify CDS/ODS
    
    return(dat_proc)
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
      mydata() %>% select(-segment)
    }, rownames= FALSE) %>% 
      formatRound(columns=c("AWC", "CTC", "CVC", "noise", "silence", "distant", "tv", "meaningful"), digits=1) %>%
      formatRound(columns=c("nap_prob","cds_prob"), digits=2) %>% 
      formatRound(columns=c("nap_pred","cds_pred"), digits=0)
    )
  
  
  # only show the download button once they've uploaded data
  output$download_button <- renderUI({
    req(input$dataset, mydata()) # 
    downloadButton("download_data", "Download Data", class = "btn-xs")
  })
  
  output$download_data <- downloadHandler(
    filename = function() "nap_CDS_classifications.csv", # input$dataset
    content <- function(fname) {
      write.csv(mydata(), fname, row.names = FALSE)
    }
    #contentType = "text/plain"
  )
  
  
  # show mean CVC/AWC/CTC etc for classifier-predicted OHS, CDS, and Naps
  output$summary_table <- DT::renderDataTable(DT::datatable({
    req(mydata())
    mydata() %>% 
      mutate(Type = case_when(
        nap_pred==1 ~ "Sleep", 
        cds_pred==0 ~ "Overheard Speech",
        cds_pred==1 ~ "Child-directed Speech",
        TRUE ~ NA_character_,
      )) %>%
      group_by(Type) %>% 
      summarise(CVC=mean(CVC), 
                CTC=mean(CTC), 
                AWC=mean(AWC), 
                distant=mean(distant), 
                noise=mean(noise), 
                meaningful=mean(meaningful),
                tv=mean(tv),
                silence=mean(silence), N=n()) %>% 
      arrange(CVC)
  }, rownames= FALSE) %>% formatRound(columns=c("AWC", "CTC", "CVC", "noise", "silence", "distant", "tv", "meaningful"), digits=2)
  # options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')) # <- download button?
  )
  
}


# Run the application 
#shinyApp(ui = ui, server = server)

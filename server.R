#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


library(shiny)
library(here)
library(tidyverse)
library(DT)
library(xgboost)
library(lubridate)
#library(kableExtra)

required_columns <- c('id', 'AWC', 'CTC', 'CVC', 'dur_min', 'meaningful_min',
                      'tv_min', 'noise_min', 'silence_min', 'distant_min')

# these can be in H:M:S (e.g. 0:05:00) or numeric minute (5.0) format
duration_columns <- required_columns[endsWith(required_columns, "_min")]

# load sleep classifier
sleep.model <- readRDS(file="models/sleep_classifier.Rds")

# load CDS classifier
cds.model <- xgb.load("models/final_rawLENA_xgb.model")

# select important columns and normalize to per-minute values
get_features <- function(raw) {
  # durations is sometimes in HMS format (LENA Pro) - convert any HMS columns to minutes
  for(c in duration_columns) {
    if(!is.numeric(raw$dur_min)) raw[,c] = as.numeric(as.duration(hms(raw[,c]))) / 60
  }
  
  dat <- raw %>% 
    mutate(AWC = AWC / dur_min, # per minute log(AWC+.01)
           CTC = CTC / dur_min, 
           CVC = CVC / dur_min,
           noise = noise_min / dur_min, # proportion of segment's duration that is noise
           silence = silence_min / dur_min,
           distant = distant_min / dur_min,
           tv = tv_min / dur_min,
           meaningful = meaningful_min / dur_min
           #cds_ods = ifelse(cds_ods=="split" | cds_ods=="nap", 0, cds_ods), # re-classify "split" & "nap" as ODS
           ) %>%
    dplyr::select(id, AWC, CTC, CVC, noise, silence, distant, tv, meaningful)
  return(dat)
}

get_sleep_predictions <- function(dat) {
  dat <- dat %>%
    mutate(sleep_prob = predict(sleep.model, dat)[,2], # probability of a segment being sleep
           sleep_pred = ifelse(sleep_prob > .5, 1, 0)) # binarized
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
  raw <- read.csv(here("data_sample_00h00m00s.csv")) # with LENA pro HMS duration format
  raw <- read.csv(here("data_SOT_Stanford_withNAPS.csv"))
  dat <- get_features(raw) 
  table(raw$cds_ohs) # human raters: 427 sleep, 2028 CDS, 267 split, 768 ODS
  dat_naps <- get_sleep_predictions(dat) %>% 
    mutate(segment = 1:n())
  table(dat_naps$sleep_pred) # 350 sleep, 3140 awake
  dat_cds <- get_cds_predictions(dat) %>% # Feature names stored in `object` and `newdata` are different!
    mutate(segment = 1:n())
  table(dat_cds$cds_pred) # 833 ODS, 2657 CDS
  
  proc_dat <- dat_naps %>% left_join(dat_cds) %>%
    mutate(cds_pred = ifelse(sleep_pred==1, NA, cds_pred))
  #table(proc_dat$sleep_pred) # 350 sleep, 3140 awake
  #table(proc_dat$cds_pred) # 2606 CDS, 534 ODS
  
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
    
    raw_dat <- read.csv(inFile$datapath, 
                    header = input$header, 
                    sep = input$sep,  
                    dec = input$dec)
    
    dat <- raw_dat %>% select(required_columns) # validate this?
    
    # want to save uploaded data, and add classification columns to it for download
    
    proc_dat <- get_features(dat) 
    
    dat_naps <- get_sleep_predictions(proc_dat) %>%
      mutate(segment = 1:n())
    dat_cds <- get_cds_predictions(proc_dat) %>%
      mutate(segment = 1:n())
    proc_dat <- dat_naps %>% left_join(dat_cds) %>%
      mutate(cds_pred = ifelse(sleep_pred==1, NA, cds_pred)) # if napping, then don't classify CDS/ODS
    
    raw_dat <- raw_dat %>%
      rename(AWC_total = AWC, 
             CTC_total = CTC, 
             CVC_total = CVC) %>%
      mutate(segment = 1:n())
    
    all_dat <- raw_dat %>% left_join(proc_dat, by=c('id','segment'))
    
    return(all_dat) # was proc_dat
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
      mydata() %>% select(id, AWC, CTC, CVC, noise, silence, distant, tv, meaningful, sleep_prob, cds_prob, sleep_pred, cds_pred)
    }, rownames= FALSE) %>% 
      formatRound(columns=c("AWC", "CTC", "CVC", "noise", "silence", "distant", "tv", "meaningful"), digits=1) %>%
      formatRound(columns=c("sleep_prob","cds_prob"), digits=2) %>% 
      formatRound(columns=c("sleep_pred","cds_pred"), digits=0)
    )
  
  
  # only show the download button once they've uploaded data
  output$download_button <- renderUI({
    req(input$dataset, mydata()) # 
    downloadButton("download_data", "Download Data", class = "btn-xs")
  })
  
  output$download_data <- downloadHandler(
    filename = function() "nap_CDS_classifications.csv", # input$dataset
    content <- function(fname) {
      write.csv(mydata() %>% 
                  select(-AWC, -CTC, -CVC, -noise, -silence, -silence, -distant, -tv, -meaningful), 
                fname, row.names = FALSE)
    }
    #contentType = "text/plain"
  )
  
  
  # show mean CVC/AWC/CTC etc for classifier-predicted ODS, CDS, and sleep
  output$summary_table <- DT::renderDataTable(DT::datatable({
    req(mydata())
    mydata() %>% 
      select(AWC, CTC, CVC, noise, silence, distant, tv, meaningful, sleep_pred, cds_pred) %>%
      mutate(Type = case_when(
        sleep_pred==1 ~ "Sleep", 
        cds_pred==0 ~ "Other-directed Speech",
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
  
  output$summary_plot <- renderPlot({
    req(mydata())
    mydata() # %>% ggplot()
      
  })
  
}

# Run the application 
#shinyApp(ui = ui, server = server)

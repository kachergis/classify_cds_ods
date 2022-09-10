library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Identifying Periods of Sleep and Child-directed Speech in Daylong LENA Recordings"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            fileInput("dataset", "Choose LENA data file to upload",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE),
            radioButtons('sep', 'Separator',
                         c(Comma=',', Semicolon=';', Tab='\t'),
                         ','),
            
            radioButtons('dec', 'Decimal seperator',
                         c('Comma (,)'=",",
                           'Point (.)'="."), '.'),
            
            uiOutput("download_button")
        ),
        
        mainPanel(
            #tableOutput("contents"),
          tabsetPanel(id = "tabset",
            tabPanel("Overview and Instructions", 
                   br(),
                   h3("Overview"),
                   p("This app will let you upload segment summaries from LENA recordings, and will assign a probability to each segment for 1) periods of sleep vs. a waking period, and 2) predominantly containing child-directed speech (CDS) vs. other-directed speech (ODS)."),
                   p("For full documentation of how the classifiers were constructed, including discussion of potential limitations to generalizability, see Bang, Kachergis, Weisleder, and Marchman (submitted)."),
                   #br(),
                   h3("Instructions"),
                   p("You will first need to export your LENA data in 5-minute segments, and then save them in a comma-/tab-separated format (see required column names below). Once you have your LENA segment data ready to upload, in the panel at left select 'Browse...' and navigate to that file."),
                   p(HTML("Once the file is uploaded, you can preview the classifications in the <i>Classified Data</i> tab, view a <i>Summary Table</i>, or download the classified data using the button in the left panel.")),
                   p("Your data will have four additional columns, denoting for each segment:"),
                   p(HTML("<b>nap_prob</b> - the classifier-predicted probability that the child was predominantly asleep during recording (i.e., nap_prob=0.95 means the child was very likely napping, while nap_prob=.05 means the child was very likely awake)")),
                   p(HTML("<b>nap_pred</b> - binarized nap_prob (1=asleep, 0=awake)")),
                   p(HTML("<b>cds_prob</b> - the classifier-predicted probability that the segment contains predominantly CDS")),
                   p(HTML("<b>cds_pred</b> - binarized cds_prob (1=CDS, 0=ODS)")),
                   p("Note that the segments in your downloaded data will be in the same order as your uploaded segments, but that the LENA speech values (AWC, CTC, CVC) will now be normalized to per-minute values (e.g., AWC/dur_min). Extra variables will be dropped."),
                   br(),
                   h4("Required Data Format"),
                   p("The following columns are required when uploading your data (extra columns will be ignored; column order is not important):"),
                   p(HTML("<b>id</b> - participant identifier")),
                   p(HTML("<b>AWC</b> - total adult word count per segment")),
                   p(HTML("<b>CTC</b> - total conversational turns count per segment")),
                   p(HTML("<b>CVC</b> - total child vocalizations count per segment")),
                   p(HTML("<b>dur_min</b> - duration of LENA segment (minutes)")),
                   p(HTML("<b>meaningful_min</b> - duration of meaningful speech (minutes)")),
                   p(HTML("<b>tv_min</b> - minutes of television per segment")),
                   p(HTML("<b>noise_min</b> - minutes of noise per segment")),
                   p(HTML("<b>silence_min</b> - minutes of silence per segment")),
                   p(HTML("<b>distant_min</b> - minutes of distant speech per segment")),
                   p(HTML("All columns except <b>id</b> should be LENA-produced numeric values."))),
            tabPanel("Classified Data", value="classified_data", type="hidden", # hidden until uploaded
                   br(),
                   #uiOutput("validate"),
                   DT::dataTableOutput("contents")),
            tabPanel("Summary Table", value="summary_table", type="hidden",
                     br(),
                     p("This table shows mean values of normalized (per-minute) segments in the classified data, split by the class (e.g., Sleep). The final column (N) indicates the number of segments identified by the classifier."),
                     p("Note: the sleep classifier is applied to the data first, and any segments that are classified as 'sleep' are excluded from CDS/ODS classification, and are given 'NA' values."),
                     br(),
                     DT::dataTableOutput("summary_table"))
          )
          #tabPanel("Classified Data", tableOutput("table")),
          #tabPanel("Plot")
            #downloadButton("Download Predictions") # should only show once they've uploaded data
        )
    )
)

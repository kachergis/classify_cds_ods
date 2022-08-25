library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Identifying Naps and Child-directed Speech in LENA Segments"),

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
            
            uiOutput("downloadData")
        ),
        
        mainPanel(
            #tableOutput("contents"),
          tabsetPanel(
            tabPanel("Overview and Instructions", 
                   p("This shiny app will let you upload segment summaries from LENA recordings, and will assign a probability to each segment of it being 1) during a child's nap vs. a waking period, and 2) predominantly containing child-directed speech (CDS) vs. other-directed speech (ODS)."),
                   h2("Required Data Format"),
                   p("The following columns are required (extra columns will be ignored; column order is not important):"),
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
            tabPanel("Uploaded Data", 
                   uiOutput("validate"),
                   DT::dataTableOutput("contents"))
          )
          #tabPanel("Classified Data", tableOutput("table")),
          #tabPanel("Plot")
            #downloadButton("Download Predictions") # should only show once they've uploaded data
        )
    )
)

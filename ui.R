library(shiny)
library(shinythemes)

# Define UI
fluidPage(
  theme = shinytheme("cerulean"),
  # Page Header
  headerPanel("Universiti Malaya Result Parser"),
        
  # Input values
  sidebarPanel(
    HTML("<h3>Input Results</h3>"),
    
    fileInput('file_input','Choose PDF Files',
              multiple = T,accept = c('.pdf')),
    
    downloadButton('download_xlsx','Download xlsx'),
    downloadButton('download_csv','Download csv')
  ),
          
  # main panel
  mainPanel(
    uiOutput("title1"),
    verbatimTextOutput('profile'),
    
    uiOutput("title2"),
    tableOutput('summary'),
    
    # uiOutput("title3"),
    plotOutput('grades'),
    
    uiOutput("title4"),
    tableOutput('table'),
  
    # keep alive
    tags$head(
      HTML(
        "
    <script>
    var socket_timeout_interval
    var n = 0
    $(document).on('shiny:connected', function(event) {
    socket_timeout_interval = setInterval(function(){
    Shiny.onInputChange('count', n++)
    }, 15000)
    });
    $(document).on('shiny:disconnected', function(event) {
    clearInterval(socket_timeout_interval)
    });
    </script>
    "
        )),
    textOutput("keepAlive")
  )
) # fluidPage
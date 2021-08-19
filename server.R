
# library
library(shiny)
library(shinythemes)
library(xlsx)
library(stringr)

# source scripts
source('parsing.R')

# Define server function
server = function(input, output) {
  observe({
    req(input$file_input)
    
    # title
    output$title1 = renderUI({
      HTML("<h4>Profile</h4>")
    })
    output$title2 = renderUI({
      HTML("<h4>Summary</h4>")
    })
    output$title4 = renderUI({
      HTML("<h4>Overall Results</h4>")
    })
    
    # read temporary datapath
    if(!dir.exists('tmp')){dir.create('tmp')}
    datapath = vector()
    for(i in 1:length(input$file_input$datapath)){
      datapath = append(datapath,paste0('tmp/input_',i,'.pdf'))
      file.copy(input$file_input$datapath[i],datapath[i], overwrite = T)
    }
    # render profile
    the_profile = c('Name','Matric No','IC/Passport No','Programme','CGPA','Credit Hours')
    the_profile = paste0(str_pad(the_profile,15,side = 'right'),':')
    user_profile = result_parser(datapath)[[1]]
    user_profile = paste0(the_profile,user_profile)
    output$profile = renderText({
      # length(user_profile)
      paste(user_profile,collapse = '\n')
    })
    # render summary table
    summary_result = result_parser(datapath)[[2]]
    output$summary = renderTable({
      summary_result
    })
    # render summary table
    grades = result_parser(datapath)[[3]]
    output$grades = renderPlot({
      barplot(grades$total,names.arg = grades$grade, col = '#40E0D0', main = 'Total Grades')
    })
    # render result table
    result = result_parser(datapath)[[4]]
    output$table = renderTable({
      result
    })
    
    
    # delete temporary data
    for(i in 1:length(datapath)){file.remove(datapath[i])}
    
    
    # download buttons
    output$download_xlsx = downloadHandler(
      filename = function(){
        'result.xlsx'
      },
      content = function(file){
        write.xlsx(result,file)
      }
    )
    output$download_csv = downloadHandler(
      filename = function(){
        'result.csv'
      },
      content = function(file){
        write.csv(result,file)
      }
    )    
    
  })
  
  # # keep alive
  # output$keepAlive <- renderText({
  #   req(input$count)
  #   paste("keep alive ", input$count)
  # })
  
} # server

# library
library(shiny)
library(shinythemes)
library(xlsx)
library(stringr)
library(pdftools)
library(stringr)
library(dplyr)

# read pdf
result_parser = function(pdfs_path){
  score_df = data.frame()
  for(i in 1:length(pdfs_path)){
    # read pdf
    pdf = pdf_text(pdfs_path[i])
    # get name
    if(i==1){
      name = str_squish(str_extract(pdf,'Name[[:print:]]{1,100}\n'))
      name = gsub('Name : ','',name)
      reg_no = str_squish(str_extract(pdf,'Registration[[:print:]]{1,100}\n'))
      reg_no = gsub('Registration No : ','',reg_no)
      ic_no = str_squish(str_extract(pdf,'NRIC[[:print:]]{1,100}\n'))
      ic_no = gsub('NRIC/Passport No : ','',ic_no)
      programme = str_squish(str_extract(pdf,'Programme[[:print:]]{1,100}\n'))
      programme = gsub('Programme : ','',programme)
      personal_details = c(name,reg_no,ic_no,programme)
    }
    # skip if different ic
    next_ic = str_squish(str_extract(pdf,'NRIC[[:print:]]{1,100}\n'))
    next_ic = gsub('NRIC/Passport No : ','',next_ic)
    if(ic_no!=next_ic){next}
    # get semester and session
    semester = str_extract(pdf,'Semester [[:digit:]]')
    semester = as.integer(str_extract(semester,'[[:digit:]]'))
    year = str_extract(pdf,'Session [[:digit:]]{4}')
    year = as.integer(str_extract(year,'[[:digit:]]{4}'))
    # get the subjects and score
    pdf = trimws(str_split(pdf,'\n')[[1]])
    pdf = grep('^[[:digit:]]',pdf,value = T)
    pdf = str_split(pdf,'  ')
    # clean text
    for(j in 1:length(pdf)){
      pdf[[j]] = pdf[[j]][!grepl('^$',pdf[[j]])]
      pdf[[j]] = str_squish(pdf[[j]])
    }
    # get the details
    course_code = unlist(lapply(1:length(pdf), function(x) pdf[[x]][2]))
    course_name = unlist(lapply(1:length(pdf), function(x) pdf[[x]][3]))
    credit_hour = unlist(lapply(1:length(pdf), function(x) pdf[[x]][4]))
    grade = unlist(lapply(1:length(pdf), function(x) pdf[[x]][5]))
    grade_point = unlist(lapply(1:length(pdf), function(x) pdf[[x]][6]))
    # clean data
    credit_hour = as.numeric(str_squish(credit_hour))
    grade_point = as.numeric(str_squish(grade_point))
    # add to dataframe
    df = data.frame(course_code = course_code, course_name = course_name,
                    credit_hour = credit_hour, grade = grade, grade_point,
                    semester = semester, year = year)
    score_df = rbind(score_df,df)
  }
  # parsing semester
  years = score_df %>% group_by(semester,year) %>% 
    summarise(gpa = round(sum(grade_point)/sum(credit_hour),2), credit_hour = as.integer(sum(credit_hour))) %>% 
    arrange(year,semester)
  years = cbind(data.frame(no = 1:nrow(years)),years)
  # add cgpa
  cgpa = round(sum(score_df$grade_point)/sum(score_df$credit_hour),2)
  personal_details = append(personal_details,cgpa)
  # add credit hours
  ch = sum(score_df$credit_hour)
  personal_details = append(personal_details,ch)
  # grades
  grades = score_df %>% group_by(grade) %>% summarise(total = n())
  grades$x = str_extract(grades$grade,'^[[:alpha:]]')
  grades$y = ifelse(grepl('\\+',grades$grade),0,1)
  grades$y = ifelse(grepl('\\-',grades$grade),2,grades$y)
  grades = grades %>% arrange(x,y) %>% select(-x,-y)
  # return data
  data = list(personal_details,years,grades,score_df)
  return(data)
}

# Define server function
shinyServer(function(input, output, session) {
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
  
  # keep alive
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })
  
}) # server

# library
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


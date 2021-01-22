#STA 141B project 3 Webscraping
#Libraries:
library(curl)
library(RCurl)
library(XML)
library(xml2)
library(rlist)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Testing Cybercoders
#Allows webscraping for personal use
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#start with data scientiest
web_url = "https://www.cybercoders.com/search/?searchterms=Data+Scientist&searchlocation=&newsearch=true&originalsearch=true&sorttype="
web_content1 = getURLContent(web_url) 
#check
grepl("Computer Vision", web_content1) #Matches with a phrase in the job postings
content_parsed = htmlParse(web_content1)
#Now, look for the posts:
first_page_listings = getNodeSet(content_parsed, "//div[@class = 'job-listing-item']") 
#first_page_listings[[1]] 

#Can get the job names:
unique(xpathSApply(first_page_listings[[1]], ".//a", xmlValue))[1] #Grabs the job title

job_names = sapply(first_page_listings, function(x)  unique(xpathSApply(x, #To get all job names
                                                                        ".//a", xmlValue))[1])
#We will need to remove NULL values or have a check in the functions
#Now, we need the URL for the job posting itself:
test_url = unique(xpathSApply(first_page_listings[[1]], ".//a[1]/@href"))[1]
sapply(first_page_listings, function(x)  unique(xpathSApply(x,
                                                      ".//a[1]/@href"))[1]) #Gets full URL for the job post
full_purl=paste0("https://www.cybercoders.com", test_url) 
ptxt = getURLContent(full_purl) #This gives the html for the page
#Now, we have to find the arrow(next page):
web_url2 = "https://www.cybercoders.com/search/?searchterms=Data+Analyst&searchlocation=&newsearch=true&originalsearch=true&sorttype="
web_content2 = getURLContent(web_url2) 
content_parsed2 = htmlParse(web_content2)
next_page = xpathSApply(content_parsed2, "//a[@class= 'get-search-results next']/@href")
#Full Next page URL:
next_page_url = getRelativeURL(next_page, 
                'https://www.cybercoders.com/search/?searchterms=Data+Analyst&searchlocation=&newsearch=true&originalsearch=true&sorttype=')

rm(list = ls()[ls() %in% c('web_url','content_parsed', 
                             'web_content1', 'first_page_listings', 'test_url',
                             'ptxt', 'full_purl', 'content_parsed2', 'first_page_listings2',
                           'job_names', 'next_page', 'web_content2', 'weburl2')])

#--------------------------------------------------------------------------------------------------------------------
#BUILD FUNCTIONS:
#Webscrape Cybercoders:
#--------------------------------------------------------------------------------------------------------------------
get_job_content = function(job_url){
  job_info_condensed = getURLContent(job_url) #Get the html file for the current page
  return(job_info_condensed)
}

get_postings = function(page_parsed){
  page_listings = getNodeSet(page_parsed, "//div[@class = 'job-listing-item']") #Listings on current page
  if(length(page_listings)==0){ #If there are no results in the output
    return(data.frame("No Results", "No Results"))
    }
  job_names_l = sapply(page_listings, function(x)  unique(xpathSApply(x, #To get all job names
                                                                    ".//a", xmlValue))[1])#Job Names
  job_urls_l = sapply(page_listings, function(x)  unique(xpathSApply(x,
                                                                     ".//a[1]/@href"))[1])#Job URLS
  if("NULL" %in% job_names_l){ #To remove null values which are ads
    job_names_l[which(job_names_l == "NULL")] = NULL
    job_urls_l[which(job_names_l == "NULL")] = NULL 
  }
  job_names = unlist(job_names_l)
  job_urls_1  =  unlist(job_urls_l)
  #job_urls_2 = gsub(".+(\\?[^=]=.+)", "\\1", job_urls_1)
  full_job_urls = sapply(job_urls_1, #Get the full job URL.
                         function(x) paste0("https://www.cybercoders.com", x)) #.com/viewjob
  job_specific_info = sapply(full_job_urls, get_job_content) #Function to get the job html pages.
  job_name_info = data.frame(job_names, job_specific_info) #Send data frame back
  return(job_name_info)
}

get_all_jobs = function(web_url, job_search){
  all_job_posts = data.frame() #Put all job posts into a data frame
  repeat{ #Repeat this chunk of code till we get to the last page
    web_content = getForm(web_url,searchterms = job_search) #Get the html content from the first page
    web_parsed = htmlParse(web_content) 
    job_posts = get_postings(web_parsed) #Send the parsed html to function for jobs
    all_job_posts = rbind(all_job_posts, job_posts) #Add page's job posts to the dataframe
    #Find the next page's URL:
    next_page = xpathSApply(web_parsed, "//a[@class= 'get-search-results next']/@href") 
    if(length(next_page) == 0){break} #If the next page URL is not found, we break.
    web_url = getRelativeURL(next_page, web_url) #Next page full URL
  }
  return(all_job_posts)
}

#Data Scientists:
coders_url = "https://www.cybercoders.com/search/"
web_content_ds = get_all_jobs(coders_url, 'data scientist')
#Statisticians:
web_content_s = get_all_jobs(coders_url, 'statistician')
#Data Analyst:
web_content_da = get_all_jobs(coders_url, 'data analyst')
#Data Engineer
web_content_de = get_all_jobs(coders_url, 'Data Engineer')


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#GET DATA FROM THE POSTS
#Use test processing techniques and 
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Logic test:
text_doc = htmlParse(web_content_da[1,2])
#test_full_form = getNodeSet(text_doc, "//div[@class = 'job-details span9']") #description in html format
test_full_text = getNodeSet(text_doc, "//div[@class = 'section-data']/text()") #Outside sections not grouped with titles
test_full_text2 =  getNodeSet(text_doc, "//h4[@class = 'section-title']/text()") #Section Titles plus preferred skills title
section_info = getNodeSet(text_doc, "//div[@data-section='5']/text()") #Sections are grouped by number 
test_full_text3 =  getNodeSet(text_doc, "//div[contains(@class, 'section-data')]/text()") #Free form data

test_location_node = getNodeSet(text_doc, 
                                "//div[@class = 'location']//span[contains(.,text())]/text()")  #location
test_salary = getNodeSet(text_doc, 
                         "//div[@class = 'wage']//span[contains(.,text())]/text()") #salary
test_salary2 = unique(xpathSApply(text_doc,
                                  "//div[@class = 'wage']//span[contains(.,text())]/text()", xmlValue))
#Conditional statement to get the employment type:
if(grepl("[^k$-]{3}",test_salary2)){
  salary_1 = gsub("[^0-9$]+(\\$*[0-9]+.+)", "\\1", test_salary2)
  employment_status_1 = gsub("([^0-9$]+)\\$*[0-9]+.+", "\\1", test_salary2)
}
#To get preferred skills:
test_prefered_skills = getNodeSet(text_doc, "//span[@class = 'skill-name']") 
unique(xpathSApply(text_doc,"//span[@class = 'skill-name']", xmlValue))

rm(list = ls()[ls() %in% c('text_doc','test_full_text', 
                             'test_full_text2', 'section_info', 'test_full_text3 ',
                             'test_salary', 'content_parsed2', 'first_page_listings2',
                             'test_full_text3', 'test_salary2', 'test_prefered_skills',
                           'salary_1','employment_status_1')])

#Create functions that process the pages and acquire the necessary info:----------------------------------------------------
#IF ISSUE WITH GETTING THE TEXT WE CAN REMOVE THE text() part from the xml commands
#Will need to clean the results.

get_description = function(html_parsed){  #This function obtains all text in job post
  job_descript = unique(xpathSApply(html_parsed,
              "//div[contains(@class, 'section-data')]/text()", xmlValue, trim= TRUE))
  if(length(job_descript) == 0){ #Check for missing data
    return("No Job Description Provided")
  }
  return(paste(job_descript, collapse = " "))
}

#-----------------------
get_job_tasks = function(html_parsed){ #This function obtains the job assignments
  job_tasks = unique(xpathSApply(html_parsed, "//div[@data-section='5']/text()", xmlValue, trim= TRUE))
  if(length(job_tasks) == 0 ){ #Check for missing data
    return("No Specified Job Assignments")
  }
  job_tasks = paste(job_tasks, collapse = ", ")
  return(job_tasks)
}

#-------------------------
get_job_location = function(html_parsed){ #This function finds the job location
  job_location = unique(xpathSApply(html_parsed,"//div[@class = 'location']//span[contains(.,text())]/text()", xmlValue))
  if(length(job_location) == 0){#Check for missing data
    return("No Job Location Provided")
  }
  job_location = paste(job_location, collapse = ", " )
  return(job_location)
}

#------------------------
get_salary_emp = function(html_parsed){ #This function gets the salary and employment type
  job_salary = unique(xpathSApply(html_parsed,
           "//div[@class = 'wage']//span[contains(.,text())]/text()", xmlValue))
  if(length(job_salary) == 0){#Check for missing entries
    return(c("No Salary Provided", "No Employment Type Provided"))
  }
  if(grepl("[^k$0-9]{4}", job_salary)){ #Split the salary range and employment type
    salary_1 = gsub("[^0-9$]+(\\$*[0-9]+.+)", "\\1", job_salary)
    employment_status_1 = gsub("([^0-9$]+)\\$*[0-9]+.+", "\\1", job_salary)
  }else{
    salary_1 = job_salary
    employment_status_1 = "No Employment Type Provided"
  }
    return(c(salary_1, employment_status_1))
}

#-----------------------------
get_required_skills = function(html_parsed){ #Obtain the required skills
 required_skills = unique(xpathSApply(html_parsed, 
            "//div[@data-section='7']", xmlValue, trim = TRUE)) #add /text() for crisper output, but will not capture some posts
 if(length(required_skills) == 0){ #check for missing entries
   return("No required Skills Specified")
 }
 required_skills = gsub("\\\n", " ",  required_skills)
 return(required_skills)
}

#-----------------------------
get_prefered_skills = function(html_parsed){ #obtain the preferred skills
  job_skills = unique(xpathSApply(html_parsed,"//span[@class = 'skill-name']", xmlValue))
  if(length(job_skills) == 0){ #Check for missing entries
    return("No Prefered Skills Specified")
  }
 return(paste(job_skills, collapse = ", "))
}


#------------------------
#degree_types = ".*(^[[:punct:][:space:]]*GED| GED |bachelor[ ]*['s]*|master[ ]*['s]* in |master[ ]*['s]* of |master[ ]*['s]* degree
#^[[:punct:][:space:]]*ms in|^[[:punct:][:space:]]*ms of | ms in | ms of |
#ms degree| ms\\,| ms or |^[[:punct:][:space:]]*m\\.s\\.| m\\.s\\. |doctorate|^[[:punct:][:space:]]phd| phd |
#^[[:punct:][:space:]]ph.d.| ph.d. |[^[[:punct:][:space:]]*p.h.d.|p.h.d.|high school diploma|associate['s]* degree|associate['s]* in).*"
#degree_types = paste0(degree_types,substring(degree_types, 3, nchar(degree_types)-2), "*", substring(degree_types, 3, nchar(degree_types)-2), "*", ".*")
degree_types= "^GED$|bachelor[s]*|^BA$|^BS$|^BABS$|^BSBA$|master[s]*[^A-Za-z]*$|^ms$|^msma$|^mams$|doctorate|^phd$|diploma|associate['s]*"
get_education = function(html_parsed){ #Get the education requirements
  free_form =unique(xpathSApply(html_parsed,
                            "//div[contains(@class, 'section-data')]/text()", xmlValue, trim= TRUE))
  #locate = grep()
  if(length(free_form) == 0){#Check for missing entries 
    return("No Degree Specified")
  }
  free_form = gsub("[[:punct:]]", "", free_form)
  free_form  = paste(free_form, collapse = " ")
  all_words = unlist(strsplit(free_form, " ", fixed = TRUE))
  degree_locate = grepl(degree_types, all_words, ignore.case = TRUE)
  if(sum(degree_locate) == 0){
    return("No Degree Specified")
  }
  #job_education = free_form[locate_education]
  #job_education = gsub(degree_types, "\\1 \\2 \\3",
                     #  job_education, ignore.case = TRUE)
  #job_education = unique(job_education)
  return(paste(all_words[degree_locate], collapse = " "))
}

#-----------------------
get_years_exp = function(html_parsed){ #Obtain the years of experience
  free_form = unique(xpathSApply(html_parsed, 
                  "//div[contains(@class, 'section-data')]/text()", xmlValue))
  locate_years_exp = grep("years [of]* experience|year[s]*", 
                          free_form, ignore.case = TRUE)
  if(length(locate_years_exp) == 0){#Check for missing entries
    return("No Years of Experience Specified")
  }
years_exp = free_form[locate_years_exp]
#remove extra text and capture more than one years specified for a skill
year_exp = gsub("[^0-9]*([0-9]{,2}-*[0-9]{1,2}[+ ]*year[s]*).*([0-9]{,2}-*[0-9]{1,2}[+ ]*year[s]*)*.*([0-9]{,2}-*[0-9]{1,2}[+ ]*year[s]*)*.*", 
                "\\1-\\2-\\3", years_exp) 
year_exp = unique(year_exp)
  return(paste(year_exp, collapse = "-"))
}

#----------------------
get_benefits = function(html_parsed){ #Get the job benefits
  job_benefits = unique(xpathSApply(html_parsed, 
                "//div[@data-section='8']/text()", xmlValue, trim = TRUE)) 
  if(length(job_benefits) == 0){ #Check for missing entries
    return("No Benefits Specified")
  }
  return(paste(job_benefits, collapse = " "))
}

#--------------------------------
get_company_incentives = function(html_parsed){ #Get the extra positives for working at the company
  company_info = unique(xpathSApply(html_parsed, 
                                    "//div[@data-section='2']/text()", xmlValue, trim = TRUE)) 
  if(length(company_info) == 0){ #Check for missing entries
    return("No Further Company Incentives Specified")
  }
    return(paste(company_info, collapse = " "))
}

#---------------------------------
poss_degrees = "computer science|statistics|biostatistics|[^A-Za-z]*math|mathematics|engineering|business analytics|physics|technical field|related field|genetics|computational biology"
degree_search = ".*(computer science|statistics|biostatistics|[^A-Za-z]*math|mathematics|engineering|business analytics|physics|technical field|related field|genetics|computational biology).*"
degree_gsub_search = paste0(degree_search,substring(degree_search, 3, nchar(degree_search)-2),
                      "*", substring(degree_search, 3, nchar(degree_search)-2), "*", ".*",
                      substring(degree_search, 3, nchar(degree_search)-2), "*", ".*")

get_degree_fields = function(html_parsed){ 
  job_descript = unique(xpathSApply(html_parsed,
                "//div[contains(@class, 'section-data')]/text()", 
                xmlValue, trim= TRUE))
  if(length(job_descript) == 0){ #Check for missing data
    return("No Degree Fields Specified")
  }
  locate_field = grep(poss_degrees, job_descript)
  ed_field = job_descript[locate_field]
  ed_field = gsub(degree_gsub_search, "\\1 \\2 \\3 \\4",
                  ed_field, ignore.case = TRUE)
  spec_fields = unique(ed_field)
  if(length(spec_fields) == 0){
    return("No Degree Fields Specified")
  }
  return(paste(spec_fields, collapse = " "))
}

#---------------------
get_job_info= function(html_page){
  if(html_page =="No Results"){
    return("No Data Available")
  }
  html_parsed = htmlParse(html_page)
  #Get the data:
  full_description = get_description(html_parsed)
  job_tasks = get_job_tasks(html_parsed)
  job_location = get_job_location(html_parsed)
  salary_and_employment = get_salary_emp(html_parsed)
  job_salary =  salary_and_employment[1]
  employ_type = salary_and_employment[2]
  required_skills = get_required_skills(html_parsed)
  prefered_skills = get_prefered_skills(html_parsed)
  education_levels =  get_education(html_parsed)
  years_experience = get_years_exp(html_parsed)
  degree_fields = get_degree_fields(html_parsed)
  benefits = get_benefits(html_parsed)
  company_details = get_company_incentives(html_parsed)
  return(c(full_description, job_tasks, job_location, job_salary,employ_type,
           required_skills, prefered_skills, education_levels, degree_fields, 
           years_experience, benefits, company_details))
}

#Data Analyst data:
data_fields_da = sapply(web_content_da[,2], get_job_info)
full_data_da = data.frame(cbind(web_content_da, t(data_fields_da)))

#Data Scientist data:
data_fields_ds  = sapply(web_content_ds[,2], get_job_info)
full_data_ds = data.frame(cbind(web_content_ds, t(data_fields_ds)))

#Statistician data:
data_fields_s  = sapply(web_content_s[,2], get_job_info)
full_data_s = data.frame(cbind(web_content_s, t(data_fields_s)))

#Data Engineer:
data_fields_de  = sapply(web_content_de[,2], get_job_info)
full_data_de = data.frame(cbind(web_content_de, t(data_fields_de)))

#Function for naming the columns:

name_cols = function(data_object){
  if(dim(data_object)[2] == 3){
    colnames(data_object) = c("Job Names", "Full HTML", "Data")
  }else{colnames(data_object) = c("Job Name", "Full HTML", "All Job Text", 
                             "Job Tasks", "Job Location", "Salary Range", 
                             "Employment Type", "Required Skills", "Preferred Skills",
                             "Education", "Degree Field", "Years of Experience", "Employee Benefits",
                             "Company Incentives")}
}
name_cols(full_data_da)
name_cols(full_data_ds)
name_cols(full_data_s)
name_cols(full_data_de)

#---------------------------------------------------------------------------------------------------------------------------

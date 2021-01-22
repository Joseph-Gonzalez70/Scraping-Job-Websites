#STA 141B Project 5
#Web Scaping:
#I will use indeed to web scrape job information:
#We can look at the Statistician search first:
#https://www.indeed.com/jobs?q=statistician&l

#First, I will go through some steps to obtain a procedure for getting
#the data
#--------------------------------------------------------------------------------------------------------------------
#Testing Indeed
#---------------------------------------------------------------------------------------------------------------------

library(curl)
library(RCurl)
library(XML)
library(xml2)
library(rlist)

web_url = "https://www.indeed.com/jobs?q=statistician&l"
web_content1 = readLines(web_url) 
#ReadLines works. However, the posts are out of order. 
#Using getForm and GetURLContent on their own does not obtain the correct html form
#I had to add fields into the .opts parameter to get the html in proper form.
cookie2 = 'CTK=1emd7vre6sa5p800; _ga=GA1.2.728288988.1604614879; RF="TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtYVTqxn-AoiOs3kbv-9XsAg="; CTK=1emd7vre6sa5p800; INDEED_CSRF_TOKEN=NOGtMWa1sP0rs0GsG7Lu0pVbnsP6iNJZ; jasxMarvin=1; SURF=Ikp6BahER7tWpoPRXNQK6NCp5Y2BJlug; _gcl_au=1.1.1373383692.1606091220; SHARED_INDEED_CSRF_TOKEN=NOGtMWa1sP0rs0GsG7Lu0pVbnsP6iNJZ; pjps=1; RSJC=30d45cd5c911c6c1; LC="co=US&hl=en"; CMP_VISITED=1; cmppmeta=eNoBWACn/xRuJNbue5UvPt3kPuHo20wGqkGpOFxnrtFbsjz09HBOtXVa4bAmnYqAA5WZSjwmHamZc6AlsO7eEcKxNmhTkjqSTggWJrfjdt3Vt8wZPKaoAlzzI2mIvTuxjStW; cmpsmeta="eNoBOADH/3ldn5a8fUd0vcyxG75L3y0bobsCXsBkwmn6ndAybKKJIWIOZ53Lf7BYDLoJtVlnNfHHYQPlS+gxIfsbhQ=="; g_state={"i_p":1606955107458,"i_l":3}; __ssid=37d64c650c2cd6b2ef6c0a6d64f4ebd; SOCK="SaLvIZ9bWL2XiYpQWo1hnIGfgsg="; SHOE="8LVJLBGyDNHk_qwtmdVZC3uzeQeaBiB8oSnbs-aiOgq2z32gjGia2o_FaDBHZozct30UZarAXOeLR43bFfftLwLYKG_UdV9Gvd13jPUm1ae-YaP9603pcbJfEX5tTTqB5Wb_0eMyT_P1fg=="; CSRF=L5DgLjhPd3MTkeMaiKkMLKA4rxDIsN9s; MICRO_CONTENT_CSRF_TOKEN=ebwJA7gaqD0KdFlVd9GnKQeIvx8070v2; CO=US; _mkto_trk=id:699-SXJ-715&token:_mch-indeed.com-1606429007741-35814; ROJC=5eb9a873b1c725dd:d837ebd3a5c1e417:e379661f504cfa5a:319a5826e4f249f3:e975e0850e28c3b1:81cde875e060ddaa:bf4578b1f4e6d3f7:9126c5309ff16a13:10100a842b71a829:d0cc42d13476e68b; indeed_rcc="PREF:cmppmeta:LV:CTK:CO:UD:RQ"; PPN=1; PREF="TM=1606592363236:L="; PPID=eyJraWQiOiI5MmMyZGZkNS0xZGIzLTRmY2YtODZlNS00YzJjMTk1ODE5MDgiLCJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJzdWIiOiJlNTBmMzhlMDY2ZDU5N2MxIiwiYXVkIjoiYzFhYjhmMDRmIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImNyZWF0ZWQiOjE1MDk0MzE2NjIwMDAsInJlbV9tZSI6dHJ1ZSwiaXNzIjoiaHR0cHM6XC9cL3NlY3VyZS5pbmRlZWQuY29tIiwiZXhwIjoxNjA2NzYyODk0LCJpYXQiOjE2MDY3NjEwOTQsImxvZ190cyI6MTYwNjM1NDQ2OCwiZW1haWwiOiJqZGdvbnphQGNhbGx1dGhlcmFuLmVkdSJ9.ISkDg9Re1WS9hmIreoa4KI-DyLOi02iBWacEx3s6y1DninZ8cbaGzRngRqQDnJBSZ7GNpTRFfpjDwVvEwe8FVQ; loctip=1; RQ="q=statistician&l=&ts=1606761094690&pts=1606592373416:q=janitorial+manager&l=Camarillo%2C+CA&ts=1606538966376:q=janitorial+manager&l=&ts=1606538575810:q=janitor&l=&ts=1606538564803:q=Basketball-Statistician&l=&ts=1606430700220:q=data+scientist&l=&ts=1606428225325&pts=1606281311623:q=statistician+&l=&ts=1606425564762:q=statistician+2&l=&ts=1606425390129:q=Data&l=Camarillo%2C+CA&ts=1606277330302:q=Data&l=Los+Angeles%2C+CA&ts=1606277273992"; jaSerpCount=114; _gid=GA1.2.63919141.1606761096; _gat=1; LV="LA=1606761094:LV=1606592363:CV=1606761094:TS=1606091203"; UD="LA=1606761094:LV=1606592363:CV=1606761094:TS=1606091203"; JSESSIONID=BA555144CAB2AA27774BDCDC909D44EB; _gali=whatWhereFormId; ac="l:#q:"'
webcontent_practice = suppressWarnings(getForm(web_url, .opts = list(
                                        referer = 'https://www.indeed.com/?from=gnav-jobsearch--jasx', 
                                        useragent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36'
                                        )))
#Referer seems to be the most important field.
webcontent_practice = suppressWarnings(getForm(web_url, .opts = list(followlocation = TRUE)))
x = rawToChar(webcontent_practice)

webcontent_practice = suppressWarnings(getForm(web_url, .opts = list(followlocation = TRUE,
                                      referer = 'https://www.indeed.com/?from=gnav-jobsearch--jasx')))
Sys.sleep(1) 
#Individual pages:
webcontent_practice2 = suppressWarnings(getForm("https://www.indeed.com/viewjob?jk=e379661f504cfa5a&tk=1eod6psfmo295800&from=serp&vjs=3", 
                              .opts = list(cookie = cookie2,
                              referer = 'https://www.indeed.com/?from=gnav-jobsearch--jasx', 
                              useragent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36'
                              )))
grepl("Pre-employment drug testing", webcontent_practice2)
#Next page:
Sys.sleep(1)
webcontent_practice3 = suppressWarnings(getForm("https://www.indeed.com/jobs?q=statistician&start=10", 
                               .opts = list(cookie = cookie2,
                                referer = 'https://www.indeed.com/?from=gnav-jobsearch--jasx', 
                                useragent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36'
                                )))
grepl("The Cleveland VA Medical Research", webcontent_practice3)
#page farther down list:
Sys.sleep(1)
webcontent_practice4 = suppressWarnings(getForm("https://www.indeed.com/viewjob?jk=ce5cff2c62df4ee2&tk=1eod94mqoo292800&from=serp&vjs=3&advn=2321614383300855&adid=361698325&ad=-6NYlbfkN0B0WYpsfFdlHnzkaod_43XUfc-xLBay7Qn_M2_P6Smr4Px3Ail4N-4efSn1FR2VJNANyvM8iCb4X5HSnTXLVtcYuFgua55gpu-ru8VoccvMqgyn7m5R6xHntegVJGea9rIMrqPT-Hgk72SuqAAZw2cdRGjTAvIv0IdnPvWe1BiIWXhxCbmN-Vdj52J_GQsyCiyQYzaT690yyT7RYuihkZDptw2x4MXd8A1IsSf_VeeGvRuBXX_K0hCmsMhzCMLZFH2WApTJCRB_UXUyj1XZnh39NtMs-Osckm2Mr10W3apUaiFoQrCHueNDcNPqCmIxRXw=&sjdu=vsxB_cusczO2O2SCSTNRaxUwFs_GKsRzLF_-yW55ffnzp1v9VSq6R6ViBDZ3uLAf4bnCZOh902wMgsU0wOUsHdw1QALXNsDPpNrM0c4zH_ukjPgh6ggBhUtPfrxUmihw0tTLyOhe07SSg5W3rWtkhdGTYPcXUOLPhqyjamhVY8Q", 
                              .opts = list(cookie = cookie2,
                              referer = 'https://www.indeed.com/?from=gnav-jobsearch--jasx', 
                              useragent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36'
                              )))
grepl("10 pounds frequently", webcontent_practice4)


#Below uses readLines to figure out the necessary functions to grab the ---------------------------------------------------------------
#required fields. This was done before I fixed the GetForm problem. The process
#should still work the same!

#Using readLines, I see that some jobs are not in order
#Will see why that is later
head(web_content1) #Matches with the beginning
web_content1[2220:2225] #Matches with the end
content_parsed = htmlParse(web_content1)
web_content1[85:100]
#Now, look for the posts:
first_page_listings = getNodeSet(content_parsed, "//h2[@class = 'title']") #h2  with 'title' or <a target="_blank"
first_page_listings[[1]] #Some results don't seem to match on the first page, Make sure to match total count

#Can get the job names:
unique(xpathSApply(first_page_listings[[1]], ".//a[@target = '_blank']/@title")) #Grabs the job title

job_names = sapply(first_page_listings, function(x)  unique(xpathSApply(x, #To get all job names
                                                                  ".//a[@target = '_blank']/@title")))

#Now, we need the URL for the job posting itself:
test_url = unique(xpathSApply(first_page_listings[[3]], ".//a[@target = '_blank']/@href"))
purl_part=gsub(".+(\\?jk=.+)", "\\1" , test_url)
full_purl=paste0("https://www.indeed.com/viewjob", purl_part) #Could not find full URLs, this may need to change
ptxt=readLines(full_purl) #This gives the html for the page

#Now, we have to find the arrow:
next_page = xpathSApply(content_parsed, "//link[@rel = 'next']/@href")
next_page_url = getRelativeURL(next_page, web_url)
web_content2 =  readLines(next_page_url)
content_parsed2 = htmlParse(web_content2)
first_page_listings2 = getNodeSet(content_parsed2, "//h2[@class = 'title']") 
first_page_listings2[[15]] 
#Seems to work, will need to confirm with total. Need to fix order at some point

rm(list = ls()[!(ls() %in% c('web_cookie','web_referer', 
                             'web_user_agent'))])
#--------------------------------------------------------------------------------------------------------------------
#BUILD FUNCTIONS:
#Webscrape the Indeed Website:
#Indeed seems top be more strict about webscraping on their website.
#This webscraping project is only for academic purposes only and will not be used
#for a 3rd party service or monetary gain. 
#-------------------------------------------------------------------------------
get_job_content = function(job_url){
  Sys.sleep(1)
  job_html_doc = suppressWarnings(getForm(job_url,
                         .opts = list(cookiejar='',
                                      #cookie = web_cookie,
                                      referer = web_referer, 
                                      useragent = web_user_agent,
                                      followlocation = TRUE)))#Get the html file for the current page
  return(job_html_doc)
}

get_postings = function(page_parsed){
  page_listings = getNodeSet(page_parsed, "//h2[@class = 'title']") #Listings on current page
  job_names = sapply(page_listings, function(x)  unique(xpathSApply(x,
                                ".//a[@target = '_blank']/@title"))) #Job Names
  job_urls_1 = sapply(page_listings, function(x)  unique(xpathSApply(x,
                                   ".//a[@target = '_blank']/@href")))  
  #job_urls_2 = gsub(".+(\\?[^=]=.+)", "\\1", job_urls_1)
  full_job_urls = sapply(job_urls_1, #Get the full job URL.
                         function(x) paste0("https://www.indeed.com", x)) #.com/viewjob
  job_specific_info = sapply(full_job_urls, get_job_content) #Function to get the job html pages.
  job_name_info = data.frame(job_names, job_specific_info) #Send dataframe back
  return(job_name_info)
}

get_all_jobs = function(web_url, query_name, job_location){
  all_job_posts = data.frame() #Put all job posts into a dataframe
  repeat{ #Repeat this chunk of code till we get to the last page
    Sys.sleep(1)
    web_content = suppressWarnings(getForm(web_url, q = query_name, l = job_location,
                          .opts = list(cookiejar='',
                                       cookie = web_cookie,
                                       referer = web_referer, 
                                       useragent = web_user_agent))) #Get the html content from the first page,useragent = web_user_agent
    web_parsed = htmlParse(web_content) 
    job_posts = get_postings(web_parsed) #Send the parsed html to function for jobs
    all_job_posts = rbind(all_job_posts, job_posts) #Add page's job posts to the dataframe
    #Find the next page's URL:
    next_page = xpathSApply(web_parsed, "//link[@rel = 'next']/@href") 
      if(length(next_page) == 0){break} #If the next page URL is not found, we break.
    web_url = getRelativeURL(next_page, web_url) #Next page full URL
  }
  return(all_job_posts)
}

#statistician:
indeed_url = "https://www.indeed.com/jobs" 
web_cookie = 'CTK=1emd7vre6sa5p800; _ga=GA1.2.728288988.1604614879; RF="TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtYVTqxn-AoiOs3kbv-9XsAg="; CTK=1emd7vre6sa5p800; jasxMarvin=1; _gcl_au=1.1.1373383692.1606091220; pjps=1; LC="co=US&hl=en"; CMP_VISITED=1; cmppmeta=eNoBWACn/xRuJNbue5UvPt3kPuHo20wGqkGpOFxnrtFbsjz09HBOtXVa4bAmnYqAA5WZSjwmHamZc6AlsO7eEcKxNmhTkjqSTggWJrfjdt3Vt8wZPKaoAlzzI2mIvTuxjStW; g_state={"i_p":1606955107458,"i_l":3}; __ssid=37d64c650c2cd6b2ef6c0a6d64f4ebd; SOCK="SaLvIZ9bWL2XiYpQWo1hnIGfgsg="; SHOE="8LVJLBGyDNHk_qwtmdVZC3uzeQeaBiB8oSnbs-aiOgq2z32gjGia2o_FaDBHZozct30UZarAXOeLR43bFfftLwLYKG_UdV9Gvd13jPUm1ae-YaP9603pcbJfEX5tTTqB5Wb_0eMyT_P1fg=="; CO=US; _mkto_trk=id:699-SXJ-715&token:_mch-indeed.com-1606429007741-35814; indeed_rcc="PREF:cmppmeta:LV:CTK:CO:UD:RQ"; loctip=1; RSJC=ce5cff2c62df4ee2:30d45cd5c911c6c1; INDEED_CSRF_TOKEN=pTecGxSjZ0zL1PrnwbaYNfj6fSnFYLCN; SHARED_INDEED_CSRF_TOKEN=pTecGxSjZ0zL1PrnwbaYNfj6fSnFYLCN; CSRF=QYva97A1OWdhjM92nazkvZQHez1us4qu; MICRO_CONTENT_CSRF_TOKEN=lqTUuPv0qV1DT10hKs6W9WmVXxejOifq; ROJC=86d364acf481664a:e975e0850e28c3b1:042a5ae3a11717e4:e379661f504cfa5a:5eb9a873b1c725dd:d837ebd3a5c1e417:319a5826e4f249f3:81cde875e060ddaa:bf4578b1f4e6d3f7:9126c5309ff16a13; UD="LA=1606959031:LV=1606884302:CV=1606949875:TS=1606091203"; RCLK="jk=e975e0850e28c3b1&tk=1eoj3i1lto2j1802&from=web&rd=0HcsNzDq-OLJ6bT4SC-8zPFPeETa0_JiahdAvij16oz8BChA0uFybm70vS5pOUm-&qd=7tdTJLF8oc4dPpT7T_zGvKhAQcNdCK5yEBmq9Egv_ojwJQuyF1HxE3gccONDuEKCvPbyOGNSpgkXc3qa4WVFayyELKK5do6J-tNQgTuocttshGFVLkPl97NnunDPL1Lw&ts=1606959040189&sal=0&onclick=1"; LV="LA=1606959373:LV=1606884302:CV=1606949875:TS=1606091203"; PPN=1; PREF="TM=1607056231475:L=Oxnard%2C+CA"; RQ="q=data+scientist&l=Oxnard%2C+CA&ts=1607056249130:q=statistician&l=Oxnard%2C+CA&ts=1607056243164:q=data+analyst&l=Oxnard%2C+CA&ts=1607056231605:q=data+analyst&l=Woodland+Hills%2C+CA&ts=1607056221000:q=data+scientist&l=Woodland+Hills%2C+CA&ts=1607056196113:q=statistician&l=Woodland+Hills%2C+CA&ts=1607055377813:q=statistician&l=Camarillo%2C+CA&ts=1607054895052:q=data+scientist&l=&ts=1606962900813&pts=1606955515221:q=data+analyst&l=&ts=1606962860541&pts=1606959534137:q=data+engineer&l=&ts=1606962822745"; jaSerpCount=105; JSESSIONID=6FC52E090463BC727BCF2AF1393D12AF; PPID=eyJraWQiOiIxYWIyYTVhYS1lMjVhLTQ2ODAtOTBlMi01YzVlYTk4NTIwZTciLCJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJzdWIiOiJlNTBmMzhlMDY2ZDU5N2MxIiwiYXVkIjoiYzFhYjhmMDRmIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImNyZWF0ZWQiOjE1MDk0MzE2NjIwMDAsInJlbV9tZSI6dHJ1ZSwiaXNzIjoiaHR0cHM6XC9cL3NlY3VyZS5pbmRlZWQuY29tIiwiZXhwIjoxNjA3MTQxNTUyLCJpYXQiOjE2MDcxMzk3NTIsImxvZ190cyI6MTYwNjM1NDQ2OCwiZW1haWwiOiJqZGdvbnphQGNhbGx1dGhlcmFuLmVkdSJ9.oq3dVBH0jJj_-ZyHSQB11pLki-rSzRF6a85Ofl-ZgeYFMZCTdRxPGvNtYDZcKBV3mowAKp6RYIRxcQ3y16HYkw; _gali=fj; PTK="tk=&type=jobsearch&subtype=topsearch"'
web_referer = 'https://www.indeed.com/'
web_user_agent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36'
web_content_stat = get_all_jobs(indeed_url, "statistician", "Oxnard, CA")

#Data Scientist
web_cookie = 'CTK=1emd7vre6sa5p800; _ga=GA1.2.728288988.1604614879; RF="TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtYVTqxn-AoiOs3kbv-9XsAg="; CTK=1emd7vre6sa5p800; jasxMarvin=1; _gcl_au=1.1.1373383692.1606091220; pjps=1; LC="co=US&hl=en"; CMP_VISITED=1; cmppmeta=eNoBWACn/xRuJNbue5UvPt3kPuHo20wGqkGpOFxnrtFbsjz09HBOtXVa4bAmnYqAA5WZSjwmHamZc6AlsO7eEcKxNmhTkjqSTggWJrfjdt3Vt8wZPKaoAlzzI2mIvTuxjStW; g_state={"i_p":1606955107458,"i_l":3}; __ssid=37d64c650c2cd6b2ef6c0a6d64f4ebd; SOCK="SaLvIZ9bWL2XiYpQWo1hnIGfgsg="; SHOE="8LVJLBGyDNHk_qwtmdVZC3uzeQeaBiB8oSnbs-aiOgq2z32gjGia2o_FaDBHZozct30UZarAXOeLR43bFfftLwLYKG_UdV9Gvd13jPUm1ae-YaP9603pcbJfEX5tTTqB5Wb_0eMyT_P1fg=="; CO=US; _mkto_trk=id:699-SXJ-715&token:_mch-indeed.com-1606429007741-35814; indeed_rcc="PREF:cmppmeta:LV:CTK:CO:UD:RQ"; loctip=1; RSJC=ce5cff2c62df4ee2:30d45cd5c911c6c1; INDEED_CSRF_TOKEN=pTecGxSjZ0zL1PrnwbaYNfj6fSnFYLCN; SHARED_INDEED_CSRF_TOKEN=pTecGxSjZ0zL1PrnwbaYNfj6fSnFYLCN; CSRF=QYva97A1OWdhjM92nazkvZQHez1us4qu; MICRO_CONTENT_CSRF_TOKEN=lqTUuPv0qV1DT10hKs6W9WmVXxejOifq; ROJC=86d364acf481664a:e975e0850e28c3b1:042a5ae3a11717e4:e379661f504cfa5a:5eb9a873b1c725dd:d837ebd3a5c1e417:319a5826e4f249f3:81cde875e060ddaa:bf4578b1f4e6d3f7:9126c5309ff16a13; UD="LA=1606959031:LV=1606884302:CV=1606949875:TS=1606091203"; RCLK="jk=e975e0850e28c3b1&tk=1eoj3i1lto2j1802&from=web&rd=0HcsNzDq-OLJ6bT4SC-8zPFPeETa0_JiahdAvij16oz8BChA0uFybm70vS5pOUm-&qd=7tdTJLF8oc4dPpT7T_zGvKhAQcNdCK5yEBmq9Egv_ojwJQuyF1HxE3gccONDuEKCvPbyOGNSpgkXc3qa4WVFayyELKK5do6J-tNQgTuocttshGFVLkPl97NnunDPL1Lw&ts=1606959040189&sal=0&onclick=1"; LV="LA=1606959373:LV=1606884302:CV=1606949875:TS=1606091203"; PPN=1; PREF="TM=1607056231475:L=Oxnard%2C+CA"; PPID=eyJraWQiOiIxYWIyYTVhYS1lMjVhLTQ2ODAtOTBlMi01YzVlYTk4NTIwZTciLCJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJzdWIiOiJlNTBmMzhlMDY2ZDU5N2MxIiwiYXVkIjoiYzFhYjhmMDRmIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImNyZWF0ZWQiOjE1MDk0MzE2NjIwMDAsInJlbV9tZSI6dHJ1ZSwiaXNzIjoiaHR0cHM6XC9cL3NlY3VyZS5pbmRlZWQuY29tIiwiZXhwIjoxNjA3MTQ5MjY3LCJpYXQiOjE2MDcxNDc0NjcsImxvZ190cyI6MTYwNjM1NDQ2OCwiZW1haWwiOiJqZGdvbnphQGNhbGx1dGhlcmFuLmVkdSJ9.VASJCxsYnjVLCBJBwbo9fh3AyYGSwOub1onQGh_S4J5eJgdmqdI9I_xKjBeMMgflRHDM1a_txP72kVCGOg_OxQ; RQ="q=statistician&l=Oxnard%2C+CA&ts=1607147467499&pts=1607056243164:q=data+scientist&l=Oxnard%2C+CA&ts=1607056249130:q=data+analyst&l=Oxnard%2C+CA&ts=1607056231605:q=data+analyst&l=Woodland+Hills%2C+CA&ts=1607056221000:q=data+scientist&l=Woodland+Hills%2C+CA&ts=1607056196113:q=statistician&l=Woodland+Hills%2C+CA&ts=1607055377813:q=statistician&l=Camarillo%2C+CA&ts=1607054895052:q=data+scientist&l=&ts=1606962900813&pts=1606955515221:q=data+analyst&l=&ts=1606962860541&pts=1606959534137:q=data+engineer&l=&ts=1606962822745"; jaSerpCount=106; _gid=GA1.2.1512598435.1607147468; JSESSIONID=2A9B6E2A193BBB89D8F513D06DF8A0B9; _gali=fj; PTK="tk=&type=jobsearch&subtype=topsearch"'
web_content_ds = get_all_jobs(indeed_url, "Data Scientist", "Oxnard, CA")

#data Analyst:
web_cookie = 'CTK=1emd7vre6sa5p800; _ga=GA1.2.728288988.1604614879; RF="TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtYVTqxn-AoiOs3kbv-9XsAg="; CTK=1emd7vre6sa5p800; jasxMarvin=1; _gcl_au=1.1.1373383692.1606091220; pjps=1; LC="co=US&hl=en"; CMP_VISITED=1; cmppmeta=eNoBWACn/xRuJNbue5UvPt3kPuHo20wGqkGpOFxnrtFbsjz09HBOtXVa4bAmnYqAA5WZSjwmHamZc6AlsO7eEcKxNmhTkjqSTggWJrfjdt3Vt8wZPKaoAlzzI2mIvTuxjStW; g_state={"i_p":1606955107458,"i_l":3}; __ssid=37d64c650c2cd6b2ef6c0a6d64f4ebd; SOCK="SaLvIZ9bWL2XiYpQWo1hnIGfgsg="; SHOE="8LVJLBGyDNHk_qwtmdVZC3uzeQeaBiB8oSnbs-aiOgq2z32gjGia2o_FaDBHZozct30UZarAXOeLR43bFfftLwLYKG_UdV9Gvd13jPUm1ae-YaP9603pcbJfEX5tTTqB5Wb_0eMyT_P1fg=="; CO=US; _mkto_trk=id:699-SXJ-715&token:_mch-indeed.com-1606429007741-35814; indeed_rcc="PREF:cmppmeta:LV:CTK:CO:UD:RQ"; loctip=1; RSJC=ce5cff2c62df4ee2:30d45cd5c911c6c1; INDEED_CSRF_TOKEN=pTecGxSjZ0zL1PrnwbaYNfj6fSnFYLCN; SHARED_INDEED_CSRF_TOKEN=pTecGxSjZ0zL1PrnwbaYNfj6fSnFYLCN; CSRF=QYva97A1OWdhjM92nazkvZQHez1us4qu; MICRO_CONTENT_CSRF_TOKEN=lqTUuPv0qV1DT10hKs6W9WmVXxejOifq; ROJC=86d364acf481664a:e975e0850e28c3b1:042a5ae3a11717e4:e379661f504cfa5a:5eb9a873b1c725dd:d837ebd3a5c1e417:319a5826e4f249f3:81cde875e060ddaa:bf4578b1f4e6d3f7:9126c5309ff16a13; UD="LA=1606959031:LV=1606884302:CV=1606949875:TS=1606091203"; RCLK="jk=e975e0850e28c3b1&tk=1eoj3i1lto2j1802&from=web&rd=0HcsNzDq-OLJ6bT4SC-8zPFPeETa0_JiahdAvij16oz8BChA0uFybm70vS5pOUm-&qd=7tdTJLF8oc4dPpT7T_zGvKhAQcNdCK5yEBmq9Egv_ojwJQuyF1HxE3gccONDuEKCvPbyOGNSpgkXc3qa4WVFayyELKK5do6J-tNQgTuocttshGFVLkPl97NnunDPL1Lw&ts=1606959040189&sal=0&onclick=1"; LV="LA=1606959373:LV=1606884302:CV=1606949875:TS=1606091203"; PPN=1; PREF="TM=1607056231475:L=Oxnard%2C+CA"; PPID=eyJraWQiOiIxYWIyYTVhYS1lMjVhLTQ2ODAtOTBlMi01YzVlYTk4NTIwZTciLCJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJzdWIiOiJlNTBmMzhlMDY2ZDU5N2MxIiwiYXVkIjoiYzFhYjhmMDRmIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImNyZWF0ZWQiOjE1MDk0MzE2NjIwMDAsInJlbV9tZSI6dHJ1ZSwiaXNzIjoiaHR0cHM6XC9cL3NlY3VyZS5pbmRlZWQuY29tIiwiZXhwIjoxNjA3MTQ5MjY3LCJpYXQiOjE2MDcxNDc0NjcsImxvZ190cyI6MTYwNjM1NDQ2OCwiZW1haWwiOiJqZGdvbnphQGNhbGx1dGhlcmFuLmVkdSJ9.VASJCxsYnjVLCBJBwbo9fh3AyYGSwOub1onQGh_S4J5eJgdmqdI9I_xKjBeMMgflRHDM1a_txP72kVCGOg_OxQ; _gid=GA1.2.1512598435.1607147468; RQ="q=data+scientist&l=Oxnard%2C+CA&ts=1607147599704&pts=1607056249130:q=statistician&l=Oxnard%2C+CA&ts=1607147467499&pts=1607056243164:q=data+analyst&l=Oxnard%2C+CA&ts=1607056231605:q=data+analyst&l=Woodland+Hills%2C+CA&ts=1607056221000:q=data+scientist&l=Woodland+Hills%2C+CA&ts=1607056196113:q=statistician&l=Woodland+Hills%2C+CA&ts=1607055377813:q=statistician&l=Camarillo%2C+CA&ts=1607054895052:q=data+scientist&l=&ts=1606962900813&pts=1606955515221:q=data+analyst&l=&ts=1606962860541&pts=1606959534137:q=data+engineer&l=&ts=1606962822745"; jaSerpCount=107; JSESSIONID=F48DF003E5840CD6AA3B9CE18F78BF72; _gali=fj; PTK="tk=&type=jobsearch&subtype=topsearch"
referer: https://www.indeed.com/jobs?q=data+scientist&l=Oxnard%2C+CA'
web_content_da = get_all_jobs(indeed_url, "Data Analyst", "Oxnard, CA")


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




library(rvest)
library(RCurl)
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(tidytext)
library(xtable)
library(readr)
library(tidytext)
library(knitr)



city.set_small <- c("New+York+NY", "Seattle+WA")

city.set <- c("New+York+NY", "Seattle+WA", "San+Francisco+CA",
              "Washington+DC","Atlanta+GA","Boston+MA", "Austin+TX")


target.job <- "data+scientist"   

base.url <- "https://www.indeed.com/"

max.results <- 50

#create a df to hold everything that we collect
jobs.data <- data.frame(matrix(ncol = 7, nrow = 0))
n <- c("city","job.title","company.name","job.location","summary.short","salary","links,summary.full")
colnames(jobs.data)


for (city in city.set){
  print(paste("Downloading data for: ", city))
  
  
  for (start in range(0,max.results,10)){
    
    url <- paste(base.url,"jobs?q=",target.job,"&l=",city,"&start=", start ,sep="")
    page <- read_html(url)
    Sys.sleep(1)
    
    #recorded the city search term << not working yet...
    #i<-i+1
    #job.city[i] <- city
    
    #get the links
    links <- page %>% 
      html_nodes("div") %>%
      html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
      html_attr("href")
    
    
    #get the job title
    job.title <- page %>% 
      html_nodes("div") %>%
      html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
      html_attr("title")
    
    #get the job title
    job.title <- page %>% 
      html_nodes("div") %>%
      html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
      html_attr("title")
    
    #get the company name
    company.name <- page %>% 
      html_nodes("span")  %>% 
      html_nodes(xpath = '//*[@class="company"]')  %>% 
      html_text() %>%
      trimws -> company.name 
    
    #get job location
    job.location <- page %>% 
      html_nodes("span") %>% 
      html_nodes(xpath = '//*[@class="location"]')%>% 
      html_text() %>%
      trimws -> job.location
    
    #get the short sumary
    summary.short <- page %>% 
      html_nodes("span")  %>% 
      html_nodes(xpath = '//*[@class="summary"]')  %>% 
      html_text() %>%
      trimws -> summary.short 
    
  }
  
  #create a structure to hold our full summaries
  summary.full <- rep(NA, length(links))
  
  #fill in the job data
  job.city <- rep(city,length(links))
  
  #add a place-holder for the salary
  job.salary <- rep(0,length(links))
  
  #iterate over the links that we collected
  for ( n in 1:length(links) ){
    
    #build the link
    link <- paste(base.url,links[n],sep="")
    
    #pull the link
    page <- read_html(link)
    
    #get the full summary
    s.full <- page %>%
      html_nodes("span")  %>% 
      html_nodes(xpath = '//*[@class="summary"]') %>% 
      html_text() %>%
      trimws -> s.full
    
    #check to make sure we got some data and if so, append it.
    #as expired postings return an empty var
    if (length(s.full) > 0 ){
      summary.full[n] = s.full  
    } 
    
  }
  
  #add the newly collected data to the jobs.data
  jobs.data <- rbind(jobs.data,data.frame(city,
                                          job.title,
                                          company.name,
                                          job.location,
                                          summary.short,
                                          job.salary,
                                          links,
                                          summary.full))
  
  
}

write.csv(jobs.data, file = "MyData.csv")

#DATA CLEANING

url <- "MyData.csv"

df <- read.csv(url, sep="|", stringsAsFactors = F)

sample <- df[sample(1:nrow(df), 100, replace=F),]

#Removed brackets surrounding summaries.

sample1 <- sample %>% separate(summary_full, c("bracket", "new_summary"), sep="^[\\[]", remove=T, convert=F) %>%
  separate(new_summary, c("summary_full", "bracket"), sep="[\\]]$", remove=T, convert=F)

sample1 <- sample1[, -c(5, 8)]

#Renamed column headers.

names(sample1) <- c("list_ID", "city", "job_title", "company_name", "link", "summary")
#Removed state and plus signs from city column.

# Separate City column into City and State by pattern of two uppercase letters after a plus sign (i.e., "+NY")
sample2 <- sample1 %>% separate(city, c("city", "state"), sep="[\\+][[:upper:]][[:upper:]]$", convert=T)

# Remove empty State column
sample2 <- sample2[, -c(3)]

# Replace plus signs with spaces
sample2$city <- str_replace_all(sample2$city, "[\\+]", " ")

df_final <- sample2
write.csv(df_final, "indeed_final.csv", row.names = FALSE)








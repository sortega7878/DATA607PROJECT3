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
library(RColorBrewer)
library(SnowballC)


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

res<-rquery.wordcloud(jobs.data, type=c("text"), 
                 lang="english", excludeWords = c("new", "york", "austin","sanfrancisco", "san", "francisco","newyorkny","seattlewa","austintx","seattle","boston","data","scientist",
                                                  "atlantaga","atlanta","washingtondc","sanfranciscoca","washington"), 
                 textStemming = FALSE,  colorPalette="Dark2",
                 max.words=200)
tdm <- res$tdm
freqTable <- res$freqTable

# Show the top10 words and their frequency
head(freqTable, 10)

# Bar plot of the frequency for the top10
barplot(freqTable[1:10,]$freq, las = 2, 
        names.arg = freqTable[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#we want to identify words that occur at least 15 times :
findFreqTerms(tdm, lowfreq = 15)

#You could also analyze the correlation (or association) between frequent terms. The R code below identifies which words are associated with "learning" 

findAssocs(tdm, terms = "learning", corlimit = 0.3)


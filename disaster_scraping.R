# Web scraping of disasters, CD 2020
# Website: https://reliefweb.int/disasters

rm(list=ls())
library(XML)
library(httr)
library(RCurl)

# get total number of observations for full df
total.disaster <- as.numeric( 
  gsub(",", "", 
       xpathSApply(htmlParse(content(GET("https://reliefweb.int/disasters"))), 
                   "//body/main/section[@id='disasters']/section[@id='list']/div[@id='results']/span", xmlValue)[3]) 
  )
# prepare full df
df <- data.frame(row.names = 1:total.disaster )

pb <- txtProgressBar(min = 0, max = total.disaster, initial = 0, style=3) 
e <- 0
current.max <- 0
while(current.max<total.disaster){
  # scrape content
  url <-  paste0( "https://reliefweb.int/disasters?page=", e )
  disaster.html.content <- htmlParse(content(GET(url)))
  
  # node paths
  path.headings <- "//body/main/section[@id='disasters']/section[@id='list']/
  div[@class='articles']/article[@class='disaster']/header/h4[@class='title']"
  path.status <- "//body/main/section[@id='disasters']/section[@id='list']/
  div[@class='articles']/article[@class='disaster']/footer/dl[@class='meta core']/dd[@class='status']"
  path.types <- "//body/main/section[@id='disasters']/section[@id='list']/
  div[@class='articles']/article[@class='disaster']/footer/dl[@class='meta core']/dd[@class='type']/ul"
  path.countries <- "//body/main/section[@id='disasters']/section[@id='list']/
  div[@class='articles']/article[@class='disaster']/footer/dl[@class='meta core']/dd[@class='country']/ul"
  
  # extract nodes
  headings <- xpathSApply(disaster.html.content, path.headings, xmlValue)
  status <- xpathSApply(disaster.html.content, path.status, xmlValue)
  types <- xpathSApply(disaster.html.content, path.types, xmlValue)
  countries <- xpathSApply(disaster.html.content, path.countries, xmlValue)
  
  # set up page data frame & clean strings
  df.page <- data.frame(row.names = 1:length(headings) )
  df.page$heading <- gsub("(^.{1,})(.{7}\\d{4}$)", "\\1", headings, perl=TRUE)
  df.page$date <- gsub("(^.{1,})(.{4}\\d{4}$)", "\\2", headings, perl=TRUE)
  df.page$month <- match(gsub("(^[a-zA-Z]{3})(\\s{1}\\d{4}$)", "\\1", df.page$date, perl=TRUE), month.abb)
  df.page$year <- as.numeric(gsub("(^[a-zA-Z]{3})(\\s{1})(\\d{4}$)", "\\3", df.page$date, perl=TRUE))
  df.page$date <- paste(df.page$year, df.page$month, sep="/")
  df.page$type <- gsub("\\s{1,}", "", types, perl=TRUE)
  df.page$type <- gsub("\\d{1,}more$", "", df.page$type, perl=TRUE)
  df.page$more.types <- as.numeric( gsub("[a-zA-Z]", "", types, perl=TRUE) )
  df.page$more.types[is.na(df.page$more.types)==TRUE] <- 0
  df.page$country <- gsub("\\s{1,}", "", countries, perl=TRUE)
  df.page$country <- gsub("\\d{1,}more$", "", df.page$country, perl=TRUE)
  df.page$more.countries <- as.numeric( gsub("[a-zA-Z]", "", countries, perl=TRUE) ) # warnings ok (special characters -> NA)
  df.page$more.countries[is.na(df.page$more.countries)==TRUE] <- 0

  df <- rbind(df, df.page) 
  
  e <- e+1
  current.max <- as.numeric( gsub(",", "", xpathSApply(disaster.html.content, 
                   "//body/main/section[@id='disasters']/section[@id='list']/div[@id='results']/span", xmlValue)[2]) )
  setTxtProgressBar(pb,current.max)
}

write.csv(df, file = "disaster_scraped_data.csv", fileEncoding = "utf-8")


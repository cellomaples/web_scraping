

# This web scraping script joins Joy of Painting episode airdate data from IMDB with
# painted elements data originally provided by Walt Hickey:
# https://fivethirtyeight.com/features/a-statistical-analysis-of-the-work-of-bob-ross/


rm(list=ls())

library(XML)
library(RCurl)

df <- c()
for(i in 1:31){
  url <- getURL(paste0("https://www.imdb.com/title/tt0383795/episodes?season=",i))
  pg <- htmlParse(url)
  
  ep = as.character(xpathApply(pg,"//*[@class='image']/a/div/div", xmlValue))
  tl = xpathSApply(pg,"//a[@itemprop='name']", xmlValue)
  dt = xpathSApply(pg, "//div[@class='airdate']", xmlValue)
  
  dftemp <- data.frame(Episode = ep, Title=tl, Airdate=dt)
  
  df <- rbind(df, dftemp)
}

# Account for any whitespace
df$Episode <- trimws(df$Episode)
df$Title <- trimws(df$Title)
df$Airdate <- trimws(df$Airdate)

# Coerce format of episode
for(i in 1:nrow(df)){
  if(substr(df[i,1],3,3)=="," && nchar(df[i,1])==7){
    df[i,1] <- paste0(substr(df[i,1],1,1),0,substr(df[i,1],2,2),"E",0,substr(df[i,1],7,7))
  }
  if(substr(df[i,1],3,3)=="," && nchar(df[i,1])==8){
    df[i,1] <- paste0(substr(df[i,1],1,1),0,substr(df[i,1],2,2),"E",substr(df[i,1],7,8))
  }
  if(substr(df[i,1],3,3)!="," && nchar(df[i,1])==8){
    df[i,1] <- paste0(substr(df[i,1],1,1),substr(df[i,1],2,3),"E",0,substr(df[i,1],8,8))
  }
  if(substr(df[i,1],3,3)!="," && nchar(df[i,1])==9){
    df[i,1] <- paste0(substr(df[i,1],1,1),substr(df[i,1],2,3),"E",substr(df[i,1],8,9))
  }
}

# Change title to all caps
df$Title <- toupper(df$Title)

# Coerce dates yyyy-mm-dd
df$Airdate <- gsub("[.]","",df$Airdate)

for(i in 1:nrow(df)){
  if(grepl("Jan",df[i,3])==T && nchar(df[i,3])==10){
    df[i,3] <- paste(substr(df[i,3],7,10),1,substr(df[i,3],1,1), sep = "-")
  }
  if(grepl("Jan",df[i,3])==T && nchar(df[i,3])==11){
    df[i,3] <- paste(substr(df[i,3],8,11),1,substr(df[i,3],1,2), sep = "-")
  }
  if(grepl("Feb",df[i,3])==T && nchar(df[i,3])==10){
    df[i,3] <- paste(substr(df[i,3],7,10),2,substr(df[i,3],1,1), sep = "-")
  }
  if(grepl("Feb",df[i,3])==T && nchar(df[i,3])==11){
    df[i,3] <- paste(substr(df[i,3],8,11),2,substr(df[i,3],1,2), sep = "-")
  }
  if(grepl("Mar",df[i,3])==T && nchar(df[i,3])==10){
    df[i,3] <- paste(substr(df[i,3],7,10),3,substr(df[i,3],1,1), sep = "-")
  }
  if(grepl("Mar",df[i,3])==T && nchar(df[i,3])==11){
    df[i,3] <- paste(substr(df[i,3],8,11),3,substr(df[i,3],1,2), sep = "-")
  }
  if(grepl("Apr",df[i,3])==T && nchar(df[i,3])==10){
    df[i,3] <- paste(substr(df[i,3],7,10),4,substr(df[i,3],1,1), sep = "-")
  }
  if(grepl("Apr",df[i,3])==T && nchar(df[i,3])==11){
    df[i,3] <- paste(substr(df[i,3],8,11),4,substr(df[i,3],1,2), sep = "-")
  }
  if(grepl("May",df[i,3])==T && nchar(df[i,3])==10){
    df[i,3] <- paste(substr(df[i,3],7,10),5,substr(df[i,3],1,1), sep = "-")
  }
  if(grepl("May",df[i,3])==T && nchar(df[i,3])==11){
    df[i,3] <- paste(substr(df[i,3],8,11),5,substr(df[i,3],1,2), sep = "-")
  }
  if(grepl("Jun",df[i,3])==T && nchar(df[i,3])==10){
    df[i,3] <- paste(substr(df[i,3],7,10),6,substr(df[i,3],1,1), sep = "-")
  }
  if(grepl("Jun",df[i,3])==T && nchar(df[i,3])==11){
    df[i,3] <- paste(substr(df[i,3],8,11),6,substr(df[i,3],1,2), sep = "-")
  }
  if(grepl("Jul",df[i,3])==T && nchar(df[i,3])==10){
    df[i,3] <- paste(substr(df[i,3],7,10),7,substr(df[i,3],1,1), sep = "-")
  }
  if(grepl("Jul",df[i,3])==T && nchar(df[i,3])==11){
    df[i,3] <- paste(substr(df[i,3],8,11),7,substr(df[i,3],1,2), sep = "-")
  }
  if(grepl("Aug",df[i,3])==T && nchar(df[i,3])==10){
    df[i,3] <- paste(substr(df[i,3],7,10),8,substr(df[i,3],1,1), sep = "-")
  }
  if(grepl("Aug",df[i,3])==T && nchar(df[i,3])==11){
    df[i,3] <- paste(substr(df[i,3],8,11),8,substr(df[i,3],1,2), sep = "-")
  }
  if(grepl("Sep",df[i,3])==T && nchar(df[i,3])==10){
    df[i,3] <- paste(substr(df[i,3],7,10),9,substr(df[i,3],1,1), sep = "-")
  }
  if(grepl("Sep",df[i,3])==T && nchar(df[i,3])==11){
    df[i,3] <- paste(substr(df[i,3],8,11),9,substr(df[i,3],1,2), sep = "-")
  }
  if(grepl("Oct",df[i,3])==T && nchar(df[i,3])==10){
    df[i,3] <- paste(substr(df[i,3],7,10),10,substr(df[i,3],1,1), sep = "-")
  }
  if(grepl("Oct",df[i,3])==T && nchar(df[i,3])==11){
    df[i,3] <- paste(substr(df[i,3],8,11),10,substr(df[i,3],1,2), sep = "-")
  }
  if(grepl("Nov",df[i,3])==T && nchar(df[i,3])==10){
    df[i,3] <- paste(substr(df[i,3],7,10),11,substr(df[i,3],1,1), sep = "-")
  }
  if(grepl("Nov",df[i,3])==T && nchar(df[i,3])==11){
    df[i,3] <- paste(substr(df[i,3],8,11),11,substr(df[i,3],1,2), sep = "-")
  }
  if(grepl("Dec",df[i,3])==T && nchar(df[i,3])==10){
    df[i,3] <- paste(substr(df[i,3],7,10),12,substr(df[i,3],1,1), sep = "-")
  }
  if(grepl("Dec",df[i,3])==T && nchar(df[i,3])==11){
    df[i,3] <- paste(substr(df[i,3],8,11),12,substr(df[i,3],1,2), sep = "-")
  }
}

df$Airdate <- as.Date(df$Airdate)


# Read in original data
df2 <- read.csv("elements-by-episode.csv", header = T, stringsAsFactors = F, skipNul = T, sep = "\t")
names(df2)[1] <- "Element"

df3 <- merge(df2, df, by = "Episode")
df3$Title.y <- NULL
names(df3)[3] <- "Title"
df3$Season <- sapply(df3$Episode, function(x) as.numeric(substr(x,2,3)))
df3 <- df3[,c("Episode","Season","Airdate","Title","Element","Included")]

write.csv(df3, "bob_ross.csv", row.names = F)

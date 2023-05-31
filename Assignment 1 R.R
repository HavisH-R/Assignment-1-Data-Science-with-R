library(tidyverse)
library(rvest)
library(dplyr)
library(imager)
library(stringr)


#Q a
data2<-read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
table <- html_nodes(data2,"table")
nifty50 <- html_table(table)[[1]]
nifty50 <- nifty50[1:50,2:13]


#Q b
#stock1
data3<- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/miscellaneous/trading/adani-enterprises/company-info")
table2 <-  html_nodes(data3,"table")
analysis_1 <- html_table(table2)[[1]]
analysis_1 <- analysis_1[-(2:5),1:11]
colnames(analysis_1) <- analysis_1[1,]
analysis_1 <- analysis_1[-1,]
analysis_2 <- html_table(table2)[[3]]
colnames(analysis_2) <- analysis_2[1,]
analysis_2 <- analysis_2[-1,1:11]
Analysis_1 <- rbind(analysis_1,analysis_2)
#stock2
data3<- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/automobiles/automobile-two-three-wheelers/hero-motocorp/company-info")
table2 <-  html_nodes(data3,"table")
analysis_1 <- html_table(table2)[[1]]
analysis_1 <- analysis_1[-(2:5),1:11]
colnames(analysis_1) <- analysis_1[1,]
analysis_1 <- analysis_1[-1,]
analysis_2 <- html_table(table2)[[3]]
colnames(analysis_2) <- analysis_2[1,]
analysis_2 <- analysis_2[-1,1:11]
Analysis_2 <- rbind(analysis_1,analysis_2)
#stock3
data3<- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/automobiles/automobile-two-three-wheelers/bajaj-auto/company-info")
table2 <-  html_nodes(data3,"table")
analysis_1 <- html_table(table2)[[1]]
analysis_1 <- analysis_1[-(2:5),1:11]
colnames(analysis_1) <- analysis_1[1,]
analysis_1 <- analysis_1[-1,]
analysis_2 <- html_table(table2)[[3]]
colnames(analysis_2) <- analysis_2[1,]
analysis_2 <- analysis_2[-1,1:11]
Analysis_3 <- rbind(analysis_1,analysis_2)
#stock4
data3<- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/metals-mining/steel-iron-products/tata-steel/company-info")
table2 <-  html_nodes(data3,"table")
analysis_1 <- html_table(table2)[[1]]
analysis_1 <- analysis_1[-(2:5),1:11]
colnames(analysis_1) <- analysis_1[1,]
analysis_1 <- analysis_1[-1,]
analysis_2 <- html_table(table2)[[3]]
colnames(analysis_2) <- analysis_2[1,]
analysis_2 <- analysis_2[-1,1:11]
Analysis_4 <- rbind(analysis_1,analysis_2)
#stock5
data3<- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/oil-gas/oil-exploration/ongc/company-info")
table2 <-  html_nodes(data3,"table")
analysis_1 <- html_table(table2)[[1]]
analysis_1 <- analysis_1[-(2:5),1:11]
colnames(analysis_1) <- analysis_1[1,]
analysis_1 <- analysis_1[-1,]
analysis_2 <- html_table(table2)[[3]]
colnames(analysis_2) <- analysis_2[1,]
analysis_2 <- analysis_2[-1,1:11]
Analysis_5 <- rbind(analysis_1,analysis_2)


#Q c
tennis <- function(p)
{
  wins_A <- 0
  wins_B <- 0
  count <- 0
  while(wins_A<3 && wins_B < 3){
    k <- rbinom(n =1, size = 1, prob = p)
    wins_A <- wins_A+k
    wins_B <- wins_B+1-k
    count <- count+1
  }
  
  return(count)
}
matches <- numeric(length = 1000)
for(i in 1:1000)
{
  matches[i] <- tennis(0.7)
}
ans <- mean(matches)


#Q d
MontyHall <- function(){
  car <- sample(x = 1:3, size = 1)
  initial <- sample(x = 1:3, size = 1) 
  v <- numeric(length= 3)
  for(i in 1:3){
    if(car == initial){
      if(i == initial) v[i] <- 0 
      else v[i] <- 0.5}
    else{
      v[car] <- 0
      v[initial] <- 0
      v[6-car-initial] <- 1
    }
  }
  monty <- sample(x = 1:3, size = 1,prob = v)
  i = 6-monty-initial
  if(car == i) return(1)
  else return(0)
}
values <- numeric(length = 1000)
for(i in 1:1000){
  values[i] <- MontyHall()}
result <- mean(values)



#Q e
data<-read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
score<- data %>% html_elements(".article_movie_title span.tMeterScore") %>% html_text()
score <- str_remove_all(score, "%") %>% as.numeric()
movie_names <- data %>% html_elements(".article_movie_title a") %>% html_text()
year <- data %>% html_elements(".article_movie_title span") %>% html_text()
for(i in 1:100){
  year[i] <- year[3*i -2]
}
year <- year[1:100]
year <- str_remove_all(year, "[(,)]") %>% as.numeric()

ranking <- data %>% html_elements(".countdown-index") %>% html_text()
ranking <- str_remove_all(ranking, "#") %>% as.numeric()
movie_table <- data.frame("Ranking" = ranking,"Name of Movie" = movie_names,"Tomato % score" = score, "Year of movie
" = year )  

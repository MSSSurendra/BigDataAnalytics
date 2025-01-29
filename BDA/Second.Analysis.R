install.packages("qdap")
library(qdap)
library(dplyr)
install.packages("tm")
library(tm)
install.packages("stringr")
library(stringr)
tweets<-read.csv("coffee.csv")
glimpse(tweets)
nrow(tweets)
coffee_tweets<-tweets$text
coffee_tweets
coffee_source<-VectorSource(coffee_tweets)
coffee_source
coffee_corpus<-VCorpus(coffee_source)
coffee_corpus
coffee_corpus$content<-gsub(pattern="\\#",replacement=" ",coffee_corpus$content)
coffee_corpus$content<-gsub(pattern ="\\@",replacement = "",coffee_corpus$content)
head(coffee_corpus$content)
coffee_corpus$content<-gsub(pattern ="\\-",replacement = "",coffee_corpus$content)
coffee_corpus$content<-gsub(pattern ="\\:",replacement = "",coffee_corpus$content)
coffee_corpus$content<-gsub(pattern ="\\!",replacement = "",coffee_corpus$content)
coffee_corpus$content<-gsub(pattern ="\\_",replacement = "",coffee_corpus$content)
coffee_corpus$content
tolower(coffee_corpus$content)
coffee_corpus$content<-gsub("\\s+"," ",coffee_corpus$content)
str_trim(clean(coffee_corpus$content))
Trim(clean(coffee_corpus$content))
removeWords(coffee_corpus$content,stopwords("english"))

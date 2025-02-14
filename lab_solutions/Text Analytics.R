install.packages("dplyr")
install.packages("qdap")
install.packages("tm")
library(qdap)
library(dplyr)
install.packages("tm")
library(tm)
install.packages("stringr")
library(stringr)
tweets<-read.csv("coffee.csv")
head(tweets)
glimpse(tweets)
nrow(tweets)
ncol(tweets)
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
#File 1

write.csv(coffee_corpus$content, file = "File 1.csv", row.names = FALSE)

tolower(coffee_corpus$content)

coffee_corpus$content<-gsub("\\s+"," ",coffee_corpus$content)
print(coffee_corpus$content)

#File 2

write.csv(coffee_corpus$content, file = "File 2.csv", row.names = FALSE)


coffee_corpus$content <- str_trim(coffee_corpus$content)

#File 3

write.csv(coffee_corpus$content, file = "File 3.csv", row.names = FALSE)

coffee_corpus$content <- trimws(coffee_corpus$content)

#File 4

write.csv(coffee_corpus$content, file = "File 4.csv", row.names = FALSE)
#Trim(coffee_corpus$content)
removeWords(coffee_corpus$content,stopwords("english"))
print(coffee_corpus$content)


#File 5

write.csv(coffee_corpus$content, file = "File 5.csv", row.names = FALSE)
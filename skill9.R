# a) Unzip the data from the UCI Repository
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00228/smsspamcollection.zip"
download.file(url, destfile = "smsspamcollection.zip")
unzip("smsspamcollection.zip", exdir = "spam_data")

# b) Read the data into 'data_text' by a tab separator
data_text <- read.delim("spam_data/SMSSpamCollection", sep = "\t", header = FALSE, stringsAsFactors = FALSE, quote = "", fill = TRUE)


# c) Rename the column names as 'class' and 'text' and factorize the class variables
colnames(data_text) <- c("Class", "Text")
data_text$Class <- factor(data_text$Class)

# d) Import the 'tm' and 'SnowballC' libraries and display the proportion of spam and ham records
library(tm)
library(SnowballC)
prop.table(table(data_text$Class))

# e) Create a corpus of the text data by considering the text column of the 'data_text'
corpus <- VCorpus(VectorSource(data_text$Text))

# f) Preprocess this corpus data
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

# g) Generate a Document-Term Matrix (DTM) and remove sparse terms
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.999)

# h) Write a 'convert_count' function
convert_count <- function(x) {
  y <- ifelse(x > 0, 1, 0)
  y <- factor(y, levels = c(0,1), labels = c("No", "Yes"))
  return(y)
}

# i) Apply convert_count function to the dtm columns
datasetNB <- apply(dtm, 2, convert_count)

# j) Convert this modified dtm into a matrix and store it in a data frame named 'dataset'
dataset <- as.data.frame(as.matrix(datasetNB))

# k) Sort the rows of the dtm in decreasing order of their column sums and store in 'freq'
freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)

# l) Find out the frequent terms from the 'dtm' with a minimum frequency of 60
cat(findFreqTerms(dtm, lowfreq=60), sep=", ")


# m) Construct a data frame 'wf' with the names of the 'freq' as 'word' and the freq as freq
wf <- data.frame(word = names(freq), freq = freq)

# n) Plot a bar graph displaying the frequency of words using the 'ggplot2' library
library(ggplot2)
ggplot(subset(wf, freq > 100), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# o) Import 'wordcloud' and 'RColorBrewer' libraries and plot a word cloud
library(wordcloud)
library(RColorBrewer)
set.seed(1234)
wordcloud(words = wf$word, freq = wf$freq, min.freq = 1, max.words = 200, 
          random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

# p) Append the class variable of 'data_text' into 'dataset'
dataset$Class <- data_text$Class

# q) Split the dataset into training and test sets in a 3:1 ratio
set.seed(222)
split <- sample(2, nrow(dataset), prob = c(0.75, 0.25), replace = TRUE)
train_set <- dataset[split == 1, ]
test_set <- dataset[split == 2, ]

# r) Apply the random forest classifier to the training dataset
library(randomForest)
rf_classifier <- randomForest(x = train_set[-ncol(train_set)], y = train_set$Class, ntree = 300)

# s) Evaluate the model on the test data and display the confusion matrixinstall.packages("caret")
rf_pred <- predict(rf_classifier, newdata = test_set[-ncol(test_set)])
library(caret)
confusionMatrix(table(rf_pred, test_set$Class))
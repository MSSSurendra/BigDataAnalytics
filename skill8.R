install.packages("tidytext")

library(data.table)
library(readr)
library(dplyr)
library(caTools)
library(NLP)
library(tm)
library(tidytext)
library(tidyr)
library(lattice)

# Read CSV file with correct delimiter
product_review <- fread('D:/ACADEMICS/SEM6/BDA/amazon_reviews.csv', sep='\t', showProgress = FALSE)

# Print actual column names
print(colnames(product_review))

# Ensure 'RATING' is numeric
product_review <- product_review %>%
  rename(review = REVIEW_TEXT) %>%
  mutate(RATING = as.numeric(RATING))

# Display count of each rating
table_of_ratings <- product_review %>% group_by(RATING) %>% summarize(count = n())
print(table_of_ratings)

# Filter out rating 3
product_review_filtered <- product_review %>% filter(RATING != 3)

# Count after filtering
table_of_ratings_filtered <- product_review_filtered %>% group_by(RATING) %>% summarize(count = n())
print(table_of_ratings_filtered)

# Create a new binary rating column
product_review_modified <- product_review_filtered %>%
  mutate(rating_new = case_when(RATING %in% c(1, 2) ~ 0,
                                RATING %in% c(4, 5) ~ 1))
print(head(product_review_modified))

# Count of new binary ratings
table_of_ratings_modified <- product_review_modified %>% group_by(rating_new) %>% summarize(count = n())
print(table_of_ratings_modified)

# Drop the old 'RATING' column
review_cleaned <- subset(product_review_modified, select = -c(RATING))

# Split dataset into 80% training and 20% testing
set.seed(123)
split = sample.split(review_cleaned$rating_new, SplitRatio = 0.80)
training_data = subset(review_cleaned, split == TRUE)
testing_data = subset(review_cleaned, split == FALSE)

# Print dimensions
print(dim(training_data))
print(dim(testing_data))

# Text Processing with tm package
reviews_train_vs <- VectorSource(training_data$review)
reviews_test_vs <- VectorSource(testing_data$review)

# Create Corpus
review_train_corpus <- Corpus(reviews_train_vs)
review_test_corpus <- Corpus(reviews_test_vs)

# Print structure of Corpus
print(review_train_corpus)
print(review_test_corpus)

# Add text length column
training_data$textLength <- nchar(training_data$review)

# Check for NA values before plotting histogram
if (sum(is.na(training_data$textLength)) == 0) {
  hist(training_data$textLength, main="Review Text Length Distribution", col="skyblue", xlab="Character Count")
} else {
  print("Warning: NA values in text length column.")
}
library(tm)
library(SnowballC)
library(cluster)
library(proxy)

# Step 1: Create a corpus
print("Step 1: Creating corpus...")
tweets <- c(
  "Love the sunny weather today! #happy #sunny #day",
  "Just finished a great workout at the gym #fitness #health",
  "Trying out a new recipe for dinner #cooking #food",
  "Had a relaxing day at the beach #vacation #ocean #sun",
  "The new movie was fantastic! #movies #cinema",
  "Feeling good after my workout! #fitness #workout",
  "Can't wait for the weekend! #weekend #fun",
  "I’m loving this new book I started reading #books #reading",
  "Excited for the concert tomorrow! #music #concert",
  "Had a great time with friends at the park #friends #fun"
)

corpus <- Corpus(VectorSource(tweets))
print("Corpus created successfully.")
inspect(corpus)  # Check corpus content

# Step 2: Preprocessing the text
print("Step 2: Preprocessing text...")
corpus <- tm_map(corpus, content_transformer(tolower))  # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation)  # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)  # Remove numbers
corpus <- tm_map(corpus, stripWhitespace)  # Remove extra spaces
corpus <- tm_map(corpus, removeWords, stopwords("en"))  # Remove common stopwords
corpus <- tm_map(corpus, content_transformer(function(x) gsub("http\\S+|www\\S+", " ", x)))  # Remove URLs

print("Text preprocessing completed.")
inspect(corpus)  # Verify cleaned text

# Step 3: Create a Term-Document Matrix (TDM)
print("Step 3: Creating Term-Document Matrix...")
tdm <- TermDocumentMatrix(corpus)
print("TDM created successfully.")
inspect(tdm)  # Check matrix content

# Step 4: Convert TDM to a matrix and remove sparse terms
print("Step 4: Converting TDM to matrix and removing sparse terms...")
tdm_matrix <- as.matrix(tdm)
if (nrow(tdm_matrix) == 0) {
  stop("Error: Term-Document Matrix is empty after conversion. Check preprocessing steps.")
}
print(dim(tdm_matrix))  # Check matrix dimensions before filtering
tdm_matrix <- tdm_matrix[rowSums(tdm_matrix) > 0, ]  # Remove zero-frequency terms
print(dim(tdm_matrix))  # Verify if filtering worked

# Step 5: Compute the distance matrix
print("Step 5: Computing distance matrix...")
dist_matrix <- dist(tdm_matrix, method = "euclidean")
print("Distance matrix computed.")

# Step 6: Perform hierarchical clustering
print("Step 6: Performing hierarchical clustering...")
if (length(dist_matrix) == 0) {
  stop("Error: Distance matrix is empty. Check TDM matrix and ensure data is not missing.")
}
hc <- hclust(dist_matrix, method = "ward.D")
print("Hierarchical clustering completed.")

# Step 7: Plot the dendrogram
print("Step 7: Plotting dendrogram...")
plot(hc, main = "Dendrogram of Word Clusters", xlab = "Terms", sub = "", cex = 0.8)

# Step 8: Group the dendrogram into 4 clusters
print("Step 8: Highlighting clusters...")
rect.hclust(hc, k = 4, border = "red")  # Highlight 4 clusters with blue rectangles
print("Clustering visualization completed.")
# Install necessary libraries
install.packages(c("MASS", "rattle", "klaR"))

# Load necessary libraries
library(MASS)
library(rattle)
library(klaR)

# Load the wine dataset from the rattle package
data(wine, package = "rattle")

# Display a scatterplot matrix
pairs(wine[, 2:6])

# Apply linear discriminant analysis
lda_model <- lda(type ~ ., data = wine)

# Print the proportion of trace for the two LDA components
print(lda_model)

# Plot the two components of LDA
plot(lda_model$x[, 1], lda_model$x[, 2], col = as.numeric(wine$type), 
     xlab = "LDA1", ylab = "LDA2", main = "LDA Plot")
legend("topright", levels(wine$type), col = 1:3, pch = 1)

# Draw an LDA plot using the 'parmitat' function
s <- parmitat(wine[, c("Alcohol", "Alcalinity")], wine$type)
plot(s)


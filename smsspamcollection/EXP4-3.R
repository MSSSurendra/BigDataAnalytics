# Calculate the Pearson correlation coefficient 
pearson_correlation <- cor(data$BloodPressure, data$Age, method = "pearson") 
# Perform a hypothesis test 
test_result <- cor.test(data$BloodPressure, data$Age, method = "pearson") 
# Check the p-value 
p_value <- test_result$p.value 
if (p_value < 0.05) { 
  cat("The pressure and age attributes are dependent.") 
} else { 
  cat("The pressure and age attributes are independent.") 
}


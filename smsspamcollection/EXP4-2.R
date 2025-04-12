# Create a contingency table 
contingency_table <- table(data$BloodPressure, data$Age) 
contingency_table <- data.frame(data$BloodPressure, data$Age) 
# Perform the Chi-Square test 
chi_square_result <- chisq.test(contingency_table) 
# Check the p-value 
p_value <- chi_square_result$p.value 
if (p_value < 0.05) { 
  cat("The pressure and age attributes are dependent.") 
} else { 
  cat("The pressure and age attributes are independent.") 
}

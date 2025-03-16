library(ggplot2)

system("sqoop import --connect jdbc:mysql://localhost/employees --username root --password password \
--table employees --target-dir /user/hadoop/employees_basic --m 1")

system("sqoop import --connect jdbc:mysql://localhost/employees --username root --password password \
--query 'SELECT emp_no, first_name, gender FROM employees WHERE $CONDITIONS' \
--target-dir /user/hadoop/employees_intermediate --split-by emp_no --m 2")


system("sqoop export --connect jdbc:mysql://localhost/employees --username root --password password \
--table exported_employees --export-dir /user/hadoop/employees_intermediate --m 1")

data <- read.csv("C:/Users/chsiv/OneDrive/Documents/BIG_DATA_ANALYTICS/datasets/employees_basic.csv", header = TRUE)

ggplot(data, aes(x = factor(gender))) +
  geom_bar(fill = "red") +
  labs(title = "Gender Distribution of Employees", x = "Gender", y = "Count") +
  theme_minimal()


#lab 9
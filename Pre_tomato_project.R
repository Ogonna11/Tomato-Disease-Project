setwd("/home/ogonna/Desktop/Lenovo Laptop/Tomatoes Projects/Latest_data_and_codes")
# Load necessary libraries
library(glmnet)
library(compositions)  # Assuming 'acomp' function is from this package
library(Compositional)
library(fastDummies)
library(MASS)


# Read and preprocess the data
data <- read.csv("Model.csv")
data <- data[,-1]
Response <- data[, c("SC1", "SC2", "SC3", "SC4", "SC5", "SC6", "SC7", "SC8")]
Response <- as.matrix(Response)
data <- data[, !(names(data) %in% c("SC1", "SC2", "SC3", "SC4", "SC5", "SC6", "SC7", "SC8"))]

# Encode categorical variables using one-hot encoding (dummy variables)
data <- dummy_cols(data, select_columns = c("Time", "Year", "Commercial.small.scale"))

# Drop original categorical columns
data <-subset(data, select=-c(Time, Year, Commercial.small.scale))
data<-as.matrix(data)



Response_mat <- Response 

# Replace all zero entries with a small positive number
small_number <- 1e-3
# Response_mat[Response_mat == 0] <- small_number
Response_mat <- Response_mat + small_number

# Normalize each row to sum to 1
row_sums <- rowSums(Response_mat)
Response_normalized <- Response_mat / row_sums

# Checking if each row sums to 1
row_sums_after <- rowSums(Response_normalized)
print(row_sums_after)



model <- lasso.compreg(Response_normalized, data, alpha = 1, lambda = NULL,
                       nlambda = 100, xnew = NULL)

print(model)

# Extract the lambda that minimizes cross-validation error

optimal_lambda <- model$lambda.min

# Extract coefficients at this lambda
coefficients <- coef(model, s = optimal_lambda)

# Coefficients are in a sparse matrix format, converting to regular matrix
coefficients_matrix <- as.matrix(coefficients)

# Getting names of non-zero coefficients
important_variables <- rownames(coefficients_matrix)[coefficients_matrix[,1] != 0]

# Print important variables
print(important_variables)



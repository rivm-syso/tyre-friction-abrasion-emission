library(Rcpp)

# Load the C++ code
sourceCpp("cpp/rdp.cpp")

# Example usage
points <- matrix(c(1.0, 0.9, 2.0, 1.2, 3.0, 1.0, 4.0, 1.0), ncol = 2, byrow = TRUE)
epsilon <- 0.1  # Adjust as needed

# Call the rdpWrapper function
result <- rdpWrapper(points, epsilon)

# Print or process the result as needed
print(result$points)

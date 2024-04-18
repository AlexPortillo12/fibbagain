# Define the Fibonate function with improved formatting
Fibonate <- function(n = 100, x1 = 0, x2 = 1) {
  if (n < 2) {
    stop("n should be at least 2 to generate a sequence.")
  }

  fibs <- numeric(n)
  fibs[1] <- x1
  fibs[2] <- x2

  for (i in 3:n) {
    fibs[i] <- fibs[i - 1] + fibs[i - 2]
  }

  # Avoid scientific notation
  options(scipen=999)
  fibs <- format(fibs, scientific = FALSE, trim = TRUE)  # Format numbers for better display

  return(fibs)
}

# Function to print Fibonacci sequence in a readable format
print_fibs <- function(fibonacci_sequence) {
  cat("Fibonacci Sequence:\n")
  print_no_per_line <- 10  # How many numbers per line you want to print
  fibs_length <- length(fibonacci_sequence)

  for (i in seq(1, fibs_length, by = print_no_per_line)) {
    cat(fibonacci_sequence[i:min(i+print_no_per_line-1, fibs_length)], "\n")
  }
}

# Function for custom formatted output of smaller sequences
print_custom_fibs <- function(fibs) {
  formatted_fibs <- paste(fibs, collapse = ", ")
  cat("Custom Fibonacci Sequence: [", formatted_fibs, "]\n")
}

# Example usage:
# Default usage to print the first 100 Fibonacci numbers in a clean format
fib_sequence <- Fibonate()
print_fibs(fib_sequence)

# Printing a smaller sequence with default start values
small_fibs <- Fibonate(n = 5)
print_custom_fibs(small_fibs)

# Custom starting values
custom_fibs <- Fibonate(n = 5, x1 = 2, x2 = 4.5)
print_custom_fibs(custom_fibs)


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

 
  options(scipen=999)
  fibs <- format(fibs, scientific = FALSE, trim = TRUE)  

  return(fibs)
}


print_fibs <- function(fibonacci_sequence) {
  cat("Fibonacci Sequence:\n")
  print_no_per_line <- 10  
  fibs_length <- length(fibonacci_sequence)

  for (i in seq(1, fibs_length, by = print_no_per_line)) {
    cat(fibonacci_sequence[i:min(i+print_no_per_line-1, fibs_length)], "\n")
  }
}


print_custom_fibs <- function(fibs) {
  formatted_fibs <- paste(fibs, collapse = ", ")
  cat("Custom Fibonacci Sequence: [", formatted_fibs, "]\n")
}


fib_sequence <- Fibonate()
print_fibs(fib_sequence)


small_fibs <- Fibonate(n = 5)
print_custom_fibs(small_fibs)


custom_fibs <- Fibonate(n = 5, x1 = 2, x2 = 4.5)
print_custom_fibs(custom_fibs)

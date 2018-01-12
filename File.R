

#Peer-graded Assignment: Functional and Object-Oriented Programming

# Load libraries ---------------------------------------------------------------

library(purrr)
library(microbenchmark)


#Part 1: Factorial Function

#The objective of Part 1 is to write a function that 
#computes the factorial of an integer greater than or equal to 0. 
#Recall that the factorial of a number n is n * (n-1) * (n - 2) * … * 1. 
#The factorial of 0 is defined to be 1.

#1#Factorial_loop: a version that computes the 
#factorial of an integer using looping (such as a for loop)

factorial_loop <- function(x) {
  if (x == 0 || x == 1)
    return(1)
  for (i in (x - 1):1) {
    x <- x * i
  }
  x
}
#2#Factorial_reduce: a version that computes the factorial using the reduce() 
#function in the purrr package. 
#Alternatively, you can use the Reduce() function in the base package.

factorial_reduce <- function(x) {
  if (x == 0)
    return(1)
  reduce(1:x, `*`)
}

#3#Factorial_func: a version that uses recursion to compute the factorial.

factorial_func <- function(x) {
  if (x == 0)
    return(1)
  x * factorial_func(x - 1)
}

#4#Factorial_mem: a version that uses memoization to compute the factorial.

fact_tbl <- c(rep(NA, 10))

factorial_mem <- function(x) {
  if (x == 0)
    return(1)
  if (!is.na(fact_tbl)[x])
    return(fact_tbl[x])
  fact_tbl[x] <- x * factorial_mem(x - 1)
  
}


# After writing your four versions of the Factorial function, 
# use the microbenchmark package to time the operation of these functions 
# and provide a summary of their performance. In addition to timing your 
# functions for specific inputs, make sure to show a range of inputs in order 
# to demonstrate the timing of each function for larger inputs.

inputs <- c(0, 1,4,7,10)

# Check if all functions produce the same results

map_dbl(inputs, factorial_loop)
map_dbl(inputs, factorial_reduce)
map_dbl(inputs, factorial_func)
map_dbl(inputs, factorial_mem)

# Measure performance and create output ----------------------------------------

# Use microbenchmark and purrr package to calculate performance for different 
# input values and for ranges of input values


# Reset lookup table for comparing purposes
fact_tbl <- c(rep(NA, 10))


# Calculate and compare perforamnce of individual input values
results <- map(inputs, ~ microbenchmark(
  factorial_loop(.),
  factorial_reduce(.),
  factorial_func(.),
  factorial_mem(.)
))

names(results) <- as.character(inputs)
results








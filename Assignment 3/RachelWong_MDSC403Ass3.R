# MDSC 403: Assignment 3
# Rachel Wong
# September 19th, 2019

# note: clear directory (lists)
rm(list = ls(all = T))

# 1. Write a for-loop to calculate the sum from 1, 2, to 100.
# initialize the sum to 0
sum_1to100 = 0
# use a for loop to go through the values 1 to 100 and add values to the sum
for (value in 1:100) {
  sum_1to100 = sum_1to100 + value
}
# print the sum
print (sum_1to100)

# 2. For a given matrix, find the indices of columns that contain exactly 
# one entry larger than 20. (Please try to write a two-layer loop, instead of 
# using a matrix operation)

# make a 4x4 matrix
given_matrix <- matrix( c(25, 2, 3, 4, 5, 6, 7, 8, 25), nrow = 3, ncol = 3)
# initialize the vector containing indicies of larger than 20 values and the count
larger_than_20 <- c()
count = 0

# use a for loop to search through the columns in the matrix
for (column in 1:ncol(given_matrix)) {
  # use a second for loop to search through the rows in the matrix
  for (row in 1:nrow(given_matrix)) {
    # if the value at the given column and row is greater than 20, increase count
    # the for loop is going through each row in the specified column of the first
    # for loop
    if (given_matrix[row, column] > 20) {
      count = count + 1
    }
  }
  # if the count is equal to 1, that means that each column only contained one row
  # that had a value greater than 20
  if (count == 1) {
  larger_than_20 <- c(larger_than_20, column)
  count = 0 # you need to reset your count to move onto the next column
  }
}

given_matrix
larger_than_20

# 3. Write a function that takes an integer matrix as input and outputs a 
# matrix indicating whether the corresponding entry is a prime number or not.

# make a 4x4 matrix
integer_matrix <- matrix( c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)

# create a prime function
prime <- function(number) {
  # initialize the flag as 0
  prime_flag = 0
  # if the number is 1, the prime flag is 1
  if (number == 1) {
    prime_flag = 1
  }
  # if the number is 2, the prime flag is 2
  else if (number == 2) {
    prime_flag = 0
  }
  else {
    # create a for loop that searches through the range of 2 to itself-1
    for(i in 2:(number-1)) {
      # if the number is divisble by another number in the range (1 to itself-1)
      # we break and the flag becomes 1
      calculation = number %% i 
      if (calculation == 0) {
        prime_flag = 1
      }
    }
  }
  return (prime_flag)
} 

# create a function that takes an integer matrix as input
prime_number <- function(integer_matrix) {
  # use a for loop to search through columns
  new_matrix=matrix(TRUE, nrow(integer_matrix), ncol(integer_matrix))
  for (column in 1:ncol(integer_matrix)) {
    for (row in 1:nrow(integer_matrix)) {
      #print(c(column,row))
      number = (integer_matrix[row, column])
      prime_flag = prime(number)
      #print(integer_matrix[row, column])
      # return TRUE or FALSE based on the prime flag value
      if (prime_flag == 0) {
        new_matrix[row, column] <- TRUE
      }
      else {
        new_matrix[row, column] <- FALSE
      }
      print(integer_matrix[row, column])
    }
  }
  # return a new matrix containing TRUE or FALSE if the value is a prime number
  return (new_matrix)
}

integer_matrix
outcome<-prime_number(integer_matrix)

print (outcome)

# 4. Write a function to select the two largest elements in an array. 
# (Again, please try to write code from the scratch instead of using R 
# functions such as sort and max.)

# create a random array
given_array <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10)

# create a function to determine the two largest elements in an array
largest_elements <- function(given_array) {
  
  # initialize the values for the largest and second largest elements
  largest = 0
  second_largest = 0
  
  # use a for loop to search through the elements in a given array
  for (element in given_array) {
    # if the element is larger than the largest, make the second largest equal to
    # the largest and the largest equal to the element
    if (element > largest) {
      second_largest <- largest
      largest <- element
    }
    # if the element is equal to the largest, make the second largest equal to the
    # element
    else if (element == largest) {
      second_largest <- element
    }
  }
  
  # return the values of the largest and second largest elements in a vector
  large_values <- c(largest, second_largest)
  return (large_values)
}

largest_elements (given_array)

# 5. Given an integer N, make an N x N magic square. (Don’t know what is 
# magic square? See here: https://en.wikipedia.org/wiki/Magic_square) 

# define a function, magic_square, that creates an nxn magic square
# for odd sized magic squares only
# note: R starts at an index of 1
magic_square <- function(n) {
  
  # initialize the magic square matrix and magic constant
  # note: magic_constant not used in calculations
  # create a matrix nxn filled with 0 values
  ms_matrix <- matrix(0, n, n)
  magic_constant <- n*(n^2+1)/2
  
  # initialize position of 1 
  i = floor((n / 2)) + 1 # floor rounds down (note: ceiling rounds up)
  j = (n - 1) + 1
  # position 1 is counter at 1 (1)
  ms_matrix[i,j] = 1
  
  # use a while loop to fill the magic square until the counter is larger than
  # the amount of numbers to fill in the magic square
  # ex// if n = 3, n^2 = 9, which means we have 9 numbers to fill in our magic
  # square, we use the counter to count each value we place in the square. We
  # initialize our counter at 2 because we have counted for position 1 already.
  # The counter is also the number of interest in each while that will be placed
  # in the magic square. 
  counter = 2
  while (counter <= (n^2)) 
  {
    
    # if none of the if else conditions work, the position of the next number is
    # calculated using (i-1,j+1)
    i = i-1
    j = j+1
    
    # if the row position is equal to 0 and the column position is equal to n+1,
    # then the position (i,j) = (1, n-1)
    if ((i == 0 ) && (j == n+1)) {
      i = 1
      j = n-1
    }
    # else, if the row value is less than 1, it will go around
    # also, else, if the column value is n+1, it will go around as well
    else {
      if (i < 1) {
        i = n
      }
      if (j == n+1) {
        j = 1
      }
    }
    
    # you want to use a while loop so that while the position is not empty (not 0)
    # you want to keep applying the correct increment and decrement to the position
    # row and column for that counter number
    while (ms_matrix[i,j] != 0) {
      # if there is already a number at the calculated position, the calculated
      # row position is incremented by 1 and the calculated column position is
      # decremented by 2
      i = i + 1
      j = j - 2
    
    }
    # else, the counter number in interest is placed at that position of the
    # magic square and the counter number is incremented by 1
    
      ms_matrix[i,j] = counter
      counter = counter + 1

  }
  # after the while loop, we can return our nxn magic square
  return (ms_matrix)
  
}
# call magic square function for n=7
magic_square(7)

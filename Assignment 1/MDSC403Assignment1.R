# MDSC 403: Assignment 1
# Rachel Wong
# September 5th, 2019

# note: clear directory (lists)
rm(list = ls(all = T))

# 1. Write the R statement to make a vector “myscores” that has the scores of 
# (45,48,44,50)
myscores <- c(45, 48, 44, 50)

# 2. Create myscores_percent (total is 52 points, create percents)
myscores_percent <- 1/52 * (myscores) * 100

# 3. Function to find the number of scores in myscores
length (myscores)

# 4. Single function to get a simple set of descriptive statistics about 
# myscores, including max, min and other values
summary (myscores)
myscores_summary = summary (myscores)

# 5. R statement to make this list and call the resulting list “pavelrec”.
pavelrec <- list("name" = "Pavel", "bhsc_program" = "BINF", "gpa" = 3.85)

# 6. Write 2 different ways to change Pavel’s gpa to 3.89 in pavelrec.
# First way
# update the whole list
pavelrec <- list("name" = "Pavel", "bhsc_program" = "BINF", "gpa" = 3.89)
# Second way
pavelrec[3] <- 3.89
# or
pavelrec$gpa <- 3.89

# 7. Write a simple R statement to create the pavelrec with this new system 
# (note: you can create it as an entirely new list)
# create a vector (gpa) which contains both values and update the list
gpa <- c(3.85, 3.89)
pavelrec <- list("name" = "Pavel", "bhsc_program" = "BINF", "gpa" = gpa)
# note: you may be able to do this by creating a list for gpa instead of
# a vector (c)
# pavelrec <- list("name" = "Pavel", "bhsc_program" = "BINF", "gpa" = list (3.85, 3.89))

# 8. Write a simple statement (or function) to calculate the number of 
# years a student has completed.
# note: you need to refer to the gpa list created in the pavelrec list
number_of_years = length(pavelrec$gpa)

# 9. Write a statement to add a variable years_complete to the list to hold 
# this information.
years_complete = length(pavelrec$gpa)
pavelrec["years_complete"] = length(pavelrec$gpa)

# 10. “years_complete” and the gpa entry now have redundant information. 
# When might this be a good design choice and when might it be a bad design choice?
# This would be good if the actual GPA values changed but the number of GPA entries
# did not change, that way the years_complete would still be accurate even if the
# GPA value changed. This would be good for variables that you want to change together
# but bad if you do not want your variables to change together.
# This would be bad if the years complete counted each GPA
# entry. This would mean if the GPA was entered again because of a change then the
# years_complete would update as well and not be accurate to the actual years
# completed.

# Assignment Part 2

#11
# kval <- c(4, 22, 8, 17, 0.4, 3, 0.1) Write statements for the following: 
kval <- c(4, 22, 8, 17, 0.4, 3, 0.1)
# a) Get the third element of kval
third_element = kval[3]
print (third_element) # print the third element of kval

# b) Add the number 12 to kval
# could do this to update kval
# kval <- c(4, 22, 8, 17, 0.4, 3, 0.1, 12)
# or
kval [length(kval) + 1] = 12

# c) Add the numbers 12, 14, 17 to kval in one statement
# could do this to update kval
# kval <- c(4, 22, 8, 17, 0.4, 3, 0.1, 12, 14, 17)
# or
kval <- append (kval, c(12, 14, 17))

# d) Get all elements of kval that are >10
# create a for loop that searches through kval and prints elements >10
greater_than_10 <- c(NULL)
 for (i in 1:length(kval)) {
  if (kval[i] > 10) {
    greater_than_10 <- append (greater_than_10, kval[i])
    print(kval[i]) # value with name
 }
 }

# e) Get the indices of all elements of kval that are >10 (which)
greater_than_10_kvalIndices <- which (kval > 10)

# f) Get all elements of kval EXCEPT the third element 
which (kval != kval[3]) #finds the indices (not at 3)
without3_kvalElements <- kval[which (kval!= kval[3])] #indexes kval at the indices found which returns the elements

#12
# Form a 3 x 4 matrix A. (3 rows x 4 columns)
A = matrix(1:12, nrow = 3, ncol = 4) 
# data length is 12 for 3x4 so add numbers 1-12 for the matrix A

# a) Extract the 2nd row of the matrix 
row2 = A [2,] 
# note: the [] is [row, column] so you need to include the comma

# b) Extract the 3rd column of the matrix 
column3 = A [,3]

# c) Switch A[1,2] and A[2,1] 
tempValue = A[2,1]
A[2,1] = A[1,2]
A[1,2] = tempValue

# d) How many entries in the first column are larger than 10?
greater_than_10_matrix <- length (which ((A [,1]) > 10))

# e) Calculate the mean of each row
# rowM <- rowMeans(A)
# or
rowM <- apply (A, 1, mean)

# f) Calculate the max of each column
colMax <- apply (A, 2, max) 

# 13. Create a vector and find its largest and second largest numbers.
# create a vector rachel
rachel <- c(5, 10, 15, 20, 25, 30, 35)
# find the max (largest value) of vector rachel
largestValue <- max (rachel)
# find the second largest vector rachel
# find the length of the vector rachel
lengthOfvector <- length(rachel)
# sort the vector rachel in numerical order and find the value at the
# indicie of length of rachel vector minus 1 (to get the second largest value)
# note: sort function sorts values in ascending order 
secondLargestValue <- sort(rachel,partial=lengthOfvector-1)[lengthOfvector-1]


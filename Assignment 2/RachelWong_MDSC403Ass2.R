# MDSC 403: Assignment 2
# Rachel Wong
# September 12th, 2019

# note: clear directory (lists)
rm(list = ls(all = T))

# 1.	Assume genevals is a tab separated file in 
# /home/me/mydata/genevals2018.tsv. Read this file and assign to the 
# variable genevals. Assume there is a header and the columns are named 
# Gene_id, gene description, and s????_??C. 
genevals <- read.table("/Users/rachelwong/Desktop/genevals2018.tsv", header = TRUE)
# note: set working directory in bottom right corner (More)
# * you can set column names (for example, change "type" to "gene description")

# 2. Get the number of rows (not including the header)
number_of_rows <- nrow(genevals)

# 3. Get the number of columns
number_of_cols <- ncol(genevals)

# 4. Get the value of the 4th column and 100th gene in the list
column4_gene100 <- genevals [100, 4]

#5. Get all the experiment values for Gene_id is AT1G01140
# find the row number for AT1G01140 in column 1
# AT1G01140_row_number <- which (genevals[,1] == "AT1G01140")
# get the experiment values for the row number for AT1G01140
# AT1G01140_row_values = genevals [AT1G01140_row_number,] 

# genevals$Gene_id =="AT1G01140"
# prints the TRUE/FALSE of if AT1G01140 is found

# provides the row values
AT1G01140_row_values = genevals[genevals$Gene_id =="AT1G01140",]
AT1G01140_row_values # tells you the row values for AT1G01140

#6. Get the experiment values for Gene_id AT1G0627 and only experiments between 
# columns s0002_16C and s0004_10C.

#AT1G06270_row_number <- which (genevals[,1] == "AT1G06270")
# get the experiment values for the row number for AT1G06270
#AT1G06270_row_values = genevals [AT1G06270_row_number,] 

# provides the row values
AT1G06270_row_values = genevals[genevals$Gene_id =="AT1G06270",]
AT1G06270_row_values # tells you the row values for AT1G06270

# provides the row values of AT1G06270_row_values_16Cto10C
# done by picking column indices of 4 to 8 (16C to 10C)
AT1G06270_row_values_16Cto10C <- subset (AT1G06270_row_values, select=s0002_16C:s0004_10C)
# subset runs only a few parts of the matrix, in this case columns from
# 16C to 10C
# subset takes the information from variable AT1G06270_row_values_16Cto10C and
# gives you the subset information, but we can assign it to a new variable

# this line prints out the index of the column at the specified column name
grep("s0002_16C", colnames(genevals))

# 7. Make a new dataframe in which you have data from genes in rows 
# (this is by row number, not Gene_id): row numbers are: 45, 32, 178, 195.
new_order_rows = c(45, 32, 178, 195)
new_dataframeQ7 <- genevals[new_order_rows,]

# 8. Your friend tells you that the data in experiment s1074_16C was bad. 
# Make a new dataframe with all the data except this column.
new_dataframeQ8 <- genevals[, (colnames(genevals) != "s1074_16C")]
# you can also do it this way, create a new data frame and change the column to null
#new_dataframeQ8 <- genevals
#new_dataframeQ8$s1074_16C <- NULL 

# 9. Your friend then says that there are additional problems – 
# not only s1074_16C was bad, but also s1158_16C, s1257_16C, and s1367_16C. 
# Make a dataframe that eliminates these (and s1074_16C).

# Make a vector of bad columns
#bad_columns <- c("s1158_16C","s1257_16C", "s1367_16C", "s1074_16C")
#new_dataframeQ9 <- genevals[, (colnames(genevals) %in% bad_columns)]
# %in% checks values in the list of bad_columns

# create a new dataset and make columns NULL if they are bad
new_dataframeQ9 <- genevals
new_dataframeQ9$s1158_16C <- new_dataframeQ9$s1257_16C <- new_dataframeQ9$s1367_16C <- new_dataframeQ9$s1074_16C <- NULL

# note: the class is already set as data.frame so no need to state the class

# 10. Get a “list” of all the column names so that you can check to make sure 
# these columns are gone.
column_names_new <- colnames(new_dataframeQ9)

# 11. Write a statement to verify that s1074_16C is missing from the new 
# dataframe (in which you eliminated s1367_16C).

# use %in% to search in the column names of the new dataframe

if("s1074_16C" %in% colnames(new_dataframeQ9))
{
  print ("No, it is still there!")
} else
{
  print ("Yes, it is gone!")
}

# create a verify variable that will return T or F if the name is in the dataframe
# verify = "s1074_16C" %in% colnames(new_dataframeQ9)

# 12. Write a statement to verify that s1074_16C is in the old original dataframe

# Prints true for verify since s1074_16C is present in the column names
# of the original dataframe
verify = "s1074_16C" %in% colnames(genevals)
verify

# 13. Write a statement to get the means of each data column
#column_means <- apply(genevals, 2, mean)
# you need to remove the first two columns because they are strings
#column_means <- apply(genevals[,3:395], 2, mean)

# use a for loop to force the program to search through each column
# this gives you NA NA for the first two columns
# this solution is best if you have mutliple string columns
column_means <- NULL # you need to create column_means first
for(i in 1:395){column_means[i]=mean(genevals[,i])}

# 14. Write a statement to get the variances of each data column
column_vars <- NULL
for(i in 3:395){column_vars[i]=var(genevals[,i])}
# for loop starts at 3 and fills in 1, 2 with NA
# column_vars <- apply(genevals[,3:395], 2, var)

# 15. The really key data is the Gene_id and the data columns (description 
# is nice but extra). Some software requires that you use a matrix, not a 
# dataframe. Convert your dataframe into a matrix, retaining the Gene_id 
# information (hint: where would this logically be stored in the matrix?)

# store row names from column 1 in variable row_names
# Gene_id is your row names now (index)
rownames(genevals) <- genevals [,1]

# matrix <- data.matrix(genevals [,3:length(genevals)],)

# Create the matrix
matrix <- data.matrix(genevals)

# This stores the gene_id as row names (index)

# 16. 16C and 10C actually means different temperature. Would you try to say 
# something about the differentially expressed genes between the two temperatures? 
# (Note: this can be a semester-long research project. You can do whatever you 
# can.)  

# Create a count at 0
# Using a for loop to go through the columns in genevals
# if the column name ends with "10C" we add that column (next_column) and
# its column index (count_10C) to our columns_10C 
# and then we increase the column index and move to the next column and check that
# column if it ends in "10C" and if it does then its count is +1 what the last
# count was
# You want to use the counter so that if the "if" is not true, you are not counting
# or missing the index of that false if, so your columns_10C indices make sense
# Do the same thing for 16C

# Initialize values, initialize vectors
count_10C = 0
count_16C = 0
columns_10C = c()
columns_16C = c()

for (i in 1:395) {
  
  next_column = colnames(genevals)[i]
  if (endsWith(next_column, "10C")) {
    count_10C = count_10C + 1
    columns_10C[count_10C] = next_column
  } else 
    if (endsWith(next_column, "16C")) {
    count_16C = count_16C + 1
    columns_16C[count_16C] = next_column
    }
  
}

# Create a new dataframe that filtered out 10C or 16C values  
# add a , so that it does every row at those columns
values_10C = genevals[,columns_10C]
values_16C = genevals[,columns_16C]

# Now that we have the filtered data frames we can find the mean of each row
# using apply. This now generates vectors containing the means of samples 
# expressing a gene at 10C versus at 16C.
means_10C = apply(values_10C, 1, mean)
means_16C = apply(values_16C, 1, mean)

# We can compare the expression of genes at different temperatures (10C and 16C)
# by first finding the amount each gene is expressed at 10C versus that gene
# expressed at 16C. We do this by finding the mean of all the samples 
# expressing a certain gene at 10C and compare that to the mean of all the samples
# expressing that same gene but at 16C. By comparing the average values of gene
# expression, we can see differences of expression level of a specific gene at
# 10C versus at 16C. 

# If we assume that the values given in genevals is equal to the expression level
# of each sample for each specific gene. We can ask if gene expression levels at
# 16C are greater than 10C or not. If true, this would suggest that higher temperatures
# result in higher gene expression, and if false, this would suggest that lower
# temperatures result in lower gene expression.

# Find genes that have higher expression at 16C
gene_names = rownames(genevals)
genes_16C_higher_means <- gene_names[means_16C[gene_names] > means_10C[gene_names]]

message ("The number of genes where expression is higher at
16C is ", length(genes_16C_higher_means), " and the number of genes where expression
is higher at 10C is ", (4999 - length(genes_16C_higher_means)), ". Since the gene expression 
is higher in most genes at 10C, this suggests that gene expression is higher at 10C than 16C. ")

# Since the gene expression is higher in most genes at 10C, this suggests that
# gene expression is higher at 10C than 16C. 

# We can also compare other things besides the mean

# Compare the variance
var_10C = apply(values_10C, 1, var)
var_16C = apply(values_16C, 1, var)
genes_16C_higher_var <- gene_names[var_16C[gene_names] > var_10C[gene_names]]

# Compare the maximums
max_10C = apply(values_10C, 1, max)
max_16C = apply(values_16C, 1, max)
genes_16C_higher_max <- gene_names[max_16C[gene_names] > max_10C[gene_names]]

# Compare the minimums
min_10C = apply(values_10C, 1, min)
min_16C = apply(values_16C, 1, min)
genes_16C_higher_min <- gene_names[min_16C[gene_names] > min_10C[gene_names]]

# note: we can also determine the range from the max and min

# Compare the standard deviations
sd_10C = apply(values_10C, 1, sd)
sd_16C = apply(values_16C, 1, sd)
genes_16C_higher_sd <- gene_names[sd_16C[gene_names] > sd_10C[gene_names]]

# The variance, maximum, minimum, and standard deviation results are important
# in measuring the validity of our experiment and looking for outliers. 
# The mean of 10C samples and 16C samples for specific genes is important to us
# for determining differences in gene expression levels at different temperatures.
# The next step to our analysis would be to conduct statistics.

# For example, a t-test can be used to determine if the means of specific
# gene expression for 10C versus 16C are significantly different from each other.

# We want to conduct a paired t-test (paired because we have the same genes)
t_test_results <- t.test(means_10C,means_16C,paired=TRUE) 
print (t_test_results)

# The p-value calculated is 0.6195 which is greater than 0.05, suggesting that
# there is no significant difference of gene expression at 10C versus 16C

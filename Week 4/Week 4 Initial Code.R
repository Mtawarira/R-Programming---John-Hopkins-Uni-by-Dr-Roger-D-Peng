##Read the outcome data into R via the read.csv function and look at the first few rows.
outcome <- read.csv("outcome-of-care-measures.csv", header = TRUE, sep = ",", colClasses = "character")
head(outcome)

##To make a simple histogram of the 30-day death rates from heart attack (column 11 in the outcome dataset)
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])
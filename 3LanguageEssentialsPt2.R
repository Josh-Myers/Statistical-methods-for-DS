#### Intro2R: 3 R Language Essentials Part 2 ####

# Source: Peter Dalgaard, Introductory Statistics with R
rm(list=ls())

#### 3.1 Matrices ####

x = 1:12
dim(x) = c(3,4) # treats x as a 3 x 4 matrix
x

# Create a 3x4 matrix, filled with the numbers 1-12 by rows
matrix(1:12, nrow = 3, byrow = T) 

# save the matrix, name the rows A, B and C (rownames)
# then transpose it (t())
x <- matrix(1:12, nrow = 3, byrow = T)
rownames(x) = LETTERS[1:3] #uses built in vectors; 
x
t(x)

# Other built in vectors
letters
month.name
month.abb

# Create matrices using cbind and rbind
cbind(A=1:4, B=5:8, C=9:12)
rbind(A=1:4, B=5:8, C=9:12)

#### 3.2 Lists ####


intake.pre = c(5260, 5470, 5640, 6180, 6390, 6815,
               6805,7515,7515,8230,8770)
intake.post = c(3910,4220,3885,5160,5645,4680,
                5265,5975,6790,6900,7335)

# Combine these vectors into a list called "energylist" naming the object
# as "before" and "after"

energylist = list(before=intake.pre, after=intake.post)

# Extract the pre-menstrual energy intake
energylist$before 


#### 3.3 Data Frames ####

# Create a data frame "d" holding pre and post menstrual
# energy intake using data.frame()

d = data.frame(intake.pre, intake.post)

# by converting our list from 3.2

d = as.data.frame(energylist)

# extract a column of the dataframe
d$before

# Save image of the workspace for use next time
save.image(file="ForNextTime.RData")


#### 3.4 Further Help ####

browseURL("https://www.datacamp.com/courses/free-introduction-to-r")
browseURL("http://tryr.codeschool.com")
library(swirl)
swirl()
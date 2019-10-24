#### Intro2R: 2 R Language Essentials Part 1 ####

# Source: Peter Dalgaard, Introductory Statistics with R

rm(list=ls()) #clears workspace... use wisely
# install.packages("ISwR")
library(ISwR)

#### 2.1 Expressions and Objects ####

# R's basic mode of operation is expression evaluation.. 

weight = c(60,72,57,90,95,72) 
height = c(1.75, 1.8, 1.65, 1.9, 1.74, 1.91)

weight/height^2 # operation
mean(weight) # function call

#### 2.2 Functions and Arguments ####

# Many things in R are done with function calls 

log(100)
log(x=100, base = 10)
log(100,10)

?log
args(log)

# Most fucntions have specified defaults


#### 2.3 Vectors ####

# Character
c("Huey", "Dewey", "Louie")
# Logical
c(T,T,F,T)

bmi = weight/height^2
bmi > 25

# NB Data often contains missing values. Vectors can contain an NA value that 
# is carried through computations to yield NA as a result

weight = c(60,NA,57,90,95,72)
mean(weight)
mean(na.omit(weight))

# Forces you to deal with missing values explicitly

#### 2.4 Functions that create vectors: c(), seq(), rep() ####

?c
c(42,57,12,39,1,3,4)

?seq
seq(4,9)
seq(4,9,2)
4:9 # great for integers

?rep
oops = c(7,9,13)
rep(oops,3)
rep(oops,1:3)
# has to be the same length --> 7 is repeated once, 9 twice, 13 thrice
rep(1:2, c(10,15))

#### 2.5 Factors ####

# Statistical data often has categorical variables, e.g sex, habitat type
# These are FACTORS, which have a set of LEVELS, these are stored internally as 
# integers but they can be named to make things easier for us!

# Vector with discrete values (levels)

pain = c(0,3,2,2,1)
fpain = factor(pain, levels=0:3)
levels(fpain) = c("none", "mild", "medium", "severe")
fpain
as.numeric(fpain)
# NB internally factors always start at 1
# NB if levels is not specified, levels will be the unique 
# values of the vector, sorted alphabetically
text.pain = c("none", "severe", "medium", "medium", "mild")
factor(text.pain)
# they are placed in an order that makes no sense

#### 2.6 Further Help ####

browseURL("https://www.datacamp.com/courses/free-introduction-to-r")
browseURL("http://tryr.codeschool.com")
library(swirl)
swirl()
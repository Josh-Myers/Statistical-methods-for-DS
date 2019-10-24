#### Intro2R: Data Manipulation Pt 1 ####

# Source: Peter Dalgaard, Introductory Statistics with R
rm(list=ls())
load("ForNextTime.RData")

intake = d
colnames(intake) = c("pre", "post")
rm(list=c("d", "x","energylist"))

# If you don't have the Rdata file from last time use this cose
library(ISwR)
attach(intake)
data(intake)
intake.post = intake$post
intake.pre = intake$pre

#### 4.1 Indexing ####

# Extract premenstrual intake for woman number 5

intake.pre[5]

# Extract  the Data for women 3, 5, and 7 
# - we must define these as a vector

intake.pre[c(3,5,7)]

# Index by a stored variable

index = c(1:4,7)
intake.pre[index]

# using a:b notation, extract the first 5 variables

intake.pre[1:5]

# negative indexing -  select all observations 
# except 3, 5, and 7

intake.pre[-c(3,5,7)]


#### 4.2 Conditional Selection ####

# Select the post-menstrual energy intake for the women who
# had premenstrual intake greater than 7000kJ

intake.post[intake.pre>7000]

# For women with premenstrual intake between 7000 and 8000kJ
intake.post[intake.pre>7000 & intake.pre<8000]

# UNDER THE HOOD --- R picks out the values where the logical 
# vector is TRUE

intake.pre>7000

#### 4.3 Indexing Data Frames #### 

# from "intake" select the "pre" measurement for woman number 5

intake$pre[5]
intake[5,1]
intake[5,"pre"]

# extract all data for woman number 5
intake[5,]

# extract all data for women with premenstrual intake of
# over 7000kj

intake[intake$pre>7000,]


#### Further Help ####

browseURL("https://www.datacamp.com/courses/free-introduction-to-r")
browseURL("http://tryr.codeschool.com")
library(swirl)
swirl()




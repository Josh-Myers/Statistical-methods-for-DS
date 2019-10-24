#### Intro2R: 1 First Steps ####

# Source: Peter Dalgaard, Introductory Statistics with R

rm(list=ls()) #clears workspace ... use wisely
install.packages("ISwR")
library(ISwR)


#### 1.1 An overgrown calculator ####

# Simple Arithmetic
2+2
2*2
2/2
2-2

# Simple functions


#### 1.2 Assignments ####

x = 2
x
x <- 5
x+x


#### 1.3 Vectorised Arithmetic ####

# BMI Example

weight = c(60,72,57,90,95,72) 
weight
height = c(1.75, 1.8, 1.65, 1.9, 1.74, 1.91)

bmi = weight/height^2
bmi

# Computing Mean and Standard Deviation

sum(weight)
sum(weight)/length(weight)
xbar = sum(weight)/length(weight) #Mean
weight - xbar
(weight - xbar)^2
sum((weight - xbar)^2)
sqrt(sum((weight - xbar)^2)/(length(weight)-1)) #Standard deviation

# OR

mean(weight)sd(weight)

#### 1.4 Standard Procedures ####

t.test(bmi, mu=22.5) # tests whether our data deviates
                     # from the hypothesised distribution

#### 1.5 Plotting ####

plot(height, weight) #plots x vs y
plot(height, weight, pch=2) # plotting character

#### 1.6 Helpful Resources ####

# Swirl
install.packages("swirl")
library(swirl)
swirl()
browseURL("http://www.swirlstats.com")

# Rstudio
browseURL("https://www.rstudio.com/online-learning/")
browseURL("https://www.datacamp.com/courses/free-introduction-to-r")
browseURL("http://tryr.codeschool.com/levels/1/challenges/2")

# Hadley Wickham's Free Book -- READ THIS IF YOU WANT TO WRITE ELEGANT, COMPATIBLE CODE
browseURL("http://r4ds.had.co.nz/explore-intro.html")
# Murray Logan's R Resources
browseURL("http://www.flutterbys.com.au/stats/course.html")



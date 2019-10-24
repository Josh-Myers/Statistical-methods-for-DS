#### Intro2R: Introduction to Graphics ####

attach(mtcars)
mtcars=mtcars

#### 1 Histogram ----

hist(mpg)

hist(mpg, 
     breaks=12, 
     col="red",
     xlab = "Miles Per Gallon",
     main = "Frequency of Mileage Per Gallon")

# For a density plot
hist(mpg, freq = F,
     breaks=12, 
     col="red",
     xlab = "Miles Per Gallon",
     main = "Density of Mileage Per Gallon")
curve(dnorm(x, mean=mean(mpg), sd=sd(mpg)), add=TRUE, col='darkblue', lwd=2) 

#### 2 Scatterplot w/ Line of Best Fit ----

plot(hp,mpg)

plot(hp,mpg, 
     main = "Car Mileage by Horsepower",
     ylab = "Miles Per Gallon",
     xlab = "Horsepower",
     pch = 17,
     col = "darkcyan")
abline(lm(mpg~hp))

# Plotting Characters
# http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
# Colours
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour

#### 3 Boxplot ----

boxplot(mpg~as.factor(cyl))

boxplot(mpg~cyl,
        main = "Car Mileage by Number of Cylinders",
        ylab = "Miles Per Gallon",
        xlab = "Number of Cylinders",
        col = c("red", "darkblue", "gold"))


#### 4 Line Chart ----

months = 1:12
sales.cars = sample(20:25, 12, replace=T) + 1:12
sales.trucks = sample(10:15, 12, replace=T)

plot(months, sales.cars, 
     ylim = c(0,50), type = "l",
     xlab = "Months",
     ylab = "Sales",
     main = "Car and Truck Sales")
lines(sales.trucks, lty=2)
legend(x=1,y=50, c("Cars", "Trucks"), lty=c(1,2))


#### Combining Plots ----

par(mfrow=c(2,2))
# Density 
hist(mpg, freq = F,
     breaks=12, 
     col="red",
     xlab = "Miles Per Gallon",
     main = "Frequency of Mileage Per Gallon")
curve(dnorm(x, mean=mean(mpg), sd=sd(mpg)), add=TRUE, col='darkblue', lwd=2) 
# Scatterplot
plot(hp,mpg, 
     main = "Car Mileage by Horsepower",
     ylab = "Miles Per Gallon",
     xlab = "Horsepower",
     pch = 20,
     col = "darkcyan")
abline(lm(mpg~hp))
# Boxplot
boxplot(mpg~cyl,
        main = "Car Mileage by Number of Cylinders",
        ylab = "Miles Per Gallon",
        xlab = "Number of Cylinders ",
        col = c("red", "darkblue", "gold"))
# Line Chart
plot(months, sales.cars, 
     ylim = c(0,60), type = "l",
     xlab = "Months",
     ylab = "Sales",
     main = "Car and Truck Sales"
)
lines(sales.trucks, lty=2)
legend(x=1,y=60, c("Cars", "Trucks"), lty=c(1,2), cex = 0.6)


#### Further Help ----

# Base R
# https://www.datacamp.com/courses/data-visualization-in-r

# ggplot2
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# https://www.rstudio.com/resources/cheatsheets/

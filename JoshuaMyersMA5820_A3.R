# Joshua Myers MA5820 assessment 3: R code

# import packages----
library(tidyverse)
library(cowplot)

# question 1----
# The lifetime of a particular type of TV follows a normal distribution: mean = 4800 hours, and standard deviation (sd) = 400 hours.
mean = 4800
sd = 400

# a. Find the probability that a single randomly-chosen TV will last less than 4500 hours
x = 4500
prob_x = pnorm(x, mean = mean, sd = sd, lower.tail = TRUE) # lower.tail = TRUE because want prob of <4500
print(prob_x)

# b. Find the probability that the mean lifetime of a random sample of 16 TVs is less than 4500 hours
# standard error = sd/sqrt(n)
y = 4500
se = sd / sqrt(16)
print(se)
prob_y = pnorm(y, mean = mean, sd = se, lower.tail = TRUE) # lower.tail = TRUE because want prob of <4500
print(prob_y)

# c. Compare answers from a and b.
# prob.x = 23% and prob.s = 0.1%. The probability that a single randomly-chosen TV will last <4500 hrs is much higher than the probability
# that the mean of a random sample of 16 TVs would be <4500. 

# question 2----
# Beta endorphins are morphine like substances produced by the body. They create a sense of well-being. It has been proposed that 
# Beta endorphins increase with exercise. Test this hypothesis using the data in beta.csv which has Beta endorphin levels for 10 people 
# measured for each person pre- and post exercise. Using this sample, test if Beta endorphins increase with exercise. Adopt a 5% risk of 
# committing a type I error.

# read data into R
# data is in a csv file called "beta.csv", which I have put in my working directory
beta_df = read.csv('beta.csv')
# check that data has loaded correctly, and get overview of data types, which are all numeric
View(beta_df)
str(beta_df)

# exploratory data analysis
# density plot of dif 
beta_density = beta_df %>% 
  ggplot(aes(x = dif)) +
  geom_density(colour='lightblue', fill='lightblue') +
  scale_x_continuous(limits = c(0, 100), labels = c(0, 25, 50, 75, 100)) 
# there is an extreme outlier, but relatively normal apart from this

# qq plot
beta_qq = beta_df %>% 
  ggplot(aes(sample=dif)) +
  stat_qq() +
  stat_qq_line()
# there is one extreme outlier, however, I do not wish to remove it because it may be a genuine result

# save plots
beta_plots = plot_grid(beta_density, beta_qq, nrow = 1, labels = c("A", "B"))
save_plot('beta_plots.png', beta_plots, base_aspect_ratio = 2, base_height=5, base_width=NULL)

# statistical testing
# I will use a non-parametric method
w_test = wilcox.test(beta_df$dif, mu=0, alternative = "greater") 
print(w_test)
# mu is H0 median
# greater because we hypothesise diff is positive
# there strong evidence that the median diff is not 0

# question 3----
# The dataset PlantGrowth is a datafile in the R datasets. If you type `PlantGrowth' from the R prompt you will see the data. For more 
# information about this data type help (PlantGrowth) from the R prompt. These data are obtained from an experiment to compare plant yields
# under a control and two other treatments. Test if there is a difference between the control group and treatment 2 on mean plant yields.
# assign dataset to a variable and examine
data(PlantGrowth)
View(PlantGrowth)
str(PlantGrowth)

# box plot 
plant_box = PlantGrowth %>% 
  ggplot(aes(x = group, y = weight, fill=group, colour=group)) +
  geom_jitter(width=0.16) +
  geom_boxplot(colour='black', alpha=0.4) 

# qqplot plot
plant_qq = PlantGrowth %>% 
  ggplot(aes(sample=weight, colour=group)) +
  stat_qq() +
  stat_qq_line()
# looks approximately normal, there are a couple of outliers in trt1, but we are not interested in that group

# save plots
plant_plots = plot_grid(plant_box, plant_qq, nrow = 1, labels = c("A", "B"))
save_plot('plant_plots.png', plant_plots, base_aspect_ratio = 2, base_height=6, base_width=NULL)

# remove trt1 for this test
plant_df = PlantGrowth %>% 
  filter(group != 'trt1')

t_test = t.test(weight~group, plant_df)
print(t_test)


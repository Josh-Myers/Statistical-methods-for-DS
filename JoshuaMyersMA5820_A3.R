# assessment 3: R code

# import packages----
library(ggplot2)

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

# a. read data into R
# data is in a csv file called "beta.csv", which I have put in my working directory
beta_df = read.csv('beta.csv')
# check that data has loaded correctly, and get overview of data types, which are all numeric
str(beta_df)

# b. exploratory data analysis
# put data in long form for the boxplot plot
beta_df_long = beta_df %>% 
  pivot_longer(cols = c(pre, post), names_to = 'state') 
# make pre the first level - plot makes more sense if pre is first on x-axis
beta_df_long$state = factor(beta_df_long$state, levels = c('pre', 'post'))

# density plots of each variable (pre, post, dif)
beta_density = beta_df_long %>% 
  ggplot(aes(x = value, colour=state, fill=state)) +
  geom_density(alpha=0.6) +
  scale_x_continuous(limits = c(0, 100), labels = c(0, 25, 50, 75, 100)) 
# pre looks fairly normal, and post also apart from extreme outlier
# variance for post looks higher than pre, however

# qq plot
beta_qq = beta_df_long %>% 
  ggplot(aes(sample=value, colour=state)) +
  stat_qq() +
  stat_qq_line()
# overall not too bad, apart from extreme outlier for post

# box plot 
beta_box = beta_df_long %>% 
  ggplot(aes(x = state, y = value, fill=state, colour=state)) +
  geom_jitter(width=0.16) +
  geom_boxplot(colour='black', alpha=0.4) +
  scale_y_log10() # put log axis to better see distribution of non-outliers
# there is very strong separation between the groups

# statistical test of normality 
shapiro.test(beta_df$pre) # not significant (p = 0.5), no evidence that not normal 
shapiro.test(beta_df$post) # significant (0 < 0.0001), evidence of deviation from normality

# test for equal variance
bartlett.test(value~state, beta_df_long) # significant, evidence of unequal variance

# there is one extreme outlier, however, I do not wish to remove it because it may be a genuine result
# therefore, I will analyse with non-parametric method
# Wilcoxon matched-pairs signed-ranks test, to test if there is a statistically significant difference between pre and post
wilcox.test(beta_df$pre, beta_df$post, paired = TRUE, alternative = "less") # less because we hypothesise pre - post < 0
# strong evidence of an effect

# could also do single sample test using dif (is it >0)
wilcox.test(beta_df$dif, alternative = "greater") # greater because we hypothesise diff is positive (post - pre)

# question 3
plant_df = PlantGrowth
summary(plant_df)

plant_density = plant_df %>% 
  ggplot(aes(x = weight, colour=group, fill=group)) +
  geom_density(alpha=0.5) 

# qq plot
plant_qq = plant_df %>% 
  ggplot(aes(sample=weight, colour=group)) +
  stat_qq() +
  stat_qq_line()

# box plot 
plant_box = plant_df %>% 
  ggplot(aes(x = group, y = weight, fill=group, colour=group)) +
  geom_jitter(width=0.16) +
  geom_boxplot(colour='black', alpha=0.4) 

bartlett.test(weight~group, plant_df) # not significant - no evidence of unequal variance
shapiro.test(plant_df$weight)

f = lm(weight ~ group, data = plant_df)
anova(f)
summary(f) # The interpretation of the estimates is that the intercept is the mean in the first group (ctrl), 
# whereas the two others describe the difference between that group and the first one.

an = aov(weight ~ group, data = plant_df)
summary(an)

pairwise.t.test(plant_df$weight, plant_df$group, p.adj = "bonf")
# evidence that mean trt2 is different trt1
# no evidence that mean trt2 is diff ctrl or ctrl is diff trt1



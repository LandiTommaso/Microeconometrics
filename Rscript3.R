

# A regional government funds a special program that allows primary schools to
# activate a training course for their students, with no extra costs for the family
# and the institution. The goal of the training program is to improve the students' 
# ability with logic and mathematics.

# In a large city, there are 50 primary schools. Some of them participated in the program,
# while others were not interested. To see if the training is effective, we collected a 
# random sample of sixth-graders from each school (the sample size being proportional to the number
# of pupils in the school), and administered them a logic test.

# The dataset "d3.csv" includes the following quantities:

# school (an identifier between 1 and 50)
# training = 0/1. Does the school participate in the program?
# sex of the student (f/m)
# math = average grade in math BEFORE the start of the program (60-100)
# test = the score achieved at the test (0-100)

# Q0. Load the data and attach them! Install and load "mlmRev" and "lmtest".
setwd("C:/Users/a020554/OneDrive - University of Pisa/Desktop/didattica/Topics in microeconometrics/000 data_and_rscript_for_labs")
d <- read.csv("d3.csv", sep = ";", dec = ",") 
attach(d)

library(mlmRev)
library(lmtest)

# Q1. Compute the number of students sampled from each school, and represent it graphically.
barplot(sort(table(school)))

# Q2. Show how the test score changes across different schools.
boxplot(test ~ school)
# Huge variability BETWEEN schools, much more than there is variability WITHIN each school!
barplot(sort(tapply(test, school, median)), ylab = "median test score", xlab = "school")

# Q3. Describe graphically the association between "math" and "test", using a different color for each school.
  # Also, for each school, draw a regression line.

cc <- rainbow(50)
plot(math, test)
for(i in 1:50){
  w <- which(school == i)
  points(math[w], test[w], col = cc[i])
  abline(lm(test[w] ~ math[w]), col = cc[i])
}

# The graph appears pretty useless. However, it tells me that I can reasonably assume
  # that schools only differ by an individual INTERCEPT, and that no individual slope of "math" 
  # is needed (as all lines are "essentially parallel").
# When you do fixed-effects model, you ALWAYS include an individual intercept, and you NEVER
  # include individuals slopes. Have you ever wondered if this is correct?


# Q4. Compare the test results when training = 0 vs training = 1, 
 # using t-test and wilcoxon test.

t.test(test ~ training)
wilcox.test(test ~ training)
# significant differences

# Q5. Fit a linear model "m1" in which you show how the test result depends 
  # on the following predictors: sex, math, training.

m1 <- lm(test ~ sex + math + training)
summary(m1) # training: + 14 points on average

# Q6. Add the school using a fixed-effects approach. Briefly comment.
m2 <- lm(test ~ sex + math + training + factor(school))
summary(m2) # training: + 16.5 points on average

# Q7. Is there a significant "school effect", i.e., a systematic difference across schools? Perform a suitable test.
summary(aov(m2)) # it works because I am doing lm
lrtest(m1, m2) # always works, also for GLMs etc

# Q8. Add the school using a random-effects approach. Briefly comment.
a <- lmer(test ~ sex + math + training + (1 | school))
summary(a)

# var(alpha_i) = 81.46
# training: + 13.7 points on average

# Q9. Compute the variance of the "test" variable; then, compute the same quantity, separately for each school.
  # What do you see, and why? In particular, what would happen if all data came from the same school?
  # And if only one student per school was available?

var(test)
tapply(test, school, var)

# The variance of the test, which is the "total" variance, is large.
# Within each school, the variance is MUCH, MUCH SMALLER.

# This means that most of the variability is BETWEEN, and little is WITHIN.
# If all data came from the same school, I could only see the WITHIN part of the variance.
# If only one student per school was available, I could only see the TOTAL one.

# Q10. Create a new variable "goodtest" which is 1 if the test is above the 3rd quartile, and zero otherwise.
goodtest <- (test > quantile(test, 0.75))

# Q11. Model "goodtest" with a random-intercept logistic regression.
A <- glmer(goodtest ~ sex + math + training + (1 | school), family = binomial)
summary(A)










# The dataset "d1.csv" refers to an experiment conducted on a random sample of n = 476 individuals.
# The experiment combines neurosciences with decision theory to implement marketing strategies.
# The experiment proceeds as follows:
  # Each subject is asked to wait for 20 minutes in a room. In reality, the experiment has already started.
  # The waiting room is empty, except for a shelf where the subject finds 20 different products, that include
     # snacks, books, jewelry, toys, pottery, clothes, and a variety of tools (e.g., technology, gardening, sport).
  # The subject is alone and will soon get bored and start examining the objects. 
     # An eye tracker will tell how much time is spent on each item on display.
     # Objects that capture attention for longer are deemed more "successful" from a marketing standpoint.

# The data include:
  # age of the subject (in years)
  # sex (f/m)
  # education (low/medium/high)
  # t1, ..., t20: the total time (in seconds) spent on each item


# Q0. Load the data and attach them!
d <- read.csv("d1.csv", sep = ";", dec = ",") #sep is the separeted comma while dec stands for the decimal in the original csv (which was a comma)
head(d)
attach(d) #gives the possibility to have variables ready without $sign



# Q1. Illustrate graphically the distribution of age, sex, and education.

hist(age)
plot(density(age))
hist(age, freq = FALSE); lines(density(age))

pie(table(sex)) #pie is similar to barplot. pie always represents frequencies

barplot(table(edu), col = c("red", "orange", "yellow"), ylab = "frequency", main="level of \n education") #always put the c before starting listing something


# Q2. Use a boxplot and a barplot to illustrate graphically the association between age and education.

boxplot(age ~ edu)
tapply(age, edu, mean) # tapply(to_what, by_what, what)
tapply(age, edu, median)
barplot(tapply(age, edu, median), ylab = "median age", xlab="education")

# Q3. Compute the mean age, its range, and the quartiles.
mean(age)
range(age)

quantile(age) # default: min, quartiles, max
quantile(age, c(0.25, 0.5, 0.75))
summary(age) # same as quantile, + mean

# Q4. Compute the proportion of subjects with high education.
mean(edu == "3-high") #proportion of a binary var is his mean. we use double equal to ask! single equal just assigns
prop.table(table(edu)) #prop table for rel frequencies
table(edu)/nrow(d)

# Q5. Compute the median age, separately in males and females.

# NO:
median(age[sex == "f"])
median(age[sex == "m"])

# YES:
tapply(age, sex, median)

# Q6. Compute the proportion of subjects with t1 > 30 among males.
mean(t1[sex == "m"] > 30) #square brackets to subset
tapply(t1 > 30, sex, mean)

# Q7. Compute the proportion of subjects with t5 > t8 among females.
mean(t5[sex == "f"] > t8[sex == "f"]) # inefficient
tapply(t5 > t8, sex, mean) # better

# Q8. Compute the proportion of subjects with the following characteristics: 
  # age > 30, female, edu < high.

#we use a logical command. 
mean(age > 30 & sex == "f" & edu != "3-high") # != stands for different
#we use a different method.
mean((age > 30) * (sex == "f") * (edu != "3-high"))


# Q9. Compute the proportion of subjects with at least one of the following characteristics: 
  # age > 30, female, edu < high.
mean(age > 30 | sex == "f" | edu != "3-high") # in probability, P(A | B) means "given that", not "or". Very bad.
mean((age > 30) + (sex == "f") + (edu != "3-high") >= 1)
1 - mean(age <= 30 & sex != "f" & edu == "3-high")

# Q10. Compute the proportion of subjects with at least two of the following characteristics: 
  # age > 30, female, edu < high, t1 < median, t2 > median.
mean((age > 30) + (sex == "f") + (edu != "3-high") + (t1 < median(t1)) + (t2 > median(t2)) >= 2)

# If you prefer:
n.conditions <- (age > 30) + (sex == "f") + (edu != "3-high") + (t1 < median(t1)) + (t2 > median(t2))
table(n.conditions)
mean(n.conditions >= 2)

# Q11. Represent graphically the distribution of t1 in males and females. 
  # Compare groups using a parametric and a nonparametric test.

boxplot(t1 ~ sex) # or check Q40

t.test(t1 ~ sex)# PARAMETRIC. COMPARES MEANS (2 groups). Differences are significant
# t test compares two means, ANOVA compares k>2 means where in the H0 all the means are equals

wilcox.test(t1 ~ sex) # NONPARAMETRIC. COMPARES RANKS (2 groups). Differences are significant. 
#non param test use RANK i.e. the position in the sample. Outliers are not anymore a problem!

# t-test assumes normality, no outliers, optionally homoskedasticity.
# Its performance will break if there are strong deviations from normality or outliers.

# Mann-Whitney-Wilcoxon rank-sum test is insensitive to these assumptions.
#param test -> non param test.:
#correlation test pearson-> spearman rank corr test
# ttest->wilcoxon test
#anova->kruskal-wallis


# Q12. Represent graphically the distribution of t1 in each education group. 
  # Compare groups using a parametric and a nonparametric test.
boxplot(t1 ~ edu)
summary(aov(t1 ~ edu)) # PARAMETRIC, compare MEANS (2 or more groups). Not significant
kruskal.test(t1 ~ edu) # NONPARAMETRIC, compare RANKS (2 or more groups). Not significant

# ANOVA assumes normality, no outliers, and homoskedasticity.
# Its performance will break if there are strong deviations from normality or outliers,
  # or if the variance is not the same in all groups. With two groups only, ANOVA
  # reduces to t-test with assumption of equal variances.

# Kruskal-Wallis test is insensitive to these assumptions.
  # It reduces Mann-Whitney-Wilcoxon rank-sum test if there are only two groups.

# Q13. Compare t1 in high- vs low-educated subjects, using a parametric and a nonparametric test.
t.test(t1[edu == "1-low"], t1[edu == "3-high"]) #parametric
wilcox.test(t1[edu == "1-low"], t1[edu == "3-high"]) #non param
# no significant differences

pairwise.t.test(t1, edu, p.adjust= "none", pool.sd = FALSE)
pairwise.wilcox.test(t1, edu, p.adjust= "none")

# Q14. Compare t1 in high- vs (low + medium)-educated subjects, using a parametric and a nonparametric test.

t.test(t1[edu != "3-high"], t1[edu == "3-high"])
wilcox.test(t1[edu != "3-high"], t1[edu == "3-high"])

# or

t.test(t1 ~ (edu == "3-high"))
wilcox.test(t1 ~ (edu == "3-high"))

#still no significant differences

# Q15. Is there an association between sex and edu? 
  # Create a contingency table; compute proportions, conditioning by rows, then by columns.
  # Perform a suitable test.

table(sex, edu) # absolute frequencies
prop.table(table(sex, edu)) # relative frequencies, total (sum = 1)
prop.table(table(sex, edu), margin = 1) # relative frequencies, by row (row sum = 1)
prop.table(table(sex, edu), margin = 2) # relative frequencies, by column (col sum = 1)
chisq.test(sex, edu) # not significant where H0 : no association
# Note: chi-squared test is non-parametric, and it cannot be otherwise: with categorical
  # variables, we cannot even speak of homo- or hetero-skedasticity, normality, or outliers.

# Q16. Represent graphically the association between t1 and t2. 
  # Compute Pearson's and Spearman's correlation coefficients, perform correlation test.

plot(t1, t2); abline(lm(t2 ~ t1)) # plot(t2 ~ t1)

cor(t1, t2) # Pearson's linear correlation coefficient. Assumes linearity, no outliers.
cor(t1, t2, method = "spearman") # Pearson's linear correlation coefficient. Assumes monotonicity.

cor.test(t1, t2) # Pearson linear corr is Parametric where H0: no association
cor.test(t1, t2, method = "spearman") # Nonparametric, use ranks


# Q17. Represent graphically the association between t2 and t4, 
  # using different colors for males and females. Add a legend.

plot(t2, t4)
points(t2[sex == "m"], t4[sex == "m"], col = "red")
legend("topleft", legend = c("f", "m"), pch = 1, col = c("black", "red"))

# Q18. Create a new variable "agecat1" that categorizes age into 4 groups: 
  # <= 25, 25-40, 40-65, > 65.
agecat1 <- cut(age, c(0, 25, 40, 65, Inf))
table(agecat1)

# Q19. Create a new variable "agecat2" that categorizes age into 4 groups 
  # with approximately the same numerosity.
agecat2 <- cut(age, c(0, quantile(age, c(0.25,0.5,0.75)), Inf))
table(agecat2)

# Q20. Create a new variable "agecat3" that categorizes age into 4 evenly-spaced groups.
agecat3 <- cut(age, 4)
table(agecat3)

# Q21. Is there anybody with age >= 80? If yes, in which position(s)?
any(age >= 80)
which(age >= 80)

# you may also ask:
all(age < 80)

# Q22. Create a data.frame "T" that only includes t1, ..., t20.
T <- d[, 4:23] # or T <- d[4:23], that works because "d" is a data.frame
T <- d[substr(names(d),1,1) == "t"] #logical commands that select firs character of the column is "t"
T <- d[grep("t", names(d))] # grep = positions where to find a string
 # or use "grepl", which is the same but returns TRUE/FALSE
T <- d[paste0("t", 1:20)] #with paste zero there is no space between them

# Q23. For each variable in "T", create a histogram and export it in .jpg format.

for(i in 1:ncol(T)){
  jpeg(paste0("fig", i, ".jpg"))
  hist(T[,i], col = "blue") #,i stands for colum i of object T
  dev.off()
}

# Q24. For each pair of variables in "T", create and export a scatterplot 
  # with superimposed regression line. In the title of the plot ("main"), 
  # report the Pearson's linear correlation coefficient and the associated p-value.

myplot <- function(x,y, xlab, ylab){
  r <- cor.test(x,y)
  p <- round(r$p.value, 3)
  r <- round(r$estimate, 3)
  title <- paste0("r = ", r, " (p = ", p, ")")
  plot(x, y, main = title, xlab = xlab, ylab = ylab)
  abline(lm(y ~ x))
}

myplot(t13, t17, xlab = "t13", ylab = "t17") # for example
myplot(t15, t17, xlab = "t15", ylab = "t17")

for(i in 1:(ncol(T) - 1)){
  labs <- names(T)
  for(j in (i + 1):ncol(T)){
    jpeg(paste0("fig", i, "_vs_", j, ".jpg"))
    myplot(T[,i], T[,j], xlab = labs[i], ylab = labs[j])
    dev.off() #stands for i finish the graph
  }
}


# Q25. Compute the mean of each variable in "T".
colMeans(T)

apply(T, 2, mean)
sapply(T, mean)

# "apply" is a generic command, that can be applied to a data.frame or to a matrix.
  # The second argument tells in which direction to proceed: 1 = by row, 2 = by column.
# "sapply" only works with a list or a data.frame, by column. 

# Q26. Create a new function "f1" that, given an input "x", 
  # returns the mean and the median value of x. Apply f1 to all columns of "T".

f1 <- function(x){c(Mean = mean(x), Me = median(x))}
f1(t7) # just an example to check if it works

apply(T, 2, f1)
round(t(apply(T, 2, f1)), 2) # round(x, digits); t = transpose


sapply(T, f1)
round(t(sapply(T, f1)), 2) # round(x, digits); t = transpose

# of course, if you want to impress someone, you can also do:
round(t(apply(T, 2, function(x){c(Mean = mean(x), Me = median(x))})), 2)


# Q27. Create a new function "f2" that, given an input "x", 
  # returns the mean of x in males and females. Apply f2 to all columns of "T".
f2 <- function(x){tapply(x, sex, mean)}
f2(t7) # just an example to check if it works
round(t(apply(T, 2, f2)), 2)
round(t(sapply(T, f2)), 2)

# Q28. Compute the first and third quartile of t3 in each combination of (sex & edu).
tapply(t3, paste0(sex, edu), quantile, probs = c(0.25,0.75))
t(sapply(tapply(t3, paste(sex, edu), quantile, probs = c(0.25,0.75)), I))

# "tapply" will not make any attemp to simplify the output in a nice format.
# "sapply" will do this, if possible. The function "I" is the identity,
  # and does absolutely NOTHING (which makes sense: you don't want to change
  # the output, you only want to change its format!!)

# Q29. Create a new variable "score" defined as follows:
  # Start from 0.
  # For each year of age, add 0.1 in females, and 0.12 in males.
  # If edu = "high", add 2 when age < 65, and 3 when age >= 65.

score <- age*(0.1 + 0.02*(sex == "m")) + (edu == "3-high")*(2 + 1*(age >= 65))
hist(score)

# Q30. Create a new data.frame "T1" in which each variable in "T" 
  # is replaced by the indicator (t_j < median).

T1 <- sapply(T, function(x) x < median(x)) + 0
# I add 0 to convert TRUE/FALSE into 1/0. This works because, when
  # you combine logical values with numerical ones, the output is numerical.

# Q31. For each individual, compute the number of items he/she looked at 
  # for a time longer than the median.
rowSums(T1)


# Q32. Consider the first 5 variables in "T1". In theory, how many different patterns 
  # could be observed?  And in practice, how many unique patterns do you see, 
  # and with which frequency?

# Answer: in theory, 2^5 possible patterns.
# In practice: 
u <- table(paste0(T1[,1], T1[,2], T1[,3], T1[,4], T1[,5]))
u # observed patterns, with their frequency

length(u) # 29 unique pattern


# Q33. For each individual, compute the maximum amount of time dedicated 
  # to any item (i.e., max(t1, ..., t20)).

# There is not a function "rowMax". Use "apply" (not "sapply", because you must proceed by row!)
apply(T, 1, max)

# Q34. For each individual, compute the total time dedicated to the examination of the available items.
rowSums(T) # or apply(T, 1, sum)
apply(T, 1, sum)
# Q35. For each variable (t1, ..., t20), compute the p-value of a t-test that compares males and females.
sapply(T, function(x) t.test(x ~ sex)$p.value)
round(sapply(T, function(x) t.test(x ~ sex)$p.value),3) # maybe you could round to 3 digits
# or apply(T, 1, ...)

# Q36. Create a new function "g" that, for any input "x", returns -1 if x < median, +1 if x > median, and 0 if x = median.
g <- function(x){sign(x - median(x))} #sign funzione che converte valori pos in 1 e neg in 0 e zero in 0
g(t1) # for example

# Q37. Which item has received the lowest and the largest total examination time?
which.min(colSums(T)) # number 6 Restituisce il nome/indice della colonna con la somma più bassa
which.max(colSums(T)) # number 4 nome somma piu alta

# Q38. Is there a significant association between age and education? Perform a suitable test.
boxplot(age ~ edu)
kruskal.test(age ~ edu) # significant al posto dell'anova. pvalue piccolo.

# Q39. Is there a significant association between agecat2 and education? Perform a suitable test.
chisq.test(agecat2, edu) # significant

# Q40. Create a density plot of t1, separately in males and females (two lines on the same plot, using different colors). Add legend.
  # Hint: use "lines" to add lines to an existing plot.

plot(density(t1[sex == "f"]), col = "red", xlab = "t1", main = ""); lines(density(t1[sex == "m"]), col = "blue")
legend("topright", legend = c("Females", "Males"), lty = 1, col = c("red", "blue"), cex = 0.4)
# Play with xlab, ylab, main, xlim, ylim, e.g., xlab = "something", ylim = c(0, 0.5), main = "whatever"

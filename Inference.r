data(iris)

# **********************
# Part 1
# **********************

# create graph layout
par(mfrow = c(2, 3))

# petal length histograms

# setosa histogram
hist(iris$Petal.Length[iris$Species == "setosa"],
     main = "Setosa", xlab = "Petal Length", col = "blue",
     breaks = 5, xlim = c(0,3))

# versicolor
hist(iris$Petal.Length[iris$Species == "versicolor"],
     main = "Versicolor", xlab = "Petal Length", col = "green",
     breaks = 10, xlim = c(2,6))

# virginica
hist(iris$Petal.Length[iris$Species == "virginica"],
     main = "Virginica", xlab = "Petal Length", col = "red",
     breaks = 10, xlim = c(4,7))

# petal width histograms

# setosa
hist(iris$Petal.Width[iris$Species == "setosa"],
     main = "Setosa", xlab = "Petal Width", col = "blue",
     breaks = 10, xlim = c(0,1))

# versicolor
hist(iris$Petal.Width[iris$Species == "versicolor"],
     main = "Versicolor", xlab = "Petal Width", col = "green",
     breaks = 10, xlim = c(0,2.5))

# virginica
hist(iris$Petal.Width[iris$Species == "virginica"],
     main = "Virginica", xlab = "Petal Width", col = "red",
     breaks = 10, xlim = c(0,2.5))

# Reset layout
par(mfrow = c(1, 1))


### Comments on the distributions
# Since the sample statistics have unknown population variances and the sample sizes are larger than 30, they follow a normal distribution. 
# Petal length parameters (aproximations from histogram)
# * Setosa
# ** Sample Mean      = 1.5
# ** Sample Variance  = 0.5
# * Versicolor
# ** Sample Mean      = 4.25
# ** Sample Variance  = 0.45
# * Virginica
# ** Sample Mean      = 5.5
# ** Sample Variance  = 0.5
# Petal width parameters
# * Setosa
# ** Sample Mean      = 0.25
# ** Sample Variance  = 0.1
# * Versicolor
# ** Sample Mean      = 1.3
# ** Sample Variance  = 0.2
# * Virignica
# ** Sample Mean      = 2
# ** Sample Variance  = 0.25


# **********************
# Part 2
# **********************

# function to calculate the confidence interval on the mean of a normal distribution with an unknown variance
confidence_interval_unknown_variance <- function(sample, confidence = 0.95) {
  n <- length(sample)
  sample_mean <- mean(sample)
  sample_sd <- sd(sample)
  a <- 1 - confidence
  z <- qnorm(1 - a/2,0,1)
  margin_of_error <- z * sample_sd / sqrt(n)
  lower_bound <- sample_mean - margin_of_error
  upper_bound <- sample_mean + margin_of_error
  return(c(lower_bound, upper_bound))
}

# **********************
# Part 3
# **********************

# calculate the confidence interval using the function
setosa_ci <- confidence_interval_unknown_variance(iris$Petal.Length[iris$Species == "setosa"])
versicolor_ci <- confidence_interval_unknown_variance(iris$Petal.Length[iris$Species == "versicolor"])
virginica_ci <- confidence_interval_unknown_variance(iris$Petal.Length[iris$Species == "virginica"])

# print results
print("Confidence Interval for Petal Length (Setosa):")
cat('Lower Bound:', setosa_ci[1], "\n")
cat('Upper Bound:', setosa_ci[2], "\n")

print("Confidence Interval for Petal Length (Versicolor):")
cat('Lower Bound:', versicolor_ci[1], "\n")
cat('Upper Bound:', versicolor_ci[2], "\n")

print("Confidence Interval for Petal Length (Verginica):")
cat('Lower Bound:', virginica_ci[1], "\n")
cat('Upper Bound:', virginica_ci[2], "\n")

# **********************
# Part 4
# **********************

# confidence intervals
# Setosa has confidence intervals of [1.41, 1.51], which means that with 95 percent confidence the populaiton mean lies within these bounds.
# Versicolorhas confidence intervals of [4.14, 4.39], which means that with 95 percent confidence the populaiton mean lies within these bounds.
# Verginica has confidence intervals of [5.4, 5.7], which means that with 95 percent confidence the populaiton mean lies within these bounds.

# **********************
# Part 5
# **********************

sample1 = iris$Petal.Length[iris$Species == "virginica"]
sample2 = iris$Petal.Length[iris$Species == "versicolor"]
conf_level <- 0.95


mean_hypothesis_test <- function(sample1, sample2, conf_level = 0.95) {
  n1 <- length(sample1)       # size of sample1
  n2 <- length(sample2)       # size of sample2
  mean1 <- mean(sample1)      # sample mean for sample1
  mean2 <- mean(sample2)      # sample mean for sample2
  sd1 <- sd(sample1)          # sample sd for sample1
  sd2 <- sd(sample2)          # sample sd for sample2
  
  ### check for equality in variances by testing H0: σ1=σ2 : H1: σ1~=σ2
  # test statistic
  t_stat <- (sd1^2)/(sd2^2)
  # significance level
  alpha <- 1 - conf_level
  # p value
  p_value <- 2*(1-pf(t_stat,n1,n2))
  
  # sigma values are unknown and equal
  if (p_value > alpha) {
    print("σ1=σ2")
    # degrees of freedom
    df <- n1 + n2 - 2
    # pooled valiance
    Sp <- ((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1 + n2 - 2)
    # observed value
    T_obs <- (mean1 - mean2)/(sqrt(Sp*((1/n1) + (1/n2))))
    # p value
    pt_value <- pt(T_obs, df)
    
    # sigma values are unknown and unequal
  } else if (p_value < alpha) {
    print("σ1~=σ2")
    # degree of freedom
    gamma <- (((sd1^2/n1) + (sd2^2/n2))^2) / (((sd1^2/n1)^2 / (n1 - 1)) + ((sd2^2/n2)^2 / (n2 - 1)))
    # observed value
    T_obs <- (mean1 - mean2)/(sqrt(((sd1^2/n1) + (sd2^2/n2))))
    # p value
    pt_value <- pt(T_obs, gamma)
    
  }
  print('variance p value')
  print(p_value)
  print('p value final')
  print(pt_value)
  if (pt_value > alpha) {
    result <- "Accept H0"
  } else if (pt_value < alpha) {
    result <- "Reject H0"
  }
  return(result)
}

# **********************
# Part 6
# **********************

first_test <- mean_hypothesis_test(iris$Petal.Length[iris$Species == "virginica"],
                              iris$Petal.Length[iris$Species == "versicolor"],
                              0.95)
second_test <- mean_hypothesis_test(iris$Petal.Length[iris$Species == "versicolor"],
                                   iris$Petal.Length[iris$Species == "setosa"],
                                   0.95)
print("Petal length of Virginica iris is larger than that of Versicolor")
print(first_test)
print("Petal length of Versicolor iris is larger than that of Setosa")
print(second_test)

# **********************
# Part 7
# **********************

# Petal length of Virginica iris is larger than that of Versicolor
# H0: mu1 >= m2 : H1: mu2 > mu1
# population variance test results in pooled variance (p-vale = 0.259)
# failed to reject null hypothesis because p-value = 1

# Petal length of Versicolor iris is larger than that of Setosa
# H0: mu1 >= m2 : H1: mu2 > mu1
# population variance test results in unequal population vairance (p-value = 6.6e-11)
# failed to reject null hypothesis because p-value = 1

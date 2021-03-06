# Peer-graded Assignment: Course Project on Statistical Inference (Part 1)

## By: Cecilia Cruz-Ram, MD DPCOM

## Introduction:
# In this project we will investigate the exponential distribution in R and compare it with the 
# <b>Central Limit Theorem</b>. The exponential distribution can be simulated in R with 
# <b>rexp(n, lambda)</b> where lambda is the rate parameter. The mean of exponential distribution 
# is 1/lambda and the standard deviation is also 1/lambda. 

# Provided Data:
# * lambda = 0.2 for all of the simulations
# * Distribution of averages of 40 exponentials
# * Number of simulations = 1000

## Simulations:

# A. Setwd
setwd("/Users/sexybaboy/Documents/Files/Zetch/Online Courses/Data Science Specialization Feb18/R/Statistical Inference")

# B. Set seed of reproducibility
set.seed(2018)

# C. Set sample size
n <- 40              # sample size
lambda <- 0.2        # number of exponentials
simNum <- 1000       # number of simulations

# D. Run simulations
simMeans = NULL
for (i in 1 : 1000) {
  simMeans = c(simMeans, mean(rexp(n,lambda)))
}

head(simMeans)

# E. Instructions
# <b>*1. Show the sample mean and compare it to the theoretical mean of the distribution.*</b>

# Run theoretical mean
theosampMean <- round(1/lambda,3)
theosampMean

# Run actual mean
actualMean <- round(mean(simMeans),3)
actualMean

# Plot showing both means
require(ggplot2)
simMeansDf <- as.data.frame(simMeans)
g <- ggplot(simMeansDf, aes(x = simMeans))
g <- g + geom_histogram(binwidth = .3, color = "black", fill = "green") +
  geom_vline(xintercept = theosampMean, color = "red", size = 1, linetype = 1) +
  geom_vline(xintercept = actualMean, color = "blue", size = 1, linetype = 2) +
  labs(x = "Simulation Means", y = "Frequency", 
       title = "Theoretical vs Actual Mean")
g

# *The red dashed vertical line indicate the theoretical sample mean, 1/lambda = 5, 
# while the green dashed vertical line is the calculated average sample mean size 
# of 40 of 1000 samples showing very close proximity.*

# <b>*2. Show how variable the sample is (via variance) and compare it to the theoretical 
# variance of the distribution.*</b>

# Run theoretical variance
theosampVar <- round((1/lambda)^2/n,3)
theosampVar

#' Run actual variance
actualVar <- round(var(simMeans),3)
actualVar

# Table showing both theoretical and actual mean and variance
tab <- matrix(c(theosampMean, actualMean, 
                theosampVar,actualVar),
              ncol = 2, byrow = TRUE)
colnames(tab) <- c("Theoretical","Sample")
rownames(tab) <- c("Mean","Variance")
tab <- as.table(tab)
tab

# *Preceding table shows near identical values.*

# <b>*3. Show that the distribution is approximately normal.

# In point 3, focus on the difference between the distribution of a large collection 
# of random exponentials and the distribution of a large collection of averages of 40 exponentials.*</b>

# Make a histogram with the density and sample means. Add density curve of the normal distribution 
# and the sample distribution:
g <- ggplot(simMeansDf, aes(x=simMeans))
g <- g + geom_histogram(binwidth = .3, color = "black", fill = "green" , aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = theosampMean, sd = sd(simMeans)), 
                color = "red", size = 1) +
  stat_density(geom = "line", color = "blue", size = 1)  +
  labs(x = "Simulation Means", y = "Density", 
       title = "Density of Simulated Exponential Means")
g

# *Plot2 shows the distribution of means of the sampled exponential distributions which 
# appear to follow a normal distribution, due to the <b>Central Limit Theorem</b>. 
# An increase in the number of samples (currently 1000) will create a distribution 
# that would be even closer to the standard normal distribution. The red line above is 
# the normal distribution curve which closely approximates the blue colored sample curve.*

rmarkdown::render("SI Course Project (Part 1)")
rmarkdown::render("SI Course Project (Part 1).R", "pdf_document")
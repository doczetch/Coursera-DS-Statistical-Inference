# Peer-graded Assignment: Course Project on Statistical Inference (Part 2)

## By: Cecilia Cruz-Ram, MD DPCOM

# <b>Basic Inferential Data Analysis Instructions</b>

# Now in the second portion of the project, we're going to analyze the ToothGrowth data in 
# the R datasets package.

# A. Setwd and load package
setwd("/Users/sexybaboy/Documents/Files/Zetch/Online Courses/Data Science Specialization Feb18/R/Statistical Inference")
require(ggplot2)

# B. Instructions
# 1. Load the ToothGrowth data and perform some basic exploratory data analyses
# Load ToothGrowth data
data("ToothGrowth")

# Display first few rows of data
head(ToothGrowth)

# Show Unique Values
unique(ToothGrowth$len)
unique(ToothGrowth$supp)
unique(ToothGrowth$dose)

# Analyze and explore data via plots
# Convert dose to a factor
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

# Plot tooth length ('len') vs. the dose amount ('dose'), broken out by supplement delivery method ('supp')
bp <- ggplot(ToothGrowth, aes(x = dose, y = len, fill = dose)) +
  geom_boxplot(position = "dodge", varwidth = TRUE) + xlab("Dose Amount") + ylab("Tooth Length") + facet_grid(~ dose) + 
  ggtitle("Tooth Length v Dose Amount (by Delivery Method)") + 
  theme(plot.title = element_text(lineheight = .3))
bp + scale_fill_hue(l = 70, c = 50)

# Plot tooth length ('len') vs. supplement delivery method ('supp') broken out 
#by the dose amount ('dose')
bp <- ggplot(ToothGrowth, aes(x = supp, y = len, fill = dose)) +
  geom_boxplot(position = "dodge", varwidth = TRUE) + xlab("Supplement Delivery") + ylab("Tooth Length") + facet_grid(~ supp) + 
  ggtitle("Tooth Length v Delivery Method (by Dose Amount)") + 
  theme(plot.title = element_text(lineheight = .3))
bp + scale_fill_hue(l = 70, c = 50)


# 2. Provide a basic summary of the data.
summary(ToothGrowth)


# 3. Use confidence intervals and/or hypothesis tests to compare tooth growth by supp 
# and dose. (Only use the techniques from class, even if there's other approaches worth considering)

# Run t-test
t.test(len~supp, data = ToothGrowth)

# Run t-test using dose amounts 0.5 and 1.0
ToothGrowth_sub <- subset(ToothGrowth, ToothGrowth$dose %in% c(1.0, 0.5))
t.test(len~dose, data = ToothGrowth_sub)

# Run t-test using dose amounts 0.5 and 2.0
ToothGrowth_sub <- subset(ToothGrowth, ToothGrowth$dose %in% c(0.5, 2.0))
t.test(len~dose, data = ToothGrowth_sub)

# Run t-test using dose amounts 1.0 and 2.0
ToothGrowth_sub <- subset(ToothGrowth, ToothGrowth$dose %in% c(1.0, 2.0))
t.test(len~dose, data = ToothGrowth_sub)

# 4. State your conclusions and the assumptions needed for your conclusions.

# Assuming the sample is representative of the population and the distribution of the 
# sample means follows the Central Limit Theorem, we found out that the higher the 
# dosage, the longer the tooth grows over time. Supplement delivery method, however, 
# has no effect on tooth growth/length.

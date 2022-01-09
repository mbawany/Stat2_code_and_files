#=====================================
#   R Script Homework 1 
#=====================================

#=========================================
# Load Packages and Set working directory 
#=========================================
rm(list = ls())
set
# Load the packages. 
library(tprstats)
tprstats::setup() 
# Use CTRL + L to clear screen in console

current_user_dir = dirname("~")
working_dir_beyond_usr <- "/Documents/Tepper/Stats2/"
# The paste function concatenates the text

specific_path_to_dir <- paste(current_user_dir, working_dir_beyond_usr, sep = '')
specific_path_to_dir
setwd(specific_path_to_dir)
getwd()
#=======================================
# Exercise 1 
#=======================================
stat2_hw2pt1 <- read_excel("files/stat2_homework1_pt1.xlsx")
# See first like 5 rows 
head(stat2_hw2pt1)


# Part A: Get a control Chart
#=========================================

population_mean <-  120
sample_size <- length(stat2_hw2pt1$Data)
#signma
anticipated_standard_deviation <- 20
sample_standard_deviation <- anticipated_standard_deviation/sqrt(sample_size)

alpha <- .02

#controlChart(mydata,??,??,n,??)
controlChart(stat2_hw2pt1$Data,population_mean,anticipated_standard_deviation,sample_size,alpha)



# Part B: Record Acceptance Intervals 
#=========================================

#Determine if the process was in control over the period for which your 
#70 samples were taken. What should you look for on your plot of the control 
#chart? The following are rules of thumb often applied in assessing whether the 
#process may be out of control:  
#  
#  i) More than 2??? percent of values outside the control limits. (Recall that, on 
#                                                                 average, ??? percent of the points will fall outside the limits even when the 
#                                                                 process is working properly, so you should not expect every point to be 
#                                                                 inside the control limits.)  
#  ii) A trend in the sample, i.e., six or more points in a row all increasing or all 
#      decreasing. 
#  iii)  Too many points on one side of the center line, say nine consecutively or 
#   more.
# Manual count was 18 
# 30/70.0
# Better way to count this 

acceptance_interval_without_pop_mean <-  qnorm(1-alpha/2) * anticipated_standard_deviation/sample_size^.5



stat2_hw2pt1$is_outside_interval <- abs(stat2_hw2pt1$Data - population_mean) > acceptance_interval_without_pop_mean 

view(stat2_hw2pt1)

all_observations_outside_interval <- subset(stat2_hw2pt1, stat2_hw2pt1$is_outside_interval == TRUE)
number_of_points_outside_interval <- length(all_observations_outside_interval$is_outside_interval)

print (paste("Total observations outside interval were noted to be ", number_of_points_outside_interval, sep=''))
print (paste("The error rate, points outside the interval over sample size, is ", number_of_points_outside_interval/sample_size,sep=''))
print ("This is concerning as the rate should be 2 * alpha, 4%. Also there is a concerning trend towards the end. All last 6 points fall outside the interval. From obersavtions 16 - 25 all are on the lower side of the line. The same is true for points towards the end of the line.")



# Part C: Calculate a Probability of a given Z 
#=========================================

#  Find the probability that a draw from the standard normal distribution 
#  will be less than Z=2.15. Hints: Read the Mini Tutorial below. Then see the 
#  pnorm() command in the command summary.

probability_of_draw <- pnorm(2.15)
print (paste("The probability of having a value less than z = 2.15 is ", probability_of_draw, sep=''))


# Part D: What value of Alpha is the firm using  
#=========================================
# A firm uses ????1?????/2 =2.24 in plotting its 
# control chart. Find the implied ?? for this firm. Hint: Build on the logic in the 
# previous question. 

# Get the probability of z 
unknown_alpha_z <- pnorm(2.24)

# Reverse the equation of 1-(a/2) to get 2(1-Z) = Aplha 
unknown_alpha <- 2*(1 - unknown_alpha_z)

print (paste('The alpha for the Z value of 2.24 is ', unknown_alpha))
#Alternatively you could do the following 
# Get the probability of MINUS z 
unknown_alpha_z <- pnorm(-2.24)

# Multiply by two to get as this is a/2 not alpha
unknown_alpha <- 2* (unknown_alpha_z)

print (paste('The alpha for the Z value of 2.24 is ', unknown_alpha))



# Part E: Find the upper bound of a control chart:   
#=========================================
 #Suppose you have a 
 #manufacturing process designed to produce a component with ??=110 and ??=12. 
 #The sample size per sample is 25 and ??=.03. Calculate and enter the value 
 # for the upper bound of your control chart. Hints:  
 # i) The upper bound of a control chart is: ??+????1?????/2???????? 
 # ii)  Recall that qnorm() is used to find the quantile of a normal distribution.  
 # iii) The question gives you ??, ??, and ????. Recall that ???????? = ?? ???????

mu_part_E          <- 110
sigma_part_E       <- 12 
sample_size_part_E <- 25
alpha_part_E       <- 0.3

# Step 1 Get Z value 
z_value_part_E = pnorm(1-(alpha_part_E/2))

# Step 2 calculate the Acceptance interval without the population mean 
acceptance_interval_without_pop_mean_part_E = z_value_part_E * (sigma_part_E/sqrt(sample_size_part_E))

# Step 3: Get upper bound 
upper_bound_part_E = mu_part_E + acceptance_interval_without_pop_mean_part_E

print (paste('The upper bound for the given conditions is ', upper_bound_part_E))

# Part F: Test the hypothosis:   
#=========================================
# Test the null hypothesis that the mean of the distribution generating your data 
# really  is  120.    Report  how  you  tested  the  hypothesis  and  the  result.  As  practice, 
# repeat it using data for some other columns in the dataset

sample_mean <- mean(stat2_hw2pt1$Data)
sample_standard_deviation <- sd(stat2_hw2pt1$Data)
alpha <-  .05
null_hyphotosis <- 120
degrees_of_freedom <- sample_mean - 1

### This seems wrong
critical_value <- (sample_mean - null_hyphotosis)/(sample_mean/sqrt(sample_size))

2*pt(critical_value,degrees_of_freedom)




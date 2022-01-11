#=====================================
#   R Script Homework 1 
#=====================================

#=========================================
# Load Packages and Set working directory 
#=========================================
rm(list = ls())
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


#-------------------------------------------
#  -----------------
# Setup Complete 
#  -----------------------------------------
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

#controlChart(mydata,μ,σ,n,α)
controlChart(stat2_hw2pt1$Data,population_mean,anticipated_standard_deviation,sample_size,alpha)



# Part B: Record Acceptance Intervals 
#=========================================

#Determine if the process was in control over the period for which your 
#70 samples were taken. What should you look for on your plot of the control 
#chart? The following are rules of thumb often applied in assessing whether the 
#process may be out of control:  
#  
#  i) More than 2 percent of values outside the control limits. (Recall that, on 
#                                                                 average,  percent of the points will fall outside the limits even when the 
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
# A firm uses 𝑍1−α/2 =2.24 in plotting its 
# control chart. Find the implied α for this firm. Hint: Build on the logic in the 
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
 #manufacturing process designed to produce a component with μ=110 and σ=12. 
 #The sample size per sample is 25 and α=.03. Calculate and enter the value 
 # for the upper bound of your control chart. Hints:  
 # i) The upper bound of a control chart is: μ+𝑍1−α/2σ𝑋̅ 
 # ii)  Recall that qnorm() is used to find the quantile of a normal distribution.  
 # iii) The question gives you μ, σ, and 𝑛. Recall that σ𝑋̅ = σ √𝑛

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

# this P value is far larger than the the alpha of .05, therefore we can confidently reject the null

#=======================================
# Exercise 2 Simulation Model  
#=======================================

#recording variables for first spin 
first_spin_min <- 0 
first_spin_max <- 5000

#recording variables for Second spin 
second_spin_min <- 0 
second_spin_max <- 9000

#recording variables for Third spin 
third_spin_min <- 0 
third_spin_max <- 12000

number_of_times_to_run = 5000

seed_value <- 10 

set.seed(seed_value)

# Get simulation results 

spin_1_sim_results <- runif(number_of_times_to_run, first_spin_min, first_spin_max)
spin_2_sim_results <- runif(number_of_times_to_run, second_spin_min, second_spin_max)
spin_3_sim_results <- runif(number_of_times_to_run, third_spin_min, third_spin_max)

total_sim_results <- spin_1_sim_results + spin_2_sim_results + spin_3_sim_results

#Part A report MEAN 
print("the mean of the simulation is: ", mean(total_sim_results), sep='')
      

#Part B report 90% CI
quantile(total_sim_results, c(.1, .9))
hist_CI(total_sim_results)

### QUESTION FOR THE PROFESSOR - Why doesn't this get the confidence interval. This code should get the right amount but it differs form the output of the HIST_CI
#mean(total_sim_results) + (sd(total_sim_results)) * qt(.05,5000-1,lower.tail = F)
##

is_amount_less_than_20k = ifelse(total_sim_results < 20000,1,0)
#Part C teh probability of receiveing less than 20k 
mean(is_amount_less_than_20k)
#=======================================
# Hungry Dawg Part D 
#=======================================

# Clear Environment
rm(list=ls())

# We initialize the following six variables to the same values as above
mu =250 # Mean of claims in the current month (month 1)
N=18533 # Employment in the current month
EmplContrib=125  # Employee contribution
Claim=0 # This will contain the claim per employee each month
CC=0    # This will contain company cost each month
TCC=0   # This will contain total company cost for the next 12 months

# Set Parameter Values to incorporate uncertainty
CGro=1+.01  # 1 + Growth rate of average claim per employee

EGroMin = -.04  # 1 + lower bound of growth rate of employment
EGroMax = .08  # 1 + upper bound of growth rate of employment

# Standard Deviation of Claims
SDC = 3   # Standard deviation of claims


set.seed(33)
for (r in 1:1000) {
  CC=0
  Claim=0
  for (t in 2:13) {
    N[t]=N[t-1]*runif(1,EGroMin,EGroMax)
    mu[t]=mu[t-1]*1.01
    Claim[t]=rnorm(1,mu[t],SDC) 
    EmplContrib[t]=125
    CC[t]=N[t]*(Claim[t]-EmplContrib[t])
  }
  TCC[r]=sum(CC[2:13])/1e6    # Company cost is expressed in millions
}

# Mean and Confidence Interval
hist_CI(TCC,main = "Simulated Distribution of Company Cost: Model 1", 
        xlab = "Company Cost and 90% Confidence Interval")

#=======================================
# Hungry Dawg Part E 
#=======================================

# Clear Environment
rm(list=ls())

# We initialize the following six variables to the same values as above
mu =250 # Mean of claims in the current month (month 1)
N=18533 # Employment in the current month
EmplContrib=125  # Employee contribution
Claim=0 # This will contain the claim per employee each month
CC=0    # This will contain company cost each month
TCC=0   # This will contain total company cost for the next 12 months

# Set Parameter Values to incorporate uncertainty
CGro=1+.01  # 1 + Growth rate of average claim per employee

mean = -.04  # growth rate mean 
standard_deviation = .08  # growth rate standard deviation 

# Standard Deviation of Claims
SDC = 3   # Standard deviation of claims


set.seed(33)
for (r in 1:1000) {
  CC=0
  Claim=0
  for (t in 2:13) {
    N[t]=N[t-1]* (1+rnorm(1,mean,standard_deviation))
    mu[t]=mu[t-1]*1.01
    Claim[t]=rnorm(1,mu[t],SDC) 
    EmplContrib[t]=125
    CC[t]=N[t]*(Claim[t]-EmplContrib[t])
  }
  TCC[r]=sum(CC[2:13])/1e6    # Company cost is expressed in millions
}

# Mean and Confidence Interval
hist_CI(TCC,main = "Simulated Distribution of Company Cost: Model 1", 
        xlab = "Company Cost and 90% Confidence Interval")


#=======================================
# Exercise 3
#=======================================


#** SKipping to part F as that is what we know how to do as of now 
#*

rm(list = ls()) 

brand_a_mileage_mean <- 81.22
brand_a_mileage_varience <- 59.76

brand_b_mileage_mean <- 86.04
brand_b_mileage_varience <- 56.98

sample_size = 72 #this was found in part E 

#8 Part F, find average difference in mileage 
difference_in_mileage <- brand_b_mileage_mean - brand_a_mileage_mean 

#* part G
#* What is the T statistic for testing whether these are equal 
#* slide 11 of AW session 2 for more info and formula

estimated_standard_error = sqrt((brand_a_mileage_varience/sample_size) + (brand_b_mileage_varience/sample_size))
hypothosis_t_stat = difference_in_mileage / estimated_standard_error

#* Part H 
#* 
combined_degrees_of_freedom = (sample_size * 2) - 2 
2 * pt(-abs(hypothosis_t_stat),combined_degrees_of_freedom)

# Part I 
miles_data <- read_excel('files/NLD_Mileage_Experiment.xlsx')
sample_regression <- lm(Miles~Brand_A, data = miles_data)

summaryH(sample_regression)

# Part J & K
# 
sample_regression <- lm(Miles~Age, data = miles_data)

summaryH(sample_regression)

# Check criticism by evaluating if the difference in the vans is 0 
data_for_brand_a <- subset(miles_data, subset = miles_data$Brand_A == 1)
data_for_brand_b <- subset(miles_data, subset = miles_data$Brand_A == 0)

# Age of vans is stored in the Age variable 
# This is acedemic to see if there is a significant difference between the two mean ages 
# mean_of_van_age_brand_a <- mean(data_for_brand_a$Age)
# mean_of_van_age_brand_b <- mean(data_for_brand_b$Age)
# 
# sd_of_van_age_brand_b <- sd(data_for_brand_b$Age)
# sd_of_van_age_brand_a <- sd(data_for_brand_a$Age)
# 
# print(paste("Mean for brand A = ", mean_of_van_age_brand_a, "  and the SD is ", sd_of_van_age_brand_a))
# print(paste("Mean for brand B = ", mean_of_van_age_brand_b, "  and the SD is ", sd_of_van_age_brand_b))
# 
# mean_diff_in_age <- mean_of_van_age_brand_a - mean_of_van_age_brand_b
# estimated_standard_error_of_age = sqrt()




#*==================================================================
#* Exercise 4 
#* =================================================================
#* 
# Answer 1 
# No as the relationship could be a negative one. The metric we should look at to determine how much of X explains y is the R squared

# Answer 2 
# The regression could have an intercept of less than zero or even higher as it tries to determine the relationship. 


# Imported cpde from the assignements page on canvas

#   Set working directory and packages
# Clear the working space
rm(list = ls())

# Set your working directory by clicking Session/Set working Directory/Choose Directory
# Then navigate to the folder where you placed this file and click Select Folder 
Compap_Credcrd_Treas_Rates <- read_excel('files/Compap_Credcrd_Treas_Rates.xlsx')
# Load the packages, ignore warnings. 
library(tprstats)
tprstats::setup()

#  Import Compap_Credcrd_Treas_Rates Dataset
# For convenience, put the data in a data file with a shorter name.
CCData=Compap_Credcrd_Treas_Rates

# Calculate quarter-to-quarter changes in credit card rates
dCredcrd=with(CCData,Credcrd-lag(Credcrd,1))
dCompap=with(CCData,Compap-lag(Compap,1))
dTreas=with(CCData,Treas-lag(Treas,1))



# Regress changes in Credit Card rates on changes in Treasury rates
CredReg=lm(dCredcrd~dTreas)
summaryHAC(CredReg)

# Test null hypothesis that coefficient of dTreas=1
coefTestHAC(CredReg,"dTreas=1")
# Regress changes in Commercial Paper rates on changes in Treasury rates


# Create a variable named Observation to use for plotting
Time=seq(1,92)



CredReg=lm(dCredcrd~dTreas) 
summaryHAC(CredReg) # this gives you the answer to part D as well


# Part D test whether the b1 is zero 

xbar <- 0.346150   
standard_error <- 0.077219
degrees_of_freedom <- length(dCredcrd) - 2
hypothoisis <-  .3
t_stat <- (xbar - hypothoisis)/standard_error

2 * pt(- abs(t_stat), degrees_of_freedom)

# This is well below the alpha of 1 

# Part E, Yes this has some effect on the credit score, although the R2 score is too low for that. It is suggested that the impact may not be as big

plot(dCredcrd, dTreas, main="Fit Example",
     xlab="Credit Card ", ylab="Treasury ", pch=19, col = 'navy') 
abline(a=0.003618   , b=xbar   , col='red')

#-----------------------------------------------------------------------
# Part G
#-----------------------------------------------------------------------



# Regress changes in Commercial Paper rates on changes in Treasury rates


# Create a variable named Observation to use for plotting
Time=seq(1,92)



CredReg=lm(dCompap~dTreas) 
summaryHAC(CredReg) 

# Part H 
# Test null hypothesis that coefficient of dTreas=0
coefTestHAC(CredReg,"dTreas=0")

xbar <- 0.940797      
standard_error <- 0.050951  
degrees_of_freedom <- length(dCredcrd) - 2
hypothoisis <-  0
t_stat <- (xbar - hypothoisis)/standard_error

2 * pt(- abs(t_stat), degrees_of_freedom)

plot(dCredcrd, dTreas, main="Fit Example",
     xlab="Credit Card ", ylab="Treasury ", pch=19, col = 'navy') 
abline(a=0.003618   , b=xbar   , col='red')


# Part H, Very much so. There seems to be a much higher correlation 

# Part J 

# Plot dCompap and dTreas on same graph
plot(Time,dCompap,col="white",ylab="Interest Rate Changes",
     main = "Variation in Commercial Paper and Treasury Rates Over Time")
lines(Time,dCompap,lw=2)
lines(Time,dTreas,col="red",lw=2)
# part K

plot(Time,dCredcrd,col="white",ylab="Interest Rate Changes",
     main = "Variation in credit card and Treasury Rates Over Time")
lines(Time,dCredcrd,lw=2)
lines(Time,dTreas,col="red",lw=2)

#Part I 
#* Very much so. There is a strong pattern between commercial paper and the treasury rates. My XY graph also showed this.
#* The other 

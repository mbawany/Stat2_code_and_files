#=====================================
#   Sample Simulations
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


#=======================================
# Samples Simulation Model  
#=======================================
rm(list = ls())

# Example 1 

# Simulate a sample of 1000 draws from a normal distribution with a mean=2 and 
# standard deviation=3. If we all use the same seed, we all get the same result. We can enter any 
# number we like as a seed. Let’s use seed 10. Throughout this tutorial, italics are used for R 
# commands.
# 

mean <- 2 
sd <- 3
# the following is N 
number_of_samples <- 1000
seed_value <- 10 

set.seed(seed_value)


# To use rnorm function, you need Samples, Mean, and the SD. The return is an array of values in which you have smaples number of random variables 

example1_distribution = rnorm(number_of_samples,mean, sd)
hist_CI(example1_distribution)


# Example 2: Suppose we want to simulate a t-distribution with μ=2, s=3, and df=12, where df 
# denotes degrees of freedom. Note that μ=2 and s=3 are the same as in Example 1. It is natural 
# for you to expect that s is the standard deviation, but, for the t-distribution, s is multiplied by a 
# term that varies with the degrees of freedom. Indeed, the key point that I wish to make with this 
# example is that, for given μ and s, the variance of the t-distribution is larger than the variance of 
# the normal distribution. 
rm(list = ls())


sample_mean <- 2 
standard_deviation <- 3
# the following is N 
number_of_samples <- 1000
seed_value <- 10 
degrees_of_freedom <-  12
chart_title <-  "This is my title"
sub_title  <- "This is a subtitle"
bin_breaks <- 20

set.seed(seed_value)
# the RT just returns the T value, Number fo standard deviations away form the mean. 
# Multiply by the standard Deviation and add in the mean to the right result 

example2_t_distribution <- sample_mean + standard_deviation * rt(number_of_samples, degrees_of_freedom)

hist_CI(example2_t_distribution
        , alpha = .1
        , main = chart_title
        , xlab = sub_title
        , breaks = bin_breaks
)

# Example 3: Your generous, eccentric Uncle Max gives you the following gift. At the beginning 
# of next month, he will spin a wheel of fortune. The wheel is equally likely to stop on any number 
# between 0 and 10,000. He will invest the corresponding amount in a checking account at zero 
# interest. He will do this at the beginning of each month for the next three months. At the end of 
# the three months, he will send you the proceeds. The goal of this example is to illustrate how 
# simulation can help you obtain a solution in a situation where working out a mathematical 
# solution would be difficult. 
# 
# What is the expected value of the gift? We have three independent draws from a uniform 
# distribution on the interval [0,10000]. Hence, the distribution has a mean of $5,000. Thus, the 
# expected value of the sum of three spins of the wheel is $15,000. 
# 
# How uncertain is the amount? Let’s form a 90% confidence interval for the amount you will 
# receive. This is not so easy to work out by “doing the math.” It is very straightforward using a 
# simulation approach. 
# 
# Perform 5000 replications of spin 1, then 5000 of spin 2, than 5000 of spin 3. Use seed 10: 

rm(list = ls())

seed_value <- 10 
number_of_samples <- 5000 
sample_min <- 0
sample_max <- 10000
given_alpha <- .1
chart_title <- "Simulation of 3 Labels"
horizontal_label <- "Amount"
set.seed(seed_value)

# Generate 3 samples 

x1 <- runif(number_of_samples, sample_min, sample_max)
x2 <- runif(number_of_samples, sample_min, sample_max)
x3 <- runif(number_of_samples, sample_min, sample_max)

# add them up 
amount <- x1 + x2 + x3
#view(amount)

hist_CI(amount
        , alpha = given_alpha
        , main = chart_title
        , xlab = horizontal_label
)

amount_df <- (data.frame(amount))

amount_df$is_amount_lesser_than_12K <- ifelse(amount_df$amount < 12000,1,0)

#view(amount_df)

print(mean(amount_df$is_amount_lesser_than_12K))


# Example 4: Simulate 1,000 samples from the uniform distribution, with each sample having 30 
# observations. Use a uniform with range [0,3]. Calculate the means of these samples and fit a 
# distribution to the results. From this this example, you will learn how to use a vector in R, and 
# you will learn how to use a valuable R command, the for command.

rm(list = ls())

xbar <- 0
seed_value <- 33
observations <- 30
sample_min <- 0
sample_max <- 3

set.seed(seed_value)

for (i in 1:1000){
  xbar[i] <- mean(runif(observations,sample_min,sample_max))  
}

hist_CI(xbar)

mean(xbar)
var(xbar)
# this tells you which distribution to pick
select_distribution(xbar)

# Example 5: In this example, we will provide an additional illustration of the select_distribution() 
# function. This example will also familiarize you with how to deal with missing data. Import the 
# data file Fargo_Feb_Temp.xlsx into R. This file contains daily temperatures in February for a 
# period of 19 years.
rm(list = ls())
# Import the dataset 
fargo_temps <- read_excel("files/Fargo_Feb_Temp.xlsx")

quantile(fargo_temps$TEMP_FARGO, c(.95,.05))
#count how many NAs are in the dataset 
sapply(fargo_temps, function(x) sum(is.na(x)))

# Get the row(s) which has an NA 
subset(fargo_temps, subset = is.na(fargo_temps$TEMP_FARGO))
# Remove NAs
no_null_data_set = na.omit(fargo_temps$TEMP_FARGO)
#Now run the thing without errors
quantile(no_null_data_set, c(.95,.05))
#Calculate Mean 
mean(no_null_data_set)
# this takes care of null by omitting them 
hist_CI(no_null_data_set)

select_distribution(fargo_temps$TEMP_FARGO)

seed_value <-  33
set.seed(seed_value)
#simluate 1500 samples 

# using values i got from the above function 
#Estimated parameters for the snorm :
# xi      omg    alpha 
# 30.63653 20.89595 -2.73348 
sample_size <- 1500 
xi <- 30.63653
omg <- 20.89595
given_alpha <- -2.73348 

#rsn(n,xi,omg,alpha) 
fargo_simulated_temps <- rsn(sample_size, xi, omg, given_alpha)

hist_CI(fargo_simulated_temps)




# Example 6: Let’s fit a distribution to daily returns to GE stock in 2019. Import 
# GE_Data_12_17_18_to_12_13_19.xlsx. In this example, you will learn how to fit individual 
# distributions that you might wish to compare. You will also see that these data provide a real-
#   world example of data that fit nicely to a t-distribution. 

rm(list = ls())
ge_data <- read_excel("files/GE_Data_12_17_18_to_12_13_19.xlsx")

select_distribution(ge_data$returns_GE)


# now time to fit a distribution the following options are available 
# norm", "unif", "t", "snorm", "weibull", "gamma", "lnorm", "exp"  

par(mfrow=c(1,2))

fit_distribution(ge_data$returns_GE, 'norm')
fit_distribution(ge_data$returns_GE, 't')

#show only one plot per area
par(mfrow=c(1,1)) 

# Example 7 
# use distribution from example 6. Variables for that are as follows

seed_value <- 33
set.seed(seed_value)
mean = 0.0004413722 
degrees_of_freedom = 3.3691890267 
sample_standard_deviation = 0.0179998429

strike_price = 12 
day_of_expiry = 91

#getting day 1 price
price_of_stock = 11.34

number_of_simulations = 5000
simulation_results= 0

for (simulation_number in 1:number_of_simulations){
  
for (day in 2:day_of_expiry)
{
  t_value <- mean + sample_standard_deviation * rt(1,degrees_of_freedom)
  price_of_stock[day] <- (1 + t_value) * price_of_stock[day-1] 
  
}
simulation_results[simulation_number] <- max(price_of_stock[90] - strike_price,0)
}


v = mean(simulation_results)
v
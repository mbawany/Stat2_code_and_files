#=====================================
#   R Script Access Weekend Session 1
#=====================================
#=========================================
# Load Packages and Set working directory 
#=========================================
# Clear the working space
# This isn't necessary but is a good habit
rm(list = ls())

# Load the packages. 
library(tprstats)
tprstats::setup() 
# Go to your working directory by clicking Session/Set working Directory/Choose Directory
#================================================
#   R Commands That Accompany Access Weekend Videos
#================================================

# VIDEO: Understanding and Calculating Quantiles

# Calculate .975 quantile of normal distribution
qnorm(.975)

# Calculate the .975 quantile of the t-distribution with 12 df
qt(.975,12)

# Import Excel file: Pgh_May_Temps

# View 75th quantile of Pittsburgh may Temperatures
quantile(Pgh_May_Temps$Temperature,.75)

#Calculate the 10th, 50th, and 90th quantiles
quantile(Pgh_May_Temps$Temperature,probs=c(.10,.5,.90)) 

# VIDEO: Review of Hypothesis Testing

# Calculate .975 Quantile of t-distribution with 29 degrees of freedom
qt(.975,29)

# Calculate two-tailed p-value when t=-1.205 and df = 29
2*pt(-1.205,29)


# The following are not in the video

# Import Dollar_Bank.xlsx
# Conduct the hypothesis test
t.test(Dollar_Bank$TVL,mu=15)


# Remove items we no longer need 
rm(list=ls())

# Video: Statistical Quality Control

# Import Dataset Theta_Data

#Following plots control chart for XBAR Theta & Theta Candy
controlChart(Theta_data$XBAR,50,.3,25,.05)

#Following plots control chart for XBAR2 Theta & Theta Candy
controlChart(Theta_data$XBAR2,50,.3,25,.05)

# Video: Statistical Quality Control for Binary Outcomes

#Import Dataset Defects_data.xlsx

#Following plots the control chart for defect rate d_rate
controlChartBinary(Defects_data$d_rate,.06,85,.05)

# The code below is executed when you run the controlCharts() command.
# You are not required to know these commands,
# but you may find it interesting to know what happens behind the scenes.
# I suggest you mark and run the commands one at a time. 
# Then, observe what happens when you run each command.
mu=50;sig=.3;n=25; alpha=.05
m=seq(1,NROW(Theta_data$XBAR))
ymax=max(Theta_data$XBAR,mu+qnorm(1-alpha/2)*sig/n^.5)
ymin=min(Theta_data$XBAR,mu-qnorm(1-alpha/2)*sig/n^.5)
plot(XBAR~m,ylim=c(ymin, ymax), xlab="Observation Number",ylab="Sample Means",data=Theta_data)
lines(XBAR~m,data=Theta_data)
abline(h=mu,col="Red")
abline(h=mu-qnorm(1-alpha/2)*sig/n^.5,col="Blue")
abline(h=mu+qnorm(1-alpha/2)*sig/n^.5,col="Blue")


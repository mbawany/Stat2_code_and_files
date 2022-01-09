#=====================================
#   R Script Homework 6 practice 1 
#=====================================
#=========================================
# Load Packages and Set working directory 
#=========================================
# Clear the working space
# This isn't necessary but is a good habit
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



#+=============================================================================
#+ Messy initial stuff over, now main stuff
#==============================================================================
#Get the file 
library(readxl)
stat1_homework6 <- read_excel("files/stat1_homework6.xlsx")
# See first like 5 rows 
head(stat1_homework6)


#Split data into BIO tech and Software
# remember to use == for comparison 
biotech_data = subset(stat1_homework6, subset = stat1_homework6$`Industry Group` == "Biotechnology")
IS_data = subset(stat1_homework6, subset = stat1_homework6$`Industry Group` == "Internet software and services")

xbar_biotech = mean(biotech_data$`Cost of capital in US$ (%)`)
n_biotech =    length(biotech_data$`Cost of capital in US$ (%)`)
sd_sample_of_x_biotech = sd(biotech_data$`Cost of capital in US$ (%)`)
sd(biotech_data$`Cost of capital in US$ (%)`)


xbar_IS            <- mean(IS_data$`Cost of capital in US$ (%)`)
n_IS              <- length(IS_data$`Cost of capital in US$ (%)`)
sd_sample_of_x_IS    <- sd(IS_data$`Cost of capital in US$ (%)`)
sd(IS_data$`Cost of capital in US$ (%)`)

condfidence_interval = .95
alpha = 1 - condfidence_interval

# Check hypothesis that these data sets have the same mean IE does X bar of biotech minus X bar of IS = 0 << this is our null hyphotosis
# 
# First get the difference in the means so the difference in the two x bars

mean_difference <-  xbar_biotech - xbar_IS

# Then get the estimated standard error 
# see slide 11 on the access_weekend session 2 slide deck for the formula

estimated_standard_error <- sqrt((sd_sample_of_x_biotech^2 / n_biotech) + (sd_sample_of_x_IS^2 / n_IS))

# Getting the T stat
hypothosis_t_stat <- mean_difference / estimated_standard_error

# Combined degrees of freedom 
combined_degrees_of_freedom <- n_IS + n_biotech - 2

typeof(hypothosis_t_stat)

#Get the P Value 
hypthosis_p_value <- 2 * pt(hypothosis_t_stat, combined_degrees_of_freedom)


if (hypthosis_p_value > alpha){
  print("Null is likely true")
} else {
  print("Reject Null ")
}


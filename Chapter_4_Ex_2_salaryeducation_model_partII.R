
# From Chapter 4 exercise 2 from Real Econometrics by Michael Bailey
# This is for the first edition of the textbook
# loading in libraries
library(tidyverse)
library(dplyr)
library(ggthemes)
library(broom)
# info on original model:
# the true value being: salary = 12,000 + 1000 * education in yrs + e

# functions
# this function run a linear model based on
# passed in number of observations for dataset and standard error of the error term
simulation <- function(obs_num, std_err, beta_0_val, beta_1_val){
  # Set up model and simulation parameters:
  observations <- obs_num # observations in dataframe
  true_beta_0 <- beta_0_val
  true_beta_1 <- beta_1_val
  err_std_deviation <- std_err
  # generating x values for the model: We estimate education from year 1 to year 16(undergrad)
  education <- 1:16 %>% 
    sample(size=observations, replace = TRUE)
  # creating y values based on the sample of x values and offsetting it by the
  # error term times a number form the normal distribution.
  y <- true_beta_0 + true_beta_1*education + (err_std_deviation*rnorm(n = observations))
  # creating an ols model from our x and y vector:
  ols_results <- lm(y ~ education)
  plot(jitter(x = education), y = y,pch = 19, col= 'steelblue')
  abline(ols_results,lwd=3, col= "darkgreen")
  Sys.sleep(.075)
  ols_results
}
# for practice,
# running a single simulation where:
# create a dataset with 100 observations, set std error to 10,000,
# and set the true value for beta_0(intercept) and beta_1(slope)
simulation(obs_num = 100, std_err = 10000,beta_0_val = 12000,beta_1_val = 1000)


# using replicate to run simulation n times
# stores result in a list- each element is an lm object
sim_reps <- replicate(n = 50, 
                      expr = simulation(obs_num = 100, std_err = 10000,beta_0_val = 12000,beta_1_val = 1000),
                      simplify = FALSE)
sim_reps[[1]]
sim_reps[[2]]
class(sim_reps)
class(sim_reps[[2]])

# a) Generate t-statistics for the coefficient on education 
# for each simulation.
# What are the minimal and max values of these t-statistics?
sim_reps %>% 
  map_df(tidy) %>% 
  filter(term == 'education') %>% 
  pull(statistic) %>% 
  max()

sim_reps %>% 
  map_df(tidy) %>% 
  filter(term == 'education') %>% 
  pull(statistic) %>% 
  min()

sim_reps %>% 
  map_df(tidy) %>% 
  filter(term == 'education') %>% 
  pull(statistic) %>% 
  mean()

# b) generate two-sided p values for the coeffcient
# on education for each simulation,
# what are the min and max values of these p-values
sim_reps %>% 
  map_df(tidy) %>% 
  filter(term == 'education') %>% 
  pull(p.value) %>% 
  max()

sim_reps %>% 
  map_df(tidy) %>% 
  filter(term == 'education') %>% 
  pull(p.value) %>% 
  min()

# c) In what percent of the simulation do we reject the null hyp:
# that beta_1 = 0 at alpha = 0.05 with a two-sided hypothesis?
sim_reps %>% 
  map_df(tidy) %>% 
  filter(term == 'education') %>% 
  pull(p.value) %>% 
  {. < .05} %>% 
  mean()
# we reject null 98% of the time

# d) Rerun the simulations, but see the true value
# of beta_1 = zero.
# Do tis for 500 simulations, and report
# what percent of the time we reject the null at .05 for
# a two-sided hypothesis test.


sim_reps_2 <- replicate(n = 500, 
                        expr =simulation(obs_num = 100, std_err = 10000,beta_0_val = 12000,beta_1_val = 0),
                        simplify = FALSE
)
class(sim_reps_2)
sim_reps_2[[1]]
sim_reps_2[[498]]
class(sim_reps_2[[1]])
sim_reps_2 %>% 
  map_df(tidy) %>% 
  filter(term == 'education') %>% 
  pull(p.value) %>% 
  {. < .05} %>% 
  mean()
# so we only reject the null hypothesis 5% of the time when
# we set the value of education to be 0
# so when the null hypothesis is correct,
# 5% of the time we will commit a type 1 error


sim_reps_2 %>% 
  map_df(tidy) %>% 
  filter(term == 'education') %>% 
  pull(estimate) %>% 
  mean()

# map_df equivalent to
sim_reps %>% 
  map(tidy) %>% 
  bind_rows()


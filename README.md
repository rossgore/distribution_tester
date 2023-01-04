# distribution_tester
This code implements several well-known parametric and nonparametric hypothesis tests. These tests are intended to assess if two samples of data contain  statistically significant differences such that one could conclude they were generated from different distributions.

Parametric tests are those that make assumptions about the parameters of the population distribution from which the sample is drawn. This is often the assumption that the population data are normally distributed. Non-parametric tests are “distribution-free” and, as such, can be used for non-Normal variables. The direct applicability and generality of nonparametric tests are the reasons for their usefulness in real-data applications.

The parametric tests included in our approach are: T-Test (Paired or UnPaired) and One-Way ANOVA.

The non-parametric tests included in our approach are: Kolmogorov-Smirnov Tests, Wilcoxon signed-rank test, Cramer-von Mises Statistic Test, Anderson–Darling Test and the Chi-Squared Test.


## Requirements
The code is written in the R Programming Language. There are two packages required to run the code. These are tidyverse and goftest.

## Directory Structure

The code for the software is located in the src directory.
The output directory provides examples of the results of running test_distributions.R and test_distributions_of_variables_from_multiple_simulation_runs.R

## Usage
src/test_distribution.R demonstrates how to use the software. 

The user is required to provide two samples of numeric data and indicate if the two samples are "paired".

The software provides back the p-value of running all the parametric and non-parametric tests on the data.

  # dummy distribution parameters for sample1 and sample2
  sample1_number_of_observtations <- 50
  sample1_mean <- 0
  sample1_std_dev <- 1
  
  sample2_number_of_observtations <- 50
  sample2_mean <- 0
  sample2_std_dev <- 2
  
  
  sample1_data = rnorm(n = sample1_number_of_observtations, mean = sample1_mean, sd = sample1_std_dev)
  sample2_data = rnorm(n = sample2_number_of_observtations, mean = sample2_mean, sd = sample2_std_dev)
  
  # this variable should be set to push pairedness appropriately to statistical tests
  is_the_sample_data_paired = FALSE
  
  # run the two sample tests and collect results in a table
  test_tbl = test_if_distributions_are_equal(sample1_data, sample2_data, is_the_sample_data_paired)
  
  # print or view
  print(test_tbl)

For simulation run data in a specified file format the test_distributions_of_variables_from_multiple_simulation_runs.R demonstrates how to quickly test to see if the values of every simulation output from two runs are from different distributions.

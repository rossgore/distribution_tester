library(tidyverse)
source("src/test_distribution_utils.R")

# dummy distribution parameters for sample1 and sample2
sample1_number_of_observtations <- 50
sample1_mean <- 0
sample1_std_dev <- 1

sample2_number_of_observtations <- 50
sample2_mean <- 0
sample2_std_dev <- 2


sample1_data = rnorm(n = sample1_number_of_observtations, mean = sample1_mean, sd = sample1_std_dev)
sample2_data = rnorm(n = sample2_number_of_observtations, mean = sample2_mean, sd = sample2_std_dev)

# i am relatively confident our sample data is paired, if so this variable should be true
# to push pairedness appropriately to statistical tests
is_the_sample_data_paired = FALSE

# run the two sample tests and collect results in a table
test_tbl = test_if_distributions_are_equal(sample1_data, sample2_data, is_the_sample_data_paired)

# print or view
print(test_tbl)

# can also write out to file
write_csv(test_tbl, "output/simple_dummy_data_tests_for_same_distribution_two_data_samples.csv")
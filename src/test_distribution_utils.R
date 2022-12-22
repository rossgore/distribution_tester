library(tidyverse)
library(goftest)

two_sample_cvm_test <- function(x, y) {
  
  # Sample sizes
  n <- length(x)
  m <- length(y)
  
  # Pooled sample
  z <- c(x, y)
  
  # Statistic computation via ecdf()
  cvm_stat = (n * m / (n + m)^2) * sum((ecdf(x)(z) - ecdf(y)(z))^2)
  
  cvm_pvalue = 1 - pCvM(q = cvm_stat) # goftest function
  to_return = tibble("cmv_statistic" = cvm_stat, "p_value"= cvm_pvalue)
  return(to_return)
}

two_sample_ad_test <- function(x, y) {
  
  # Sample sizes
  n <- length(x)
  m <- length(y)
  
  # Pooled sample and pooled ecdf
  z <- c(x, y)
  z <- z[-which.max(z)] # Exclude the largest point
  H <- rank(z) / (n + m)
  
  # Statistic computation via ecdf()
  ad_stat = (n * m / (n + m)^2) * sum((ecdf(x)(z) - ecdf(y)(z))^2 / ((1 - H) * H))
  
  ad_pvalue = 1 - pAD(q = ad_stat) # goftest function
  to_return = tibble("ad_statistic" = ad_stat, "p_value"= ad_pvalue)
  return(to_return)
}

test_if_distributions_are_equal = function(sample1, sample2, sample_data_pairedness)
{
  
  # generate data from distributions and store in seperate tbls
  sample1_tbl <- tibble(group = "sample1",n = sample1)
  sample2_tbl <- tibble(group = "sample2", n = sample2)
  
  
  ## 
  ##  Two-sample paired t-tests
  ## 
  ## data:  sample1 and sample2
  # sample1 = sample 2 in terms of paired t test (i.e. do sample1 and sample2 have means that are statistically insignificant from one another?)
  t_test_equal_name = "Two-sample t-test"
  if (sample_data_pairedness)
  {
    t_test_equal_name = "Two-sample paired t-test"
  }
  t_test_equal_type = "Sample1 = Sample 2"
  t_test_equal_description = "Do sample1 and sample2 have means such that the difference between them is statistically insignificant?"
  t_test_equal_result <- t.test(sample1_tbl$n, sample2_tbl$n, paired = sample_data_pairedness, alternative="two.sided")
  
  # sample1 > sample 2 in terms of paired t test (i.e. does sample1 have a mean that is statistically significantly less than the mean of sample2?)
  t_test_less_than_name = t_test_equal_name
  t_test_less_than_type = "Sample1 < Sample 2"
  t_test_less_than_description = "Does sample1 have a mean that is statistically significantly less than the mean of sample2?"
  t_test_less_than_result <- t.test(sample1_tbl$n, sample2_tbl$n, paired = sample_data_pairedness, alternative="less")
  
  # sample1 < sample 2 in terms of paired t test (i.e. does sample1 have a mean that is statistically significantly greater than the mean of sample2?)
  t_test_greater_than_name = t_test_equal_name
  t_test_greater_than_type = "Sample1 > Sample 2"
  t_test_greater_than_description = "Does sample1 have a mean that is statistically significantly greater than the mean of sample2?"
  t_test_greater_than_result <- t.test(sample1_tbl$n, sample2_tbl$n, paired = sample_data_pairedness, alternative="greater")
  
  t_test_names = c(t_test_equal_name, t_test_less_than_name, t_test_greater_than_name)
  t_test_types = c(t_test_equal_type, t_test_less_than_type, t_test_greater_than_type)
  t_test_descriptions = c(t_test_equal_description, t_test_less_than_description, t_test_greater_than_description)
  t_test_results = c(t_test_equal_result$p.value, t_test_less_than_result$p.value, t_test_greater_than_result$p.value)
  
  ## 
  ##  Two-sample paired Wilcoxon signed-rank test (i.e. non-parametric t test)
  ## 
  ## data:  sample1 and sample2
  # sample1 = sample 2 in terms of paired Wilcoxon signed-rank test (i.e. do sample1 and sample2 have statistically insignificant "shift in their main masses of probability" from one another)
  wilcox_test_equal_name = "Two-sample Wilcoxon signed-rank test (i.e. non-parametric t test)"
  if (sample_data_pairedness)
  {
    wilcox_test_equal_name = "Two-sample paired Wilcoxon signed-rank test (i.e. non-parametric t test)"
  }
  wilcox_test_equal_type = "Sample1 = Sample 2"
  wilcox_test_equal_description = "Is any shift in the main masses of the probability of sample1 and sample2 statistically insignificant"
  wilcox_test_equal_result = wilcox.test(x = sample1_tbl$n, y = sample2_tbl$n, paired = sample_data_pairedness, alternative="two.sided")
  
  # sample1 < sample 2 in terms of paired Wilcoxon signed-rank test (i.e. Is the shift the main mass of the probability of sample1 from sample2 less than 0 and statistically significant)
  wilcox_test_less_than_name = wilcox_test_equal_name
  wilcox_test_less_than_type = "Sample1 < Sample 2"
  wilcox_test_less_than_description = "Is the shift in the main mass of the probability of sample1 from sample2 greater than 0 and statistically significant"
  wilcox_test_less_than_result = wilcox.test(x = sample1_tbl$n, y = sample2_tbl$n, paired = sample_data_pairedness, alternative = "less") 
  
  # sample1 < sample 2 in terms of paired Wilcoxon signed-rank test (i.e. Is the shift the main mass of the probability of sample1 from sample2 greater than 0 and statistically significant)
  wilcox_test_greater_than_name = wilcox_test_equal_name
  wilcox_test_greater_than_type = "Sample1 > Sample 2"
  wilcox_test_greater_than_description = "Is the shift in the main mass of the probability of sample1 from sample2 greater than 0 and statistically significant"
  wilcox_test_greater_than_result = wilcox.test(x = sample1_tbl$n, y = sample2_tbl$n, paired = sample_data_pairedness, alternative = "greater")
  
  wilcox_test_names = c(wilcox_test_equal_name, wilcox_test_less_than_name, wilcox_test_greater_than_name)
  wilcox_test_types = c(wilcox_test_equal_type, wilcox_test_less_than_type, wilcox_test_greater_than_type)
  wilcox_test_descriptions = c(wilcox_test_equal_description, wilcox_test_less_than_description, wilcox_test_greater_than_description)
  wilcox_test_results = c(wilcox_test_equal_result$p.value, wilcox_test_less_than_result$p.value, wilcox_test_greater_than_result$p.value)
  ## 
  ##  Two-sample Kolmogorov-Smirnov tests
  ## 
  ## data:  sample1 and sample2
  
  # sample1 = sample 2 in terms of ks test (i.e. do sample1 and sample2 have the same asymptotic cdf using the maximum signed distance?)
  ks_test_equal_test_name = "Two-sample Kolmogorov-Smirnov Tests"
  ks_test_equal_test_type = "Sample1 = Sample 2"
  ks_test_equal_test_description = "Do sample1 and sample2 have the same asymptotic cdf using the maximum signed distance?"
  ks_test_equal_result <- ks.test(x = sample1_tbl$n, y = sample2_tbl$n, alternative="two.sided")
  
  # sample1 < sample 2 in terms of ks test (i.e. does cdf of sample1 lie below cdf of sample2 using the maximum signed distance?)
  ks_test_less_than_result_test_name = "Two-sample Kolmogorov-Smirnov Tests"
  ks_test_less_than_result_test_type = "Sample1 < Sample 2"
  ks_test_less_than_result_test_description = "Does the cdf of sample1 lie below the cdf of sample2 using the maximum signed distance?"
  ks_test_less_than_result <- ks.test(x = sample1_tbl$n, y = sample2_tbl$n, alternative = "less")
  
  # sample1 > sample 2 in terms of ks test (i.e. does cdf of sample1 lie above cdf of sample2 using the maximum signed distance?)
  ks_test_greater_than_result_test_name = "Two-sample Kolmogorov-Smirnov Tests"
  ks_test_greater_than_result_test_type = "Sample1 > Sample 2"
  ks_test_greater_than_result_test_description = "Does the cdf of sample1 lie above the cdf of sample2 using the maximum signed distance?"
  ks_test_greater_than_result <- ks.test(x = sample1_tbl$n, y = sample2_tbl$n, alternative = "greater")
  
  ks_test_names = c(ks_test_equal_test_name, ks_test_less_than_result_test_name, ks_test_greater_than_result_test_name)
  ks_test_types = c(ks_test_equal_test_type, ks_test_less_than_result_test_type, ks_test_greater_than_result_test_type)
  ks_test_descriptions = c(ks_test_equal_test_description, ks_test_less_than_result_test_description, ks_test_greater_than_result_test_description)
  ks_test_results = c(ks_test_equal_result$p.value, ks_test_less_than_result$p.value, ks_test_greater_than_result$p.value)
  
  ##
  ## Two-sample Cramér-von Mises statistic test
  ##
  ## data:  sample1 and sample2
  #sample1 = sample 2 in terms of cvm test (i.e. do sample1 and sample2 have the same asymptotic cdf using quadratic difference?)
  cvm_test_equal_test_name = "Two-sample Cramer-von Mises Statistic Test"
  cvm_test_equal_type = "Sample1 = Sample 2"
  cvm_test_equal_description = "Do sample1 and sample2 have the same asymptotic cdf using quadratic differences?"
  cvm_test_equal_result <- two_sample_cvm_test(x = sample1_tbl$n, sample2_tbl$n)
  
  cvm_test_names = c(cvm_test_equal_test_name)
  cvm_test_types = c(cvm_test_equal_type)
  cvm_test_descriptions = c(cvm_test_equal_description)
  cvm_test_results = c(cvm_test_equal_result$p_value)
  
  ##
  ## Two-sample Two-sample Anderson–Darling test
  ##
  ## data:  sample1 and sample2
  #sample1 = sample 2 in terms of ad test (i.e. do sample1 and sample2 have the same asymptotic cdf using weighted quadratic differences?)
  ad_test_equal_test_name = "Two-sample Anderson–Darling test"
  ad_test_equal_type = "Sample1 = Sample 2"
  ad_test_equal_description = "Do sample1 and sample2 have the same asymptotic cdf using weighted quadratic differences?"
  ad_test_equal_result <- two_sample_ad_test(x = sample1_tbl$n, sample2_tbl$n)
  
  ad_test_names = c(ad_test_equal_test_name)
  ad_test_types = c(ad_test_equal_type)
  ad_test_descriptions = c(ad_test_equal_description)
  ad_test_results = c(ad_test_equal_result$p_value)
  
  
  chi_squared_test_name = "Two-sample Chi–Squared test"
  chi_squared_test_type = "Sample 1 = Sample 2"
  chi_squared_test_equal_description = "Are there no statistically significant difference in the distribution of the discrete data in Sample 1 and Sample 2?"
  chi_squared_test_results = chisq.test(sample1_tbl$n, sample2_tbl$n, simulate.p.value = TRUE)
  
  chi_squared_test_names = c(chi_squared_test_name)
  chi_squared_test_types = c(chi_squared_test_type)
  chi_squared_test_descriptions = c(chi_squared_test_equal_description)
  chi_squared_test_results = c(chi_squared_test_results$p.value)
  
  # pretty results tbl
  test_names = c(t_test_names, wilcox_test_names, ks_test_names, cvm_test_names, ad_test_names, chi_squared_test_names)
  test_types = c(t_test_types, wilcox_test_types, ks_test_types, cvm_test_types, ad_test_types, chi_squared_test_types)
  test_descriptions = c(t_test_descriptions, wilcox_test_types, ks_test_types, cvm_test_types, ad_test_types, chi_squared_test_descriptions)
  test_p_values = c(t_test_results, wilcox_test_results, ks_test_results, cvm_test_results, ad_test_results, chi_squared_test_results)
  
  results_tbl = tibble(test_name=test_names, test_type=test_types, test_description=test_descriptions, test_p_value=test_p_values)
  
}

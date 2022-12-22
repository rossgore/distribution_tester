# distribution_tester
This code implements several well-known parametric and nonparametric hypothesis tests. These tests are intended to assess if two samples of data contain  statistically significant differences such that one could conclude they were generated from different distributions.

Parametric tests are those that make assumptions about the parameters of the population distribution from which the sample is drawn. This is often the assumption that the population data are normally distributed. Non-parametric tests are “distribution-free” and, as such, can be used for non-Normal variables. The direct applicability and generality of nonparametric tests are the reasons for their usefulness in real-data applications.

The parametric tests included in our approach are: T-Test (Paired or UnPaired) and One-Way ANOVA.

The non-parametric tests included in our approach are: Kolmogorov-Smirnov Tests, Wilcoxon signed-rank test, Cramer-von Mises Statistic Test, Anderson–Darling Test and the Chi-Squared Test.

## Usage
test_distribution.R demonstrates how to use the software. 

The user is required to provide two samples of numeric data and indicate if the two samples are "paired".

The software provides back the p-value of running all the parametric and non-parametric tests on the data.

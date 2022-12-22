library(tidyverse)
source("src/test_distribution_utils.R")


# read in simulation run #1
simulation_run_1 = read_csv("output/simulation_run_1.csv")

# read in simulation run #2
simulation_run_2 = read_csv("output/simulation_run_2.csv")

# be sure things are arranged by date and then runtime
simulation_run_1 = simulation_run_1 %>% arrange(date, runtime_stamp)
simulation_run_2 = simulation_run_2 %>% arrange(date, runtime_stamp)

# i am relatively confident our sample data is paired, if so this variable should be true
# to push pairedness appropriately to statistical tests
is_the_sample_data_paired = FALSE


# edit these lists for types we want to do analysis on

# overflow removed here b/c it is currently static
var_types_for_analysis = c("alos_rate", "population", "releases", 
              "beds_in_use", "los", "ice_pend", "ice_crim", 
              "ice_other", "cbp_pend", "cbp_crim", "cbp_other", "bookin_rate", 
              "arrival_time", "releases_dt")

sim_types_for_analysis = c("full_fact", "alos_span", "bi_span")

var_measurements_for_analysis = c("mean", "std", "ci_hi_95", "ci_low_95")



# set up the big tbl to collect all of our individual test results
distribution_test_tbl_results_for_sim_runs = tibble(sim_type=character(),
                                                    sim_var=character(),
                                                    var_measurement = character(),
                                                    test_name=character(), 
                                                    test_type=character(), 
                                                    test_description=character(), 
                                                    test_p_value=double())

for (i in 1:length(sim_types_for_analysis)) # for each analysis type
{
  current_sim_type = sim_types_for_analysis[i]
  
  for (j in 1:length(var_types_for_analysis)) # for each variable type of interest
  {
    current_var_type = var_types_for_analysis[j]
    
    # filter both samples so its just this variable type
    current_var_specific_data_for_sim_type_from_run_1 = simulation_run_1 %>% filter(sim_type == current_sim_type &
                                                                                      var == !!current_var_type)
    current_var_specific_data_for_sim_type_from_run_2 = simulation_run_2 %>% filter(sim_type == current_sim_type &
                                                                                      var == !!current_var_type)
    
    for (k in 1:length(var_measurements_for_analysis)) # for each variable measurement of interest
    {
      current_var_measurement = var_measurements_for_analysis[k]
      
      # get the column pertaining to the measurement type
      sample1_data = current_var_specific_data_for_sim_type_from_run_1 %>% select(!!current_var_measurement) %>% unlist()
      sample2_data = current_var_specific_data_for_sim_type_from_run_2 %>% select(!!current_var_measurement) %>% unlist()
      
      # run the test to see if distribution is the "same" given the two samples
      test_tbl_for_var = test_if_distributions_are_equal(sample1_data, sample2_data, is_the_sample_data_paired)
      
      # add metadata to organize the big tbl of results
      test_tbl_for_var = test_tbl_for_var %>% mutate(sim_type = current_sim_type)
      test_tbl_for_var = test_tbl_for_var %>% mutate(sim_var = current_var_type)
      test_tbl_for_var = test_tbl_for_var %>% mutate(var_measurement = current_var_measurement)
      
      # order rows same as the big tbl
      test_tbl_for_var = test_tbl_for_var %>% select(sim_type, sim_var, var_measurement, test_name, test_type, test_description, test_p_value)
      
      # add it into the big tibble
      distribution_test_tbl_results_for_sim_runs = distribution_test_tbl_results_for_sim_runs %>% bind_rows(test_tbl_for_var)
    }
    
  }
}

# print or View this data structure
print(distribution_test_tbl_results_for_sim_runs)

# can also write out to file
write_csv(distribution_test_tbl_results_for_sim_runs, "output/distribution_test_tbl_results_for_sim1_vs_sim2.csv")

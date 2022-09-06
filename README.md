# data_uncertainty

This repository is for the running of T1.1 as part of WP1 for my International Mobility Grant: PREDICT. 

## Aim of T1.1

Simulation study to look at impact of data uncertainty (error and bias) on estimates of vital rates, 
population growth rate, stable size distribution, and consequently extinction probability. 

This study looks at two scenarios:

1) Model assumptions regarding fecundity data are violated. Specifically there are missing reproductive events (at random and bias to particular age groups).
2) The impact of modelling measurement error explicitly or ignoring it.  

For each scenario we will use three models: a naive MPM with no observation process for fecundity, an MPM with an observation process (random error), and an integrated population model.

This work also links to T1.2 of this grant and considers 1 parameter uncertainty scenario: how different are conclusions if you ignore parameter uncertainty at point of vital rates or in lambda. (Explore this for a stable population, decreasing, and increasing - all with baseline scenario). 

## Workflow for repository

Functions to simulate data (survival and reproduction functions) are wrapped in process_input_data function and run_simulation. 

To run the simulations there are specific scripts for certain scenarios. T1.1_Scenario_1_data is for the data uncertainty simulations and the T1.2_Scenario_2_data is for the parameter uncertainty scenarios. 

Code to run models in Nimble is saved as its own script and called in the Run_model scripts. 

## Contents of repository

### Functions

- (survival_function)[https://github.com/emilygsimmonds/data_uncertainty/blob/main/Functions/survival_function.R]: Function to apply survival process during data simulation
- (reproduction_function)[https://github.com/emilygsimmonds/data_uncertainty/blob/main/Functions/reproduction_function.R]: Function to apply reproduction process during data simulation
- (process_input_data)[https://github.com/emilygsimmonds/data_uncertainty/blob/main/Functions/process_input_data.R]: Function to edit previous year's output (t) data into a format read for year t+1
- (run_simulation)[https://github.com/emilygsimmonds/data_uncertainty/blob/main/Functions/run_simulation.R]: Function to run the simulations across multiple years
- (make_input_data_function)[https://github.com/emilygsimmonds/data_uncertainty/blob/main/Functions/make_input_data_function.R]: Function to correctly format initial values, constants, and data for input to nimble model

### Scripts

- (Checks)[]
- (Checks_model)[]
- (T1.1_Model_SS)[]
- (T1.1_Model_SS_hmm)[]
- (T1.1_Scenario_1_data)[]
- (T1.2_Scenario_2_data)[]

### Data files



# data_uncertainty

This repository is for the running of T1.1 as part of WP1 for my International Mobility Grant: PREDICT. 

## Aim of T1.1

Simulation study to look at impact of data error (bias) on estimates of vital rates, 
population growth rate, stable size distribution, and extinction probability. 

This study looks at three scenarios:

1) Model assumptions regarding fecundity data are violated. Specifically there are missing reproductive events and error in counting.
2) Measurement error is not modelled explicitly. 
3) How does time series length impact the precision and accuracy of vital rate estimates and derived population measures under scenarios 1 and 2. 

## Workflow for repository

## Contents of repository

### Functions

- (survival_function)[https://github.com/emilygsimmonds/data_uncertainty/blob/main/Functions/survival_function.R]: Function to apply survival process during data simulation
- (reproduction_function)[https://github.com/emilygsimmonds/data_uncertainty/blob/main/Functions/reproduction_function.R]: Function to apply reproduction process during data simulation
- (process_input_data)[https://github.com/emilygsimmonds/data_uncertainty/blob/main/Functions/process_input_data.R]: Function to edit previous year's output (t) data into a format read for year t+1
- (run_simulation)[https://github.com/emilygsimmonds/data_uncertainty/blob/main/Functions/run_simulation.R]: Function to run the simulations across multiple years

### Scripts

- (Doi_to_pdf)[https://github.com/emilygsimmonds/data_uncertainty/blob/main/Scripts/Doi_to_pdf.R]: Script to transform csv of DOIs into Urls or PDFS
- (Explore_data)[https://github.com/emilygsimmonds/data_uncertainty/blob/main/Scripts/Explore_data.R]: Script to explore COMADRE data
- (Pull_comadre)[https://github.com/emilygsimmonds/data_uncertainty/blob/main/Scripts/Pull_comadre.R]: Script to pull matrices from COMADRE/COMPADRE databases

### Data files

- (DOIS.csv)[]. List of DOIs and Urls for papers selected for T1.2
- (working_comadre.RData)[]. Subset of matrices from COMADRE database that match criteria needed for T1.2

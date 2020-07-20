# This script runs all the other scripts
# Scripts should be run in order as some require the output of others
# Working directory should be changed to the location of the folder
# Kyle McGraw, July 2019

# Clears the R environment
rm(list=ls())
gc()

# Imports libraries used
library(dplyr)
library(data.table)
library(xtable)
library(aod)
library(ggplot2)

# Sets working directory
setwd("/Users/User/OneDrive - California Institute of Technology/Caltech/Frosh/SURF/Surgery SURF/Surgical Risk Calculator SURF")

#### Import and Recoding of Data ####
source("data-processing/data_processing_puf16.R")
source("data-processing/data_processing_puf17.R")
source("data-processing/data_processing_puf18.R")

#### Outcome Grouping ####
source("data-processing/outcome_grouping_puf16.R")
source("data-processing/outcome_grouping_puf17.R")
source("data-processing/outcome_grouping_puf18.R")

#### Generation of Simmple Verification Material and Tables ####
source("analytics/data_quality_testing_puf16.R")
source("analytics/data_quality_testing_puf17.R")
source("analytics/data_quality_testing_puf18.R")
source("analytics/demographics_by_year.R")
source("analytics/outcomes_by_year.R")

#### Simple Models ####
source("analytics/simple_models.R")

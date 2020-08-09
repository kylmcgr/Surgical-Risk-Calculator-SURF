# This script runs all the other scripts
# Scripts should be run in order as some require the output of others
# Working directory should be changed to the location of the data
# Path should be changed to the location of the files
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
library(lme4)
library(descr)

# Sets working directory
setwd("/Users/User/Documents/Surgical Risk Calculator SURF")


#### Import and Recoding of Data ####
source(paste0("./data-processing/", "data_processing_puf16.R"))
source(paste0("./data-processing/", "data_processing_puf17.R"))
source(paste0("./data-processing/", "data_processing_puf18.R"))

#### Outcome Grouping ####
source(paste0("./data-processing/", "outcome_grouping_puf16.R"))
source(paste0("./data-processing/", "outcome_grouping_puf17.R"))
source(paste0("./data-processing/", "outcome_grouping_puf18.R"))

#### Generation of Simmple Verification Material and Tables ####
source(paste0("./analytics/", "data_quality_testing_puf16.R"))
source(paste0("./analytics/", "data_quality_testing_puf17.R"))
source(paste0("./analytics/", "data_quality_testing_puf18.R"))
source(paste0("./analytics/", "demographics_by_year.R"))
source(paste0("./analytics/", "outcomes_by_year.R"))
source(paste0("./analytics/", "surgery_by_year.R"))

#### Models ####
source(paste0("./analytics/", "simple_models.R"))
source(paste0("./analytics/", "NSQIP_model.R"))

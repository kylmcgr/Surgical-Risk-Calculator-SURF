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

# Sets working directory and get path of files
setwd("/Users/User/Documents/NSQIP Surgical Data")
path <- "/Users/User/OneDrive - California Institute of Technology/Caltech/Frosh/SURF/Surgery SURF/Surgical Risk Calculator SURF"


#### Import and Recoding of Data ####
source(paste(path, "data-processing/data_processing_puf16.R", sep = "/"))
source(paste(path, "data-processing/data_processing_puf17.R", sep = "/"))
source(paste(path, "data-processing/data_processing_puf18.R", sep = "/"))

#### Outcome Grouping ####
source(paste(path, "data-processing/outcome_grouping_puf16.R", sep = "/"))
source(paste(path, "data-processing/outcome_grouping_puf17.R", sep = "/"))
source(paste(path, "data-processing/outcome_grouping_puf18.R", sep = "/"))

#### Generation of Simmple Verification Material and Tables ####
source(paste(path, "analytics/data_quality_testing_puf16.R", sep = "/"))
source(paste(path, "analytics/data_quality_testing_puf17.R", sep = "/"))
source(paste(path, "analytics/data_quality_testing_puf18.R", sep = "/"))
source(paste(path, "analytics/demographics_by_year.R", sep = "/"))
source(paste(path, "analytics/outcomes_by_year.R", sep = "/"))

#### Models ####
source(paste(path, "analytics/simple_models.R", sep = "/"))
source(paste(path, "analytics/NSQIP_model.R", sep = "/"))

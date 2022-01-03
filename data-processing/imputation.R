# This script does hot-deck imputation the processed data from the NSQIP 2016, 2017, 2018 datasets
# data_processing_puf16.R, data_processing_puf17.R, and data_processing_puf18.R must be run prior
# to running the script for the preimpute_pred_puf16, preimpute_pred_puf17, preimpute_pred_puf18,
# preimpute_outcomes_puf16, preimpute_outcomes_puf17, and preimpute_outcomes_puf18 objects.
# This file generates latex code for a table of outcomes: outcomes.tex
# Kyle McGraw, July 2019

library(simputation)
library(dplyr)

### Import Data ###
load("./data/preimpute_pred_puf16.Rda")
load("./data/preimpute_outcomes_puf16.Rda")
load("./data/preimpute_pred_puf17.Rda")
load("./data/preimpute_outcomes_puf17.Rda")
load("./data/preimpute_pred_puf18.Rda")
load("./data/preimpute_outcomes_puf18.Rda")


pred_puf16 <- mutate_all(impute_shd(preimpute_pred_puf16,
                                 . ~ .),function(x) as.numeric(x))

outcomes_puf16 <- mutate_all(impute_shd(preimpute_outcomes_puf16,
                                  . ~ .),function(x) as.numeric(x))
pred_puf17 <- mutate_all(impute_shd(preimpute_pred_puf17,
                                  . ~ .),function(x) as.numeric(x))
outcomes_puf17 <- mutate_all(impute_shd(preimpute_outcomes_puf17,
                                      . ~ .),function(x) as.numeric(x))
pred_puf18 <- mutate_all(impute_shd(preimpute_pred_puf18,
                                  . ~ .),function(x) as.numeric(x))
outcomes_puf18 <- mutate_all(impute_shd(preimpute_outcomes_puf18,
                                      . ~ .),function(x) as.numeric(x))


save(pred_puf16, file = paste0("./data/", "pred_puf16.Rda"))
save(outcomes_puf16, file = paste0("./data/", "outcomes_puf16.Rda"))
save(pred_puf17, file = paste0("./data/", "pred_puf17.Rda"))
save(outcomes_puf17, file = paste0("./data/", "outcomes_puf17.Rda"))
save(pred_puf18, file = paste0("./data/", "pred_puf18.Rda"))
save(outcomes_puf18, file = paste0("./data/", "outcomes_puf18.Rda"))
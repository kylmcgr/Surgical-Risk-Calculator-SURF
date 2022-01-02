# This script does hot-deck imputation the processed data from the NSQIP 2016, 2017, 2018 datasets
# data_processing_puf16.R, data_processing_puf17.R, and data_processing_puf18.R must be run prior
# to running the script for the preimpute_pred_puf16, preimpute_pred_puf17, preimpute_pred_puf18,
# preimpute_outcomes_puf16, preimpute_outcomes_puf17, and preimpute_outcomes_puf18 objects.
# This file generates latex code for a table of outcomes: outcomes.tex
# Kyle McGraw, July 2019

library(simputation)

### Import Data ###
load("./data/preimpute_pred_puf16.Rda")
load("./data/preimpute_outcomes_puf16.Rda")
load("./data/preimpute_pred_puf17.Rda")
load("./data/preimpute_outcomes_puf17.Rda")
load("./data/preimpute_pred_puf18.Rda")
load("./data/preimpute_outcomes_puf18.Rda")


hot_deck_pred_puf16 <- impute_shd(pred_puf16,
                                 . ~ .,
                                 pool = c("complete",
                                          "univariate",
                                          "multivariate"))
hot_deck_outcomes_puf16 <- impute_shd(outcomes_puf16,
                                  . ~ .,
                                  pool = c("complete",
                                           "univariate",
                                           "multivariate"))
hot_deck_pred_puf17 <- impute_shd(pred_puf17,
                                  . ~ .,
                                  pool = c("complete",
                                           "univariate",
                                           "multivariate"))
hot_deck_outcomes_puf17 <- impute_shd(outcomes_puf17,
                                      . ~ .,
                                      pool = c("complete",
                                               "univariate",
                                               "multivariate"))
hot_deck_pred_puf18 <- impute_shd(pred_puf18,
                                  . ~ .,
                                  pool = c("complete",
                                           "univariate",
                                           "multivariate"))
hot_deck_outcomes_puf18 <- impute_shd(outcomes_puf18,
                                      . ~ .,
                                      pool = c("complete",
                                               "univariate",
                                               "multivariate"))


save(hot_deck_pred_puf16, file = paste0("./data/", "pred_puf16.Rda"))
save(hot_deck_outcomes_puf16, file = paste0("./data/", "outcomes_puf16.Rda"))
save(hot_deck_pred_puf17, file = paste0("./data/", "pred_puf17.Rda"))
save(hot_deck_outcomes_puf17, file = paste0("./data/", "outcomes_puf17.Rda"))
save(hot_deck_pred_puf18, file = paste0("./data/", "pred_puf18.Rda"))
save(hot_deck_outcomes_puf18, file = paste0("./data/", "outcomes_puf18.Rda"))
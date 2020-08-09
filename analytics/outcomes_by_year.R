# This script creates a table of outcomes for the NSQIP 2016, 2017, and 2018 datasets
# outcome_grouping_puf16.R, outcome_grouping_puf17.R, and outcome_grouping_puf18.R must be run prior
# to running the script for the grouped_outcomes_puf16, grouped_outcomes_puf17, and grouped_outcomes_puf18 objects.
# This file generates latex code for a table of outcomes: outcomes.tex
# Kyle McGraw, July 2019

library(xtable)
library(tibble)

### Import Data ###
load("./data/grouped_outcomes_puf16.Rda")
load("./data/grouped_outcomes_puf17.Rda")
load("./data/grouped_outcomes_puf18.Rda")


#### Outcomes by Year ####

# Creates an empty data frame with years as columns
outcomes <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("2016", "2017", "2018")
colnames(outcomes) <- x

# List of variable names to be used and labels for each variable in the table
outcome_names <- c("y_discharge", "y_dead", "y_reop", "y_readmit", "y_sup_ssi", "y_deep_ssi", "y_organ_ssi", "y_wound_disruption", "y_pneumonia", "y_unplanned_intubation", "y_emb", "y_vent", "y_PRF", "y_ARF", "y_uti", "y_stroke", "y_cpr", "y_mi", "y_trans", "y_thromb", "y_sepsis", "y_sepshock", "y_cdiff")
outcome_labels <- c("Discharged", "Dead", "Reoperation", "Readmission", "Superficial Incisional SS", "Deep Incisional SSI", "Organ SSI", "Wound Disruption", "Pneumonia", "Unplanned Intubation", "Pulmonary Embolism", "Ventilator", "Progressive Renal Failure", "Acute Renal Failure", "Urinary Tract Infection", "Stroke", "Cardiac Arrest Requiring CPR", "Myocardial Infarction", "Bleeding Transfusions", "DVT/Thrombophlebitis", "Sepsis", "Septic Shock", "Clostridium Difficile Colitis")

# Adds each variable to the table
for (i in outcome_names){
  tble16 <- table(grouped_outcomes_puf16[[i]])
  tble17 <- table(grouped_outcomes_puf17[[i]])
  tble18 <- table(grouped_outcomes_puf18[[i]])
  outcomes <- add_row(outcomes, "2016" = tble16[2], "2017" = tble17[2], "2018" = tble18[2])
}

# Adds row names
row.names(outcomes) <- outcome_labels

# Export latex code to file
print(xtable(outcomes, caption = "Outcomes by Year", type = "latex"), file = paste0("./tables/", "outcomes.tex"))
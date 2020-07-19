# setwd("/Users/User/Documents/NSQIP Surgical Data")
outcomes <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("2016", "2017", "2018")
colnames(outcomes) <- x

outcome_names <- c("y_discharge", "y_dead", "y_reop", "y_related_reop", "y_readmit", "y_related_readmit", "y_sup_ssi", "y_deep_ssi", "y_organ_ssi", "y_wound_disruption", "y_pneumonia", "y_unplanned_intubation", "y_emb", "y_vent", "y_PRF", "y_ARF", "y_uti", "y_stroke", "y_cpr", "y_mi", "y_trans", "y_thromb", "y_sepsis", "y_sepshock", "y_cdiff")
outcome_labels <- c("Discharged", "Dead", "Reoperation", "Related Reoperation", "Readmission", "Related Readmission", "Superficial Incisional SS", "Deep Incisional SSI", "Organ SSI", "Wound Disruption", "Pneumonia", "Unplanned Intubation", "Pulmonary Embolism", "Ventilator", "Progressive Renal Failure", "Acute Renal Failure", "Urinary Tract Infection", "Stroke", "Cardiac Arrest Requiring CPR", "Myocardial Infarction", "Bleeding Transfusions", "DVT/Thrombophlebitis", "Sepsis", "Septic Shock", "Clostridium Difficile Colitis")

for (i in outcome_names){
  tble16 <- table(grouped_outcomes_puf16[[i]])
  tble17 <- table(grouped_outcomes_puf17[[i]])
  tble18 <- table(grouped_outcomes_puf18[[i]])
  outcomes <- add_row(outcomes, "2016" = tble16[2], "2017" = tble17[2], "2018" = tble18[2])
}

row.names(outcomes) <- outcome_labels

print(xtable(outcomes, caption = "Outcomes by Year", type = "latex"), file = paste("/Users/User/Documents/NSQIP Surgical Data/outcomes.tex", sep = ""))
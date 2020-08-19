library(dplyr)
library(xtable)
library(ROCR)
library(fuzzyforest)

### Setup Data ###
load("./data/pred_puf17.Rda")
load("./data/grouped_outcomes_puf17.Rda")

aucs <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("rf", "ff", "logit")
colnames(aucs) <- x

# took out because error, "y_cardiac", "y_renal", "y_dead", "y_discharge_care", "y_sepsis"
outcome_names <- c("y_serious", "y_any", "y_pneumonia", "y_SSI", "y_uti", "y_thromb", "y_readmit", "y_reop")
for (outcome in outcome_names){
  print(outcome)
  test <- mutate(pred_puf17, y_var = as.factor(grouped_outcomes_puf17[[outcome]]))
  levels(test$y_var) <- c("no_outcome", "outcome")
  plastic_test = filter(test, CPT_plastic == 1)

  
  load(paste0("./data/", outcome, "_rf.Rda"))
  rf_predict <- predict(plastic.rf, newdata = select(plastic_test, -y_var), type = "prob")
  rf_rocr <- prediction(rf_predict[, 2], plastic_test$y_var)
  rf_auc <- performance(rf_rocr, "auc")
  rf_perf <- performance(rf_rocr, "tpr", "fpr")
  
  load(paste0("./data/", outcome, "_ff.Rda"))
  ff_predict <- predict(plastic.ff$final_rf, newdata = select(plastic_test, -y_var), type = "prob")
  ff_rocr <- prediction(list(ff_predict[, 2]), plastic_test$y_var)
  ff_auc <- performance(ff_rocr, "auc")
  ff_perf <- performance(ff_rocr, "tpr", "fpr")
  
  load(paste0("./data/", outcome, "_logit.Rda"))
  logit_predict <- predict(plastic.logit, newdata = select(plastic_test, -y_var), type = "prob")
  logit_rocr <- prediction(logit_predict[, 2], plastic_test$y_var)
  logit_auc <- performance(logit_rocr, "auc")
  logit_perf <- performance(logit_rocr, "tpr", "fpr")
  
  aucs <- add_row(aucs, "rf" = rf_auc@y.values[[1]], "ff" = ff_auc@y.values[[1]], "logit" = logit_auc@y.values[[1]])
  
  pdf(paste0("./figures/", "ROC_", outcome, ".pdf"))
  plot(rf_perf, col="red")
  plot(ff_perf, add = TRUE, col="blue")
  plot(logit_perf, add = TRUE, col="green")
  legend("bottomright", c("rf", "ff", "logit"), lty=1, 
         col = c("red", "blue", "green"), bty="n", inset=c(0.15,0.15))
  dev.off()
  
}
  
# , "Cardiac Complication", "Renal Failure", "Death", "Discharge to Nursing or Rehab Facility", "Sepsis"
outcome_labels <- c("Serious Complication", "Any Complication", "Pneumonia", "Surgical Site Infection", "Urinary Tract Infection", "Venous Thromboembolism", "Readmission", "Return to OR")
row.names(aucs) <- outcome_labels
print(xtable(aucs, caption = "AUC by Model and Outcome for Plastic Surgery", type = "latex"), file = paste0("./tables/", "plastics_AUC.tex"))

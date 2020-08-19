library(dplyr)
library(caret)

### Setup Data ###
load("./data/pred_puf16.Rda")
load("./data/grouped_outcomes_puf16.Rda")

# took out because error, "y_cardiac", "y_renal", "y_dead", "y_discharge_care", "y_sepsis"
outcome_names <- c("y_serious", "y_any", "y_pneumonia", "y_SSI", "y_uti", "y_thromb", "y_readmit", "y_reop")
for (outcome in outcome_names){
  print(outcome)
  train <- mutate(pred_puf16, y_var = as.factor(grouped_outcomes_puf16[[outcome]]))
  levels(train$y_var) <- c("no_outcome", "outcome")
  plastic_train = filter(train, CPT_plastic == 1)
  
  load(paste0("./data/", outcome, "_ff.Rda"))
  plastic_train = select(plastic_train, plastic.ff$feature_list[["feature_name"]], y_var)
  
  repseeds <- function(folds = 10, from = 1e+04, seed = 123) {
    set.seed(seed)
    ## (n_repeats * nresampling) + 1
    seeds <- vector(mode = "list", length = folds + 1)
    for (i in 1:folds)
      seeds[[i]] <- sample.int(n = from, from)
    seeds[[folds + 1]] <- sample.int(n = from, 1)
    return(seeds)
  }
  
  tc <- trainControl(
    method = "cv",
    number = 10,
    summaryFunction = twoClassSummary, ## Provides ROC summary stats
    allowParallel = TRUE,
    verboseIter = FALSE,
    seeds = repseeds(), ## Reproducible seeds
    classProbs = TRUE
  )
  
  logit.time <- system.time(
    plastic.logit <-
      train(
        y_var ~ .,
        metric = "ROC",
        method = "glm",
        family = "binomial",
        trControl = tc,
        data = plastic_train
      )
  )
  save(
    list = c("logit.time", "plastic.logit"),
    file = paste0("./data/", outcome, "_logit.Rda")
  )
}
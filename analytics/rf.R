library(caret)
library(dplyr)

### Setup Data ###
load("./data/pred_puf16.Rda")
load("./data/grouped_outcomes_puf16.Rda")

train_dead <- mutate(pred_puf16, y_dead = grouped_outcomes_puf16[["y_dead"]])
train_dead2 <- train_dead[complete.cases(train_dead), ]

# We are mimicking caret package's choice of mtry grid
# while adding another choice from the Breiman randomForest package ============
mtrys <-
  c(
    floor(sqrt(ncol(pred_puf16))),
    2, 
    floor((2 + (dim(pred_puf16)[2] - 1)) / 2),
    dim(pred_puf16)[2]
  )

# Run random forest separately for each mtry and store the output ============
rf.time.cv <- system.time(
  turn.rf <- 
    train(
      y_dead ~ .,
      metric = "ROC",
      method = "rf",
      importance = T,
      proximity = F,
      ntree = 1000,
      tuneGrid = data.frame(.mtry = expand.grid(.mtry = mtrys)),
      trControl = tc,
      data = train_dead
    )
)
mtrys <- turn.rf$bestTune$mtry

save(
  list = c(
    "rf.time.cv", "mtrys", "tc", "turn.rf", "mtrys"
  ),
  file = ifelse(
    choice == "train", "./output/rfcv.Rda", "./output/rfcv_robust.Rda"
  )
)

stopCluster(mc)
print("Random forest cross-validation complete.")
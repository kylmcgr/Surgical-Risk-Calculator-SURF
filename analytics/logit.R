logit.time <- system.time(
  turn.logit <-
    train(
      as.factor(depvar) ~ .,
      metric = "ROC",
      method = "glm",
      family = "binomial",
      trControl = tc,
      data = turn_list[[choice]]
    )
)
save(
  list = c("logit.time", "turn.logit"),
  file = ifelse(
    choice == "train", 
    "./output/turn.logit.Rda", "./output/turn.logit_robust.Rda"
  )
)
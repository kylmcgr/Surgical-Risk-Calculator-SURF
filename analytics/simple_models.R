### Simple Models ###

library(aod)
library(ggplot2)
male <- data_puf16[[1]]$male
age <- data_puf16[[1]]$patient_age
white <- data_puf16[[1]]$race_white
asian <- data_puf16[[1]]$race_asian
black <- data_puf16[[1]]$race_black
nativeam <- data_puf16[[1]]$race_nativeam
aip <- data_puf16[[1]]$race_aip
dead <- data_puf16[[2]]$y_dead
y_dead <- data.frame(male, age, white, asian, black, nativeam, aip, dead)
is_dead <- glm(dead ~ male + age + asian + white + black + nativeam + aip, data = y_dead, family = "binomial")
summary(is_dead)
any <- data_puf16[[2]]$y_any
y_any <- data.frame(male, age, white, asian, black, nativeam, aip, any)
any_outcome <- glm(any ~ male + age + asian + white + black + nativeam + aip, data = y_any, family = "binomial")
summary(any_outcome)

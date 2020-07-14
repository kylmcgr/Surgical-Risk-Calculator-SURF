### Simple Models ###

library(aod)
library(ggplot2)
male <- pred_puf18$male
age <- pred_puf18$patient_age
white <- pred_puf18$race_white
asian <- pred_puf18$race_asian
black <- pred_puf18$race_black
nativeam <- pred_puf18$race_nativeam
aip <- pred_puf18$race_aip
dead <- grouped_outcomes_puf18$y_dead
y_dead <- data.frame(male, age, white, asian, black, nativeam, aip, dead)
is_dead <- glm(dead ~ male + age + asian + white + black + nativeam + aip, data = y_dead, family = "binomial")
summary(is_dead)
any <- grouped_outcomes_puf18$y_any
y_any <- data.frame(male, age, white, asian, black, nativeam, aip, any)
any_outcome <- glm(any ~ male + age + asian + white + black + nativeam + aip, data = y_any, family = "binomial")
summary(any_outcome)

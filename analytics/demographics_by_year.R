# setwd("/Users/User/Documents/NSQIP Surgical Data")
demo <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("2016", "2017", "2018")
colnames(demo) <- x

# age: 18-24, 25-35, 36-50, 51-65, 66+
new_pred16 <- mutate(pred_puf16,
                     age_18_to_24 = if_else(patient_age >= 18 & patient_age <= 24, 1, 0, missing=0),
                     age_25_to_35 = if_else(patient_age >= 25 & patient_age <= 35, 1, 0, missing=0),
                     age_36_to_50 = if_else(patient_age >= 36 & patient_age <= 50, 1, 0, missing=0),
                     age_51_to_65 = if_else(patient_age >= 51 & patient_age <= 65, 1, 0, missing=0),
                     age_66_plus = if_else(patient_age >= 66, 1, 0, missing=0),
)
new_pred17 <- mutate(pred_puf17,
                     age_18_to_24 = if_else(patient_age >= 18 & patient_age <= 24, 1, 0, missing=0),
                     age_25_to_35 = if_else(patient_age >= 25 & patient_age <= 35, 1, 0, missing=0),
                     age_36_to_50 = if_else(patient_age >= 36 & patient_age <= 50, 1, 0, missing=0),
                     age_51_to_65 = if_else(patient_age >= 51 & patient_age <= 65, 1, 0, missing=0),
                     age_66_plus = if_else(patient_age >= 66, 1, 0, missing=0),
)
new_pred18 <- mutate(pred_puf18,
                     age_18_to_24 = if_else(patient_age >= 18 & patient_age <= 24, 1, 0, missing=0),
                     age_25_to_35 = if_else(patient_age >= 25 & patient_age <= 35, 1, 0, missing=0),
                     age_36_to_50 = if_else(patient_age >= 36 & patient_age <= 50, 1, 0, missing=0),
                     age_51_to_65 = if_else(patient_age >= 51 & patient_age <= 65, 1, 0, missing=0),
                     age_66_plus = if_else(patient_age >= 66, 1, 0, missing=0),
)

total16 <- nrow(new_pred16)
total17 <- nrow(new_pred17)
total18 <- nrow(new_pred18)

demo_names <- c("male", "female", "age_18_to_24", "age_25_to_35", "age_36_to_50", "age_51_to_65", "age_66_plus", "race_asian", "race_black", "race_nativeam", "race_aip", "race_white", "race_unknown", "hispanic_y", "hispanic_n", "hispanic_u")
demo_labels <- c("Male", "% Male", "Female", "% Female", "Age 18-24", "% Age 18-24", "Age 25-35", "% Age 25-35", "Age 36-50", "% Age 36-50", "Age 51-65", "% Age 51-65", "Age 66+", "% Age 66+", "Race Asian", "% Race Asian", "Race Black", "% Race Black", "Race Native American", "% Race Native American", "Race Pacific Islander", "% Race Pacific Islander", "Race White", "% Race White", "Race Unknown", "% Race Unknown", "Hispanic", "% Hispanic", "Not Hispanic", "% Not Hispanic", "Hispanic Unknown", "% Hispanic Unknown")

for (i in demo_names){
  tble16 <- table(new_pred16[[i]])
  tble17 <- table(new_pred17[[i]])
  tble18 <- table(new_pred18[[i]])
  demo <- add_row(demo, "2016" = tble16[2], "2017" = tble17[2], "2018" = tble18[2])
  demo <- add_row(demo, "2016" = tble16[2]/total16*100, "2017" = tble17[2]/total17*100, "2018" = tble18[2]/total18*100)
}

row.names(demo) <- demo_labels

print(xtable(demo, caption = "Demographics by Year", type = "latex"), file = paste("/Users/User/Documents/NSQIP Surgical Data/demographics.tex", sep = ""))
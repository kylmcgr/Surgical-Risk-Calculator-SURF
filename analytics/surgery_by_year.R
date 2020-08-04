# This script creates a table of the number of a surgery type for the NSQIP 2016, 2017, and 2018 datasets
# data_processing_puf16.R, data_processing_puf17.R, and data_processing_puf18.R must be run prior
# to running the script for the pred_puf16, pred_puf17, and pred_puf18 objects.
# This file generates latex code for a table of the number of a surgery type: surgery.tex
# Kyle McGraw, July 2019


### Import Data ###
load("./data/pred_puf16.csv")
load("./data/pred_puf17.csv")
load("./data/pred_puf18.csv")


#### Demographics by Year ####

# Creates an empty data frame with years as columns
surg <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("2016", "2017", "2018")
colnames(surg) <- x

# Adds a category to look for overlap in surgery categories
surg_pred16 <- mutate(pred_puf16,
                     plastics_both = if_else(CPT_plastic == 1 & surgery_plastic == 1, 1, 0, missing=0),
)
surg_pred17 <- mutate(pred_puf17,
                     plastics_both = if_else(CPT_plastic == 1 & surgery_plastic == 1, 1, 0, missing=0),
)
surg_pred18 <- mutate(pred_puf18,
                     plastics_both = if_else(CPT_plastic == 1 & surgery_plastic == 1, 1, 0, missing=0),
)

# Generates total observations for each year for percentages
total16 <- nrow(surg_pred16)
total17 <- nrow(surg_pred17)
total18 <- nrow(surg_pred18)

# List of variable names to be used and labels for each variable in the table
surg_names <- c("CPT_plastic", "surgery_plastic", "plastics_both")
surg_labels <- c("CPT Plastic", "Surgery Speciality Plastic", "Both CPT and Surgery Speciality Plastic")

# Adds each variable as a number and as a percentage
for (i in surg_names){
  tble16 <- table(surg_pred16[[i]])
  tble17 <- table(surg_pred17[[i]])
  tble18 <- table(surg_pred18[[i]])
  surg <- add_row(surg, "2016" = tble16[2], "2017" = tble17[2], "2018" = tble18[2])
  # surg <- add_row(surg, "2016" = tble16[2]/total16*100, "2017" = tble17[2]/total17*100, "2018" = tble18[2]/total18*100)
}

# Adds row names
row.names(surg) <- surg_labels

# Export latex code to file
print(xtable(surg, caption = "Plastic Surgery", type = "latex"), file = paste0("./tables/", "surgery.tex"))

# test out cross tab
library(gmodels)
print(summary(CrossTable(new_pred16[["CPT_plastic"]], new_pred16[["surgery_plastic"]]), latex=TRUE), file = paste0("./tables/", "surgery16.tex"))
print(summary(CrossTable(new_pred17[["CPT_plastic"]], new_pred17[["surgery_plastic"]]), latex=TRUE), file = paste0("./tables/", "surgery17.tex"))
print(summary(CrossTable(new_pred18[["CPT_plastic"]], new_pred18[["surgery_plastic"]]), latex=TRUE), file = paste0("./tables/", "surgery18.tex"))

rm(list=ls())
gc()

library(dplyr)
library(data.table)

# 2016
setwd("/Users/User/OneDrive - California Institute of Technology/Caltech/Frosh/SURF/Surgery SURF/Surgical Risk Calculator SURF")
source("data-processing/data_processing_puf16.R")
setwd("/Users/User/OneDrive - California Institute of Technology/Caltech/Frosh/SURF/Surgery SURF/Surgical Risk Calculator SURF")
source("analytics/data_quality_testing_puf16.R")
source("data-processing/outcome_grouping_puf16.R")

# 2017
setwd("/Users/User/OneDrive - California Institute of Technology/Caltech/Frosh/SURF/Surgery SURF/Surgical Risk Calculator SURF")
source("data-processing/data_processing_puf17.R")
setwd("/Users/User/OneDrive - California Institute of Technology/Caltech/Frosh/SURF/Surgery SURF/Surgical Risk Calculator SURF")
source("analytics/data_quality_testing_puf17.R")
source("data-processing/outcome_grouping_puf17.R")

# 2018
setwd("/Users/User/OneDrive - California Institute of Technology/Caltech/Frosh/SURF/Surgery SURF/Surgical Risk Calculator SURF")
source("data-processing/data_processing_puf18.R")
setwd("/Users/User/OneDrive - California Institute of Technology/Caltech/Frosh/SURF/Surgery SURF/Surgical Risk Calculator SURF")
source("analytics/data_quality_testing_puf18.R")
source("data-processing/outcome_grouping_puf18.R")
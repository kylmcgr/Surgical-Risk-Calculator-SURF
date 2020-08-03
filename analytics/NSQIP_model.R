# This script attempts to replicate the NSQIP model using the 2018 dataset
# data_processing_puf18.R and outcome_grouping_puf18.R must be run prior to 
# running the script for the pred_puf18 and grouped_outcomes_puf18 objects.
# Kyle McGraw, July 2019



### Setup Data ###
pred_puf18 <- read.csv('pred_puf18.csv')
grouped_outcomes_puf18 <- read.csv('grouped_outcomes_puf18.csv')

# Changes age and BMI to categories
NSQIP_pred18 <- mutate(pred_puf18,
                     age_under_65 = if_else(patient_age < 65, 1, 0, missing=0),
                     age_65_to_74 = if_else(patient_age >= 65 & patient_age <= 74, 1, 0, missing=0),
                     age_75_to_84 = if_else(patient_age >= 75 & patient_age <= 84, 1, 0, missing=0),
                     age_85_plus = if_else(patient_age >= 85, 1, 0, missing=0),
                     
                     BMI_underweight = if_else(BMI < 18.5, 1, 0, missing=0),
                     BMI_normal = if_else(BMI >= 18.5 & BMI < 25, 1, 0, missing=0),
                     BMI_overweight = if_else(BMI >= 25 & BMI < 30, 1, 0, missing=0),
                     BMI_obese_1 = if_else(BMI >= 30 & BMI < 35, 1, 0, missing=0),
                     BMI_obese_2 = if_else(BMI >= 35 & BMI < 40, 1, 0, missing=0),
                     BMI_obese_3 = if_else(BMI >= 40, 1, 0, missing=0),
)

# Selects predictors
NSQIP_frame <- select(NSQIP_pred18, female, male, cpt, age_under_65, age_65_to_74, age_75_to_84, age_85_plus, BMI_underweight, BMI_normal, 
                      BMI_overweight, BMI_obese_1, BMI_obese_2, BMI_obese_3, diabetes_no, diabetes_insulin, diabetes_noninsulin, 
                      smoke_yes, smoke_no, dyspnea_rest, dyspnea_moderate, dyspnea_no, functional_hs_independent, functional_hs_partially, 
                      functional_hs_dependent, functional_hs_unknown, ventilator_dependent, ventilator_independent, history_COPD, 
                      history_noCOPD, ascites_n, ascites_y, CHF_y, CHF_n, Hyper_med_y, Hyper_med_n, Renal_fail_y, Renal_fail_n, 
                      Dialysis_y, Dialysis_n, Diss_cancer_y, Diss_cancer_n, Chronic_steroid_y, Chronic_steroid_n, Sepsis_none, 
                      Sepsis_sepsis, Sepsis_shock, Sepsis_sirs, Emergency_y, Emergency_n, ASA_no, ASA_mild, ASA_severe, ASA_life, 
                      ASA_moribund, ASA_none)

# Creates new data to be passed into the model
newdata <- data.frame(female = c(1, rep(0, times = 35)), age = c(rep(0, times = 1), 1, 2, 3, rep(0, times = 32)), 
                     BMI = c(rep(0, times = 4), 1, 2, 3, 4, 5, rep(0, times = 27)), diabetes = c(rep(0, times = 9), 1, 2, rep(0, times = 25)), 
                     smoke = c(rep(0, times = 11), 1, rep(0, times = 24)), dyspnea = c(rep(0, times = 12), 1, 2, rep(0, times = 22)),
                     functional_hs = c(rep(0, times = 14), 1, 2, 3, rep(0, times = 19)), ventilator = c(rep(0, times = 17), 1, rep(0, times = 18)),
                     COPD = c(rep(0, times = 18), 1, rep(0, times = 17)), ascites = c(rep(0, times = 19), 1, rep(0, times = 16)), 
                     CHF = c(rep(0, times = 20), 1, rep(0, times = 15)), Hyper_med = c(rep(0, times = 21), 1, rep(0, times = 14)), 
                     Renal_fail = c(rep(0, times = 22), 1, rep(0, times = 13)), Dialysis = c(rep(0, times = 23), 1, rep(0, times = 12)), 
                     Diss_cancer = c(rep(0, times = 24), 1, rep(0, times = 11)), Chronic_steroid = c(rep(0, times = 25), 1, rep(0, times = 10)), 
                     Sepsis = c(rep(0, times = 26), 1, 2, 3, rep(0, times = 7)), Emergency = c(rep(0, times = 29), 1, rep(0, times = 6)), 
                     ASA = c(rep(0, times = 30), 1, 2, 3, 4, 5, rep(0, times = 1)))
testdata <- transmute(newdata,
                     female,#1
                     age_65_to_74 = if_else(age == 1, 1, 0),
                     age_75_to_84 = if_else(age == 2, 1, 0),
                     age_85_plus = if_else(age == 3, 1, 0),
                     BMI_underweight = if_else(BMI == 1, 1, 0),#5
                     BMI_overweight = if_else(BMI == 2, 1, 0),
                     BMI_obese_1 = if_else(BMI == 3, 1, 0),
                     BMI_obese_2 = if_else(BMI == 4, 1, 0),
                     BMI_obese_3 = if_else(BMI == 5, 1, 0),
                     diabetes_insulin = if_else(diabetes == 1, 1, 0),#10
                     diabetes_noninsulin = if_else(diabetes == 2, 1, 0),
                     smoke_yes = smoke,
                     dyspnea_rest = if_else(dyspnea == 1, 1, 0),
                     dyspnea_moderate = if_else(dyspnea == 2, 1, 0),
                     functional_hs_partially = if_else(functional_hs == 1, 1, 0),#15
                     functional_hs_dependent = if_else(functional_hs == 2, 1, 0),
                     functional_hs_unknown = if_else(functional_hs == 3, 1, 0),
                     ventilator_dependent = ventilator,
                     history_COPD = COPD,
                     ascites_y = ascites,#20
                     CHF_y = CHF,
                     Hyper_med_y = Hyper_med,
                     Renal_fail_y = Renal_fail,
                     Dialysis_y = Dialysis,
                     Diss_cancer_y = Diss_cancer,#25
                     Chronic_steroid_y = Chronic_steroid,
                     Sepsis_sepsis = if_else(Sepsis == 1, 1, 0),
                     Sepsis_shock = if_else(Sepsis == 2, 1, 0),
                     Sepsis_sirs = if_else(Sepsis == 3, 1, 0),
                     Emergency_y = Emergency,#30
                     ASA_mild = if_else(ASA == 1, 1, 0),
                     ASA_severe = if_else(ASA == 2, 1, 0),
                     ASA_life = if_else(ASA == 3, 1, 0),
                     ASA_moribund = if_else(ASA == 4, 1, 0),
                     ASA_none = if_else(ASA == 5, 1, 0),#35
)


#### Models ####

# Creates models for all outcomes and saves the predictions on test data
outcome_names <- c("y_discharge", "y_dead", "y_reop", "y_related_reop", "y_readmit", "y_related_readmit", "y_sup_ssi", "y_deep_ssi", "y_organ_ssi", "y_wound_disruption", "y_pneumonia", "y_unplanned_intubation", "y_emb", "y_vent", "y_PRF", "y_ARF", "y_uti", "y_stroke", "y_cpr", "y_mi", "y_trans", "y_thromb", "y_sepsis", "y_sepshock", "y_cdiff")
for (i in outcome_names){
  y_var <- grouped_outcomes_puf18[[i]]
  
  y_frame <- mutate(NSQIP_frame, y_var)
  
  NSQIP_var <- glm(y_var ~ female + age_65_to_74 + age_75_to_84 + age_85_plus + BMI_underweight +
                      BMI_overweight + BMI_obese_1 + BMI_obese_2 + BMI_obese_3 + diabetes_insulin + diabetes_noninsulin + 
                      smoke_yes + dyspnea_rest + dyspnea_moderate + functional_hs_partially + functional_hs_dependent + 
                      functional_hs_unknown + ventilator_dependent  + history_COPD + ascites_y + CHF_y + Hyper_med_y + 
                      Renal_fail_y + Dialysis_y + Diss_cancer_y + Chronic_steroid_y + Sepsis_sepsis + Sepsis_shock + 
                      Sepsis_sirs + Emergency_y + ASA_mild + ASA_severe + ASA_life + ASA_moribund + ASA_none,
                   data = y_frame, family = "binomial")
  print(i)
  print(summary(NSQIP_var))

  NSQIP_var <- predict(NSQIP_var, testdata, type="response")
  pdf(paste0(i, ".pdf"))
  plot(NSQIP_var)
  dev.off()
}





# y_dead <- grouped_outcomes_puf18$y_dead
# 
# dead <- mutate(NSQIP_frame, y_dead)
# 
# NSQIP_dead <- glm(y_dead ~ female + age_65_to_74 + age_75_to_84 + age_85_plus + BMI_underweight +
#                      BMI_overweight + BMI_obese_1 + BMI_obese_2 + BMI_obese_3 + diabetes_insulin + diabetes_noninsulin +
#                      smoke_yes + dyspnea_rest + dyspnea_moderate + functional_hs_partially + functional_hs_dependent +
#                      functional_hs_unknown + ventilator_dependent  + history_COPD + ascites_y + CHF_y + Hyper_med_y +
#                      Renal_fail_y + Dialysis_y + Diss_cancer_y + Chronic_steroid_y + Sepsis_sepsis + Sepsis_shock +
#                      Sepsis_sirs + Emergency_y + ASA_mild + ASA_severe + ASA_life + ASA_moribund + ASA_none,
#                    data = dead, family = "binomial")
# summary(NSQIP_dead)
# 
# NSQIP_predict = predict(NSQIP_dead, testdata, type="response")


# gender_age_logit <- glm(dead ~ female + age_65_to_74 + age_75_to_84 + age_85_plus, data = NSQIP_dead, family = "binomial")
# summary(gender_age_logit)

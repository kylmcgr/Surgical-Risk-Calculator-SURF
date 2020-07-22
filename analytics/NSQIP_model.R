


### Import Data ###
pred_puf18 <- read.csv('pred_puf18.csv')
grouped_outcomes_puf18 <- read.csv('grouped_outcomes_puf18.csv')

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

male <- pred_puf18$male
age <- pred_puf18$patient_age
white <- pred_puf18$race_white
asian <- pred_puf18$race_asian
black <- pred_puf18$race_black
nativeam <- pred_puf18$race_nativeam
aip <- pred_puf18$race_aip

cpt <- pred_puf18$cpt

dead <- grouped_outcomes_puf18$y_dead
any <- grouped_outcomes_puf18$y_any


NSQIP_frame <- data.frame(male, age, white, asian, black, nativeam, aip, cpt, dead)

# is_dead <- glm(dead ~ male + age + asian + white + black + nativeam + aip, data = y_dead, family = "binomial")


# NSQIP_model1 <- glmer(dead ~ male + age + asian + white + black + nativeam + aip +
#         (1 | cpt), data = NSQIP_frame, family = binomial, control = glmerControl(optimizer = "bobyqa"),
#       nAGQ = 10)
# 
# 
# NSQIP_model2 <- glmer(dead ~ male + age + asian + white + black + nativeam + aip +
#                        (1 | cpt), data = NSQIP_frame, family = binomial)
# 
# summary(NSQIP_model2)


NSQIP_frame <- select(NSQIP_pred18, female, male, cpt, age_under_65, age_65_to_74, age_75_to_84, age_85_plus, BMI_underweight, BMI_normal, 
                      BMI_overweight, BMI_obese_1, BMI_obese_2, BMI_obese_3, diabetes_no, diabetes_insulin, diabetes_noninsulin, 
                      smoke_yes, smoke_no, dyspnea_rest, dyspnea_moderate, dyspnea_no, functional_hs_independent, functional_hs_partially, 
                      functional_hs_dependent, functional_hs_unknown, ventilator_dependent, ventilator_independent, history_COPD, 
                      history_noCOPD, ascites_n, ascites_y, CHF_y, CHF_n, Hyper_med_y, Hyper_med_n, Renal_fail_y, Renal_fail_n, 
                      Dialysis_y, Dialysis_n, Diss_cancer_y, Diss_cancer_n, Chronic_steroid_y, Chronic_steroid_n, Sepsis_none, 
                      Sepsis_sepsis, Sepsis_shock, Sepsis_sirs, Emergency_y, Emergency_n, ASA_no, ASA_mild, ASA_severe, ASA_life, 
                      ASA_moribund, ASA_none)

NSQIP_dead <- mutate(NSQIP_frame, dead)

NSQIP_logit <- glm(dead ~ female + age_65_to_74 + age_75_to_84 + age_85_plus + BMI_underweight +
                     BMI_overweight + BMI_obese_1 + BMI_obese_2 + BMI_obese_3 + diabetes_insulin + diabetes_noninsulin + 
                     smoke_yes + dyspnea_rest + dyspnea_moderate + functional_hs_partially + functional_hs_dependent + 
                     functional_hs_unknown + ventilator_dependent  + history_COPD + ascites_y + CHF_y + Hyper_med_y + 
                     Renal_fail_y + Dialysis_y + Diss_cancer_y + Chronic_steroid_y +Sepsis_sepsis + Sepsis_shock + 
                     Sepsis_sirs + Emergency_y + ASA_mild + ASA_severe + ASA_life + ASA_moribund + ASA_none, 
                   data = NSQIP_dead, family = "binomial")
summary(NSQIP_logit)

gender_age_logit <- glm(dead ~ female + age_65_to_74 + age_75_to_84 + age_85_plus, data = NSQIP_dead, family = "binomial")
summary(gender_age_logit)


# predict(object, newdata = NULL)



### Import Data ###
pred_puf18 <- read.csv('pred_puf18.csv')
grouped_outcomes_puf18 <- read.csv('grouped_outcomes_puf18.csv')


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


NSQIP_model2 <- glmer(dead ~ male + age + asian + white + black + nativeam + aip +
                       (1 | cpt), data = NSQIP_frame, family = binomial)

summary(NSQIP_model2)


NSQIP_frame <- select(pred_puf18, female, male, cpt, patient_age, BMI, diabetes_no, diabetes_insulin, diabetes_noninsulin, 
                      smoke_yes, smoke_no, dyspnea_rest, dyspnea_moderate, dyspnea_no, functional_hs_independent, functional_hs_partially, 
                      functional_hs_dependent, functional_hs_unknown, ventilator_dependent, ventilator_independent, history_COPD, 
                      history_noCOPD, ascites_n, ascites_y, CHF_y, CHF_n, Hyper_med_y, Hyper_med_n, Renal_fail_y, Renal_fail_n, 
                      Dialysis_y, Dialysis_n, Diss_cancer_y, Diss_cancer_n, Chronic_steroid_y, Chronic_steroid_n, Sepsis_none, 
                      Sepsis_sepsis, Sepsis_shock, Sepsis_sirs, Emergency_y, Emergency_n, ASA_no, ASA_mild, ASA_severe, ASA_life, 
                      ASA_moribund, ASA_none)

NSQIP_dead <- mutate(NSQIP_frame, dead)

is_dead <- glm(dead ~ female + male + patient_age + BMI + diabetes_no + diabetes_insulin + diabetes_noninsulin + 
               smoke_yes + smoke_no + dyspnea_rest + dyspnea_moderate + dyspnea_no + functional_hs_independent + functional_hs_partially + 
               functional_hs_dependent + functional_hs_unknown + ventilator_dependent + ventilator_independent + history_COPD + 
               history_noCOPD + ascites_n + ascites_y + CHF_y + CHF_n + Hyper_med_y + Hyper_med_n + Renal_fail_y + Renal_fail_n + 
               Dialysis_y + Dialysis_n + Diss_cancer_y + Diss_cancer_n + Chronic_steroid_y + Chronic_steroid_n + Sepsis_none + 
               Sepsis_sepsis + Sepsis_shock + Sepsis_sirs + Emergency_y + Emergency_n + ASA_no + ASA_mild + ASA_severe + ASA_life + 
               ASA_moribund + ASA_none, data = NSQIP_dead, family = "binomial")
summary(is_dead)


# predict(object, newdata = NULL)
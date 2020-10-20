## ----SNAP2_Clinician_Prediction_setup, include=FALSE---------------------
knitr::opts_chunk$set(echo = TRUE, dpi = 300)

#Load libraries and data
library(tidyverse)
library(knitr)
library(tableone)
library(pROC)
library(rms)
library(caret)
library(PredictABEL)
library(pander)
library(DiagrammeR) #Needs ver. 0.9.2 to work
library(DiagrammeRsvg)
library(cowplot)
library(nricens)
library(rmda)
panderOptions('table.split.table', Inf)

patients_clean <- read.csv("data/SNAP2_Clinician_Predictions_Anon_Dataset.csv", stringsAsFactors = FALSE)

#Create an outcome variable (mortality at 30 days)
patients_clean <- patients_clean %>% 
  mutate(S07StillInHospitalPrimaryAdmissionAfterSurgery = replace(S07StillInHospitalPrimaryAdmissionAfterSurgery,
                                                                  which(is.na(S07StillInHospitalPrimaryAdmissionAfterSurgery) &
                                                                          S05PatientStillAliveAndInHospital == "N"), "N")) %>%
  mutate(S07StatusAtDischarge = replace(S07StatusAtDischarge, which(is.na(S07StatusAtDischarge) & S05StatusAtDischarge == "A"), "A")) %>%
  mutate(S07StatusAtDischarge = replace(S07StatusAtDischarge, which(is.na(S07StatusAtDischarge) & S05StatusAtDischarge == "D"), "D")) %>%
  mutate(mort30 = (S07PostopLOS <= 30 & S07StatusAtDischarge == "D")) %>%
  mutate(mort30 = replace(mort30, which(is.na(mort30) & S07StillInHospitalPrimaryAdmissionAfterSurgery == "Y"), FALSE))

#Create a variable to indicate if only clinician assessment was used to predict risk
patients_clean <- patients_clean %>% 
  mutate(S01Age = as.numeric(S01Age)) %>%
  mutate(SORT_mort_risk = arm::invlogit(SORT_mort),
         pPOSSUM_mort_risk = arm::invlogit(pPOSSUM_mort),
         SRS_mort_risk = arm::invlogit(SRS_mort))

#Create a variable for clinical_only to indicate that clinical judgement alone or ASA alone was used
patients_clean <- patients_clean %>%
  mutate(clinical_only = ((S03MortalityEstimateClinicalJudgment == TRUE |
                             S03MortalityEstimateASAPSScore == TRUE) &
                            S03MortalityEstimateDukeOtherActivityStatusIndex == FALSE &
                            S03MortalityEstimateWalkTest == FALSE &
                            S03MortalityEstimateCardiopulmonaryExerciseTesting == FALSE &
                            S03MortalityEstimateFrailtyAssessment == FALSE &
                            S03MortalityEstimateSurgicalRiskScale == FALSE &
                            S03MortalityEstimateSurgicalOutcomeRiskTool == FALSE &
                            S03MortalityEstimateEuroSCORE == FALSE &
                            S03MortalityEstimatePOSSUM == FALSE &
                            S03MortalityEstimatePPOSSUM == FALSE &
                            S03MortalityEstimateSurgeryPPOSSUM == FALSE &
                            S03MortalityEstimateOther == FALSE) &
           !is.na(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days)) %>%
  #Create a variable for specialty which matches those in the Cancellations paper
  left_join(select(read.csv("data/SNAP2_procedurelist_specialty_coded.csv"), 
                   Code, specialty = Specialty), 
            by = c("S02PlannedProcedure" = "Code")) %>%
  mutate(specialty_recoded = recode(specialty, 
                                    `Colorectal` = "GI", 
                                    `UpperGI` = "GI", 
                                    `Bariatric` = "GI",
                                    `HPB` = "GI",
                                    `Spine` = "Neuro-Spine",
                                    `Neuro` = "Neuro-Spine",
                                    `Urology` = "Gynaecology-Urology",
                                    `Gynae` = "Gynaecology-Urology",
                                    `Transplant` = "Other", 
                                    `Endoscopic` = "Other", 
                                    `IR` = "Other", 
                                    `Cardiology` = "Other", 
                                    `Opthalmic` = "Other",
                                    `Plastics` = "Other",
                                    `MaxFax-Dental` = "Other", 
                                    `ENT` = "Other", 
                                    `Endocrine` = "Other", 
                                    `Breast` = "Other")) %>%
  mutate(S07PostopLOS = as.integer(S07PostopLOS)) 

#Need to establish the number of cases excluded because of missing data
#First these are all the variables that go into pPOSSUM, SORT and SRS.
#Remove Obstetric patients as per reviewers' requests
#To revert to original patient cohort including obstetric patients (as in sensitivity analysis)
#Quote out the following 2 lines.
patients_complete <- patients_clean %>% 
  filter(specialty_recoded != "Obs")

strobe_table <- data.frame(Description = "Total patients", 
                           N = nrow(patients_clean)) %>%
  rbind(data.frame(Description = "Excluded Obstetric patients",
                   N = nrow(patients_complete)))

patients_complete <- patients_complete %>% 
  filter(!(is.na(S01Age) |
             is.na(S03AsaPsClass) |
             is.na(S02OperativeUrgency) | 
             is.na(S02PlannedProcSeverity) |
             is.na(S04Malignancy) |
             is.na(S03PastMedicalHistoryMetastaticCancerCurrent) |
             is.na(S02PlannedProcedure)))

strobe_table <- strobe_table %>%
  rbind(data.frame(Description = "Missing predictor variable data",
                   N = nrow(patients_complete)))

patients_complete <- patients_complete %>% 
  filter(!(is.na(mort30)))

strobe_table <- rbind(strobe_table, 
                      data.frame(Description = "Missing mortality outcome data",
                                 N = nrow(patients_complete)))


## ----SNAP2_Clinician_Prediction_table1-----------------------------------
#Table 1
table1a <- CreateTableOne(vars = c("S01Gender",
                                   "S01Age",
                                   "S02OperativeUrgency",
                                   "S03AsaPsClass",
                                   "S02PlannedProcSeverity",
                                   "specialty_recoded",
                                   "S03PastMedicalHistoryCoronaryArteryDisease",
                                   "S03PastMedicalHistoryCongestiveCardiacFailure",
                                   "S03PastMedicalHistoryMetastaticCancerCurrent",
                                   "S03PastMedicalHistoryDementia",
                                   "S03PastMedicalHistoryCOPD",
                                   "S03PastMedicalHistoryPulmonaryFibrosis",
                                   "S03Diabetes",
                                   "S03PastMedicalHistoryLiverCirrhosis",
                                   "S03PastMedicalHistoryRenalDisease",
                                   "S07PostopLOS",
                                   "SORT_mort_risk",
                                   "pPOSSUM_mort_risk",
                                   "SRS_mort_risk",
                                   "clinical_only"),
                          strata = "mort30",
                          data = patients_complete)
table1b <- CreateTableOne(vars = c("S01Gender",
                                   "S01Age",
                                   "S02OperativeUrgency",
                                   "S03AsaPsClass",
                                   "S02PlannedProcSeverity",
                                   "specialty_recoded",
                                   "S03PastMedicalHistoryCoronaryArteryDisease",
                                   "S03PastMedicalHistoryCongestiveCardiacFailure",
                                   "S03PastMedicalHistoryMetastaticCancerCurrent",
                                   "S03PastMedicalHistoryDementia",
                                   "S03PastMedicalHistoryCOPD",
                                   "S03PastMedicalHistoryPulmonaryFibrosis",
                                   "S03Diabetes",
                                   "S03PastMedicalHistoryLiverCirrhosis",
                                   "S03PastMedicalHistoryRenalDisease",
                                   "S07PostopLOS",
                                   "SORT_mort_risk",
                                   "pPOSSUM_mort_risk",
                                   "SRS_mort_risk",
                                   "clinical_only"),
                          data = patients_complete)
skewed_var <- c("S01Age", "S07PostopLOS", "SORT_mort_risk", "pPOSSUM_mort_risk", "SRS_mort_risk")

cbind(print(table1b, nonnormal = skewed_var, printToggle = FALSE), 
      print(table1a, nonnormal = skewed_var, printToggle = FALSE)) %>%
  pander("Patient demographics stratified by 30d mortality.")


## ----SNAP2_Clinician_Prediction_ext_validation---------------------------
#Calibration and discrimination for the whole patient dataset
#SORT
val.prob(y = patients_complete$mort30, p = arm::invlogit(patients_complete$SORT_mort),
         xlab = "Predicted Probability (SORT)")
SORT_roc <- roc(response = patients_complete$mort30, 
                predictor = arm::invlogit(patients_complete$SORT_mort))
patients_temp <- patients_complete %>% select(mort30, SORT_mort) %>% drop_na()
generalhoslem::logitgof(obs = patients_complete$mort30, 
                        exp = arm::invlogit(patients_complete$SORT_mort))

#PPOSSUM
val.prob(y = patients_complete$mort30, p = arm::invlogit(patients_complete$pPOSSUM_mort),
         xlab = "Predicted Probability (pPOSSUM)")
pPOSSUM_roc <- roc(response = patients_complete$mort30, 
                   predictor = arm::invlogit(patients_complete$pPOSSUM_mort))
patients_temp <- patients_complete %>% select(mort30, pPOSSUM_mort) %>% drop_na()
generalhoslem::logitgof(obs = patients_temp$mort30, 
                        exp = arm::invlogit(patients_temp$pPOSSUM_mort))

#SRS
val.prob(y = patients_complete$mort30, p = arm::invlogit(patients_complete$SRS_mort),
         xlab = "Predicted Probability (SRS)")
SRS_roc <- roc(response = patients_complete$mort30, 
               predictor = arm::invlogit(patients_complete$SRS_mort))
patients_temp <- patients_complete %>% select(mort30, SRS_mort) %>% drop_na()
generalhoslem::logitgof(obs = patients_temp$mort30, 
                        exp = arm::invlogit(patients_temp$SRS_mort))

#ASA
ASA_roc <- roc(response = patients_complete$mort30, 
               predictor = factor(patients_complete$S03AsaPsClass, 
                                  levels = c("I", "II", "III", "Iv", "V"), 
                                  ordered = TRUE))


## ----SNAP2_Clinician_Prediction_misc_clean1------------------------------
patients_complete %>% select(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days:S03MortalityEstimateOtherDetails) %>%
  mutate_all(funs(factor(.))) %>% names()


## ----SNAP2_Clinician_Prediction_misc_clean2------------------------------
patients_complete <- patients_complete %>% mutate(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days = recode_factor(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days,
                       `L1` = "<1%",
                       `L2` = "1-2.5%",
                       `L6` = "2.6-5%",
                       `L10` = "5.1-10%",
                       `L50` = "10.1-50%", 
                       `G50` = ">50%",
                       .ordered = TRUE))
plot(patients_complete$S03PerioperativeTeamOfTheRiskOfDeathWithin30Days)

#patients_complete %>% select(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days:S03MortalityEstimateOtherDetails) %>% DescTools::Desc(.)


## ----SNAP2_Clinician_Prediction_misc_clean3------------------------------
patients_complete %>% filter(clinical_only == TRUE) -> clinical_only
nrow(clinical_only)
plot(clinical_only$S03PerioperativeTeamOfTheRiskOfDeathWithin30Days)


## ----SNAP2_Clinician_Prediction_clin_calib_disc--------------------------
#Tabulate the outcomes by the risk predicted by clinicians
clinical_only %>% group_by(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, .drop = TRUE) %>% 
  summarise(deaths = sum(mort30 == TRUE, na.rm = TRUE), 
            survived = sum(mort30 == FALSE, na.rm = TRUE), 
            mortality_prob = sum(mort30 == TRUE, na.rm = TRUE)/(sum(mort30 == TRUE, na.rm = TRUE) + sum(mort30 == FALSE, na.rm = TRUE)), 
            ci.lower = binconf(sum(mort30 == TRUE, na.rm = TRUE), (sum(mort30 == TRUE, na.rm = TRUE) + sum(mort30 == FALSE, na.rm = TRUE)))[2], 
            ci.upper = binconf(sum(mort30 == TRUE, na.rm = TRUE), (sum(mort30 == TRUE, na.rm = TRUE) + sum(mort30 == FALSE, na.rm = TRUE)))[3])

#Plot a calibration curve
table(clinical_only$mort30, clinical_only$S03PerioperativeTeamOfTheRiskOfDeathWithin30Days) %>% 
  prop.table(., 2) -> temp

tempDF <- data.frame(actual = c(temp[2,], temp[2,length(temp[2,])]), predicted = c(0, 0.01, 0.025, 0.05, 0.1, 0.5, 1))
plot(actual ~ predicted, tempDF, type="n", ylim = c(0, 1), xlim = c(0, 1))
lines(x = c(0,1), y = c(0,1), col = "grey")
lines(actual ~ predicted, tempDF, type= "s")

#Different way of plotting
tempDF <- clinical_only %>% group_by(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, .drop = TRUE) %>% 
  summarise(deaths = sum(mort30 == TRUE, na.rm = TRUE), 
            survived = sum(mort30 == FALSE, na.rm = TRUE), 
            mortality_prob = sum(mort30 == TRUE, na.rm = TRUE)/(sum(mort30 == TRUE, na.rm = TRUE) + sum(mort30 == FALSE, na.rm = TRUE)), 
            ci.lower = binconf(sum(mort30 == TRUE, na.rm = TRUE), (sum(mort30 == TRUE, na.rm = TRUE) + sum(mort30 == FALSE, na.rm = TRUE)))[2], 
            ci.upper = binconf(sum(mort30 == TRUE, na.rm = TRUE), (sum(mort30 == TRUE, na.rm = TRUE) + sum(mort30 == FALSE, na.rm = TRUE)))[3]) %>%
 mutate(predicted = c(0.005, 0.0175, 0.0375, 0.075, 0.3, 0.75))
with(tempDF,  errbar(x = predicted, 
                     y = mortality_prob, 
                     yminus = ci.lower, 
                     yplus = ci.upper,
                     ylim = c(0, 1), 
                     xlim = c(0, 1),
                     xlab = "predicted",
                     ylab = "actual"))
lines(x = c(0,1), y = c(0,1), col = "grey")
lines(lowess(mortality_prob ~ predicted, tempDF, iter = 0), lty = 2)

#Alternative depiction using a stacked barchart
ggplot(as.data.frame(temp), aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_col() +
  labs(title = "Calibration of predictions",
       fill = "Mortality",
       x = "Predicted Risk",
       y = "Probability")

#plot(temp[2,])

#Plot an ROC curve
#plot(roc(response = clinical_only$mort30, predictor = clinical_only$S03PerioperativeTeamOfTheRiskOfDeathWithin30Days), main = "Clinical judgement (Raw)")

#Create a logit model with single predictor (this step is in effect a recalibration of the clinicians' predictions to the data)
logit_clinical <- glm(mort30 ~ S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, data = clinical_only, family = binomial)
summary(logit_clinical)

val.prob(y = clinical_only$mort30, p = predict(logit_clinical, type = "response", newdata = clinical_only))
#roc_logit_clinical <- roc(response = clinical_only$mort30, predictor = predict(logit_clinical, type = "response", newdata = clinical_only))
#plot(roc_logit_clinical, main = "Clinical judgement (logit)")

#Following discussion at the supervision meeting on 13/08/18, consider whether should dichotomise predictions into a 5% cutoff
clinical_only$clinpred_bin <- ifelse(clinical_only$S03PerioperativeTeamOfTheRiskOfDeathWithin30Days %in% c("<1%", "1-2.5%", "2.6-5%"), "<=5%", ifelse(clinical_only$S03PerioperativeTeamOfTheRiskOfDeathWithin30Days %in% c("5.1-10%", "10.1-50%", ">50%"), ">5%", NA))

xtabs(~ clinpred_bin + mort30, data = clinical_only)

pred <- ifelse(clinical_only$clinpred_bin == "<=5%", 0, ifelse(clinical_only$clinpred_bin == ">5%", 1, NA)) %>% as.factor()
truth <- ifelse(clinical_only$mort30 == FALSE, 0, ifelse(clinical_only$mort30 == TRUE, 1, NA)) %>% as.factor()
caret::confusionMatrix(pred, truth, positive = "1")


## ----SNAP2_Clinician_Prediction_clin_model_validate----------------------
#SORT
val.prob(y = clinical_only$mort30, p = arm::invlogit(clinical_only$SORT_mort), 
         xlab = "Predicted Probability (SORT)")

## ----SNAP2_Clinician_Prediction_clin_SORT_NRI----------------------------
#Baseline model with SORT variables

#Write a function to calculate continuous NRI based on the function from PredictABEL
reclassification2 <- function (data, cOutcome, predrisk1, predrisk2, cutoff) 
{
  c1 <- cut(predrisk1, breaks = cutoff, include.lowest = TRUE, 
            right = FALSE)
  c2 <- cut(predrisk2, breaks = cutoff, include.lowest = TRUE, 
            right = FALSE)
  tabReclas <- table(`Initial Model` = c1, `Updated Model` = c2)
  cat(" _________________________________________\n")
  cat(" \n     Reclassification table    \n")
  cat(" _________________________________________\n")
  ta <- table(c1, c2, data[, cOutcome])
  cat("\n Outcome: absent \n  \n")
  TabAbs <- ta[, , 1]
  tab1 <- cbind(TabAbs, ` % reclassified` = round((rowSums(TabAbs) - 
                                                     diag(TabAbs))/rowSums(TabAbs), 2) * 100)
  names(dimnames(tab1)) <- c("Initial Model", "Updated Model")
  print(tab1)
  cat("\n \n Outcome: present \n  \n")
  TabPre <- ta[, , 2]
  tab2 <- cbind(TabPre, ` % reclassified` = round((rowSums(TabPre) - 
                                                     diag(TabPre))/rowSums(TabPre), 2) * 100)
  names(dimnames(tab2)) <- c("Initial Model", "Updated Model")
  print(tab2)
  cat("\n \n Combined Data \n  \n")
  Tab <- tabReclas
  tab <- cbind(Tab, ` % reclassified` = round((rowSums(Tab) - 
                                                 diag(Tab))/rowSums(Tab), 2) * 100)
  names(dimnames(tab)) <- c("Initial Model", "Updated Model")
  print(tab)
  cat(" _________________________________________\n")
  c11 <- factor(c1, levels = levels(c1), labels = c(1:length(levels(c1))))
  c22 <- factor(c2, levels = levels(c2), labels = c(1:length(levels(c2))))
  x <- improveProb(x1 = as.numeric(c11) * (1/(length(levels(c11)))), 
                   x2 = as.numeric(c22) * (1/(length(levels(c22)))), y = data[, 
                                                                              cOutcome])
  y <- improveProb(x1 = predrisk1, x2 = predrisk2, y = data[, 
                                                            cOutcome])
  cat("\n NRI(Categorical) [95% CI]:", round(x$nri, 4), "[", 
      round(x$nri - 1.96 * x$se.nri, 4), "-", round(x$nri + 
                                                      1.96 * x$se.nri, 4), "]", "; p-value:", round(2 * 
                                                                                                      pnorm(-abs(x$z.nri)), 5), "\n")
  cat(" NRI(Continuous) [95% CI]:", round(y$nri, 4), "[", round(y$nri - 
                                                                  1.96 * y$se.nri, 4), "-", round(y$nri + 1.96 * y$se.nri, 
                                                                                                  4), "]", "; p-value:", round(2 * pnorm(-abs(y$z.nri)), 
                                                                                                                               5), "\n")
  cat(" IDI [95% CI]:", round(y$idi, 4), "[", round(y$idi - 
                                                      1.96 * y$se.idi, 4), "-", round(y$idi + 1.96 * y$se.idi, 
                                                                                      4), "]", "; p-value:", round(2 * pnorm(-abs(y$z.idi)), 
                                                                                                                   5), "\n")
  NRI_cont <- data.frame(Type = "Continuous",
                         NRI = as.numeric(y$nri),
                         NRI_CI_lower = as.numeric(y$nri - 1.96 * y$se.nri),
                         NRI_CI_upper = as.numeric(y$nri + 1.96 * y$se.nri),
                         p = as.numeric(2 * pnorm(-abs(y$z.nri))))
  
  NRI_cat <- data.frame(Type = "Categorical",
                        NRI = as.numeric(x$nri),
                        NRI_CI_lower = as.numeric(x$nri - 1.96 * x$se.nri),
                        NRI_CI_upper = as.numeric(x$nri + 1.96 * x$se.nri),
                        p = as.numeric(2 * pnorm(-abs(x$z.nri))))
  
  NRI_df <- rbind(NRI_cat, NRI_cont)
  
  return(NRI_df)
}

#Load procedure list
procedure_list <- read.csv("data/SNAP2_procedurelist_specialty_coded.csv") %>% select(Code, Specialty)

#Wrangle component variables for SORT
clinical_only <- clinical_only %>% 
  mutate(AgeCat = cut(as.numeric(S01Age), breaks = c(0,64,79, Inf))) %>%
  mutate(ASA = recode(S03AsaPsClass,
                      `I` = "I or II",
                      `II` = "I or II",
                      `III` = "III",
                      `IV` = "IV",
                      `V` = "V")) %>%
  mutate(OpUrgency = S02OperativeUrgency) %>%
  left_join(procedure_list, by = c("S02PlannedProcedure" = "Code")) %>%
  mutate(Specialty = ifelse(Specialty %in% c("Bariatric", "Colorectal", "UpperGI", "HPB", "Thoracic", "Vascular"), 1, 0)) %>%
  mutate(OpSeverity = recode(S02PlannedProcSeverity,
                             `Xma` = "Xma/Com", 
                             `Com` = "Xma/Com")) %>%
  mutate(Malignancy = ifelse((S04Malignancy %in% c("MDM", "MNM", "PM", "Y", "1")), 1, 0))

#Compute logistic model based only on SORT variables
logit_SORT <- glm(mort30 ~ AgeCat + ASA + OpUrgency + Specialty + OpSeverity + Malignancy,
                  data = clinical_only, family = binomial)

val.prob(y = clinical_only$mort30, p = predict(logit_SORT, type = "response", newdata = clinical_only), xlab = "Predicted Probability (Recalibrated SORT)")

#Compute logistic model with baseline SORT variables + added clinician prediction variable
logit_SORT_clinical <- update(logit_SORT, . ~ . + S03PerioperativeTeamOfTheRiskOfDeathWithin30Days)

val.prob(y = clinical_only$mort30, p = predict(logit_SORT_clinical, type = "response", newdata = clinical_only), xlab = "Predicted Probability (Recalibrated SORT + Clinical)")

#Create a test dataframe with prediction outputs and patient outcomes to assess NRI 
#to compare between combined model and SORT
test.df <- data.frame(mort30 = clinical_only$mort30,
                     phat.sort = predict(logit_SORT, 
                                         type = "response", 
                                         newdata = clinical_only),
                     phat.sort_clinical = predict(logit_SORT_clinical, 
                                                  type = "response", 
                                                  newdata = clinical_only)) %>%
  drop_na()

#Compute NRI
reclassification2(data = test.df, 
                 cOutcome = 1, 
                 predrisk1 = test.df$phat.sort, 
                 predrisk2 = test.df$phat.sort_clinical, 
                 cutoff = c(0, 0.05, 0.5, 1))


## ----SNAP2_Clinician_Prediction_clin_SORT_NRI2---------------------------
#Create a test dataframe with prediction outputs and patient outcomes to assess NRI 
#to compare between combined model and clinical alone
test.df <- data.frame(mort30 = clinical_only$mort30,
                     phat.sort = predict(logit_clinical, type = "response", newdata = clinical_only),
                     phat.sort_clinical = predict(logit_SORT_clinical, type = "response", newdata = clinical_only)) %>%
  drop_na()

#Compute NRI
reclassification2(data = test.df, 
                 cOutcome = 1, 
                 predrisk1 = test.df$phat.sort, 
                 predrisk2 = test.df$phat.sort_clinical, 
                 cutoff = c(0, 0.05, 0.5, 1))


## ----SNAP2_Clinician_Prediction_clin_SORT_vanilla_NRI--------------------
#Refit the model with the original SORT prediction output (as opposed to refitted SORT coefficients)
logit_SORT2 <- glm(mort30 ~ SORT_mort,
                  data = clinical_only, family = binomial)
val.prob(y = clinical_only$mort30, p = clinical_only$SORT_mort_risk)

logit_SORT_clinical2 <- update(logit_SORT2, . ~ . + S03PerioperativeTeamOfTheRiskOfDeathWithin30Days)
val.prob(y = clinical_only$mort30, p = predict(logit_SORT_clinical2, type = "response", newdata = clinical_only))

#Create a test dataframe with prediction outputs and patient outcomes to assess NRI
#to compare new combined model with original SORT prediction
test.df <- data.frame(mort30 = clinical_only$mort30,
                     phat.sort = clinical_only$SORT_mort_risk,
                     phat.sort_clinical = predict(logit_SORT_clinical2, 
                                                  type = "response", 
                                                  newdata = clinical_only)) %>%
  drop_na()

#Compute NRI
reclassification2(data = test.df, 
                 cOutcome = 1, 
                 predrisk1 = test.df$phat.sort, 
                 predrisk2 = test.df$phat.sort_clinical, 
                 cutoff = c(0, 0.05, 0.5, 1))


## ----SNAP2_Clinician_Prediction_clin_SORT_vanilla_NRI2-------------------
logit_SORT2 <- glm(mort30 ~ SORT_mort,
                  data = clinical_only, family = binomial)
val.prob(y = clinical_only$mort30, p = clinical_only$SORT_mort_risk)

logit_SORT_clinical2 <- update(logit_SORT2, . ~ . + S03PerioperativeTeamOfTheRiskOfDeathWithin30Days)
val.prob(y = clinical_only$mort30, p = predict(logit_SORT_clinical2, type = "response", newdata = clinical_only))

clinical_only<- clinical_only %>%
  mutate(clin_pred = recode(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                            `<1%` = 0.005,
                            `1-2.5%` = 0.0175,
                            `2.6-5%` = 0.0375,
                            `5.1-10%` = 0.075,
                            `10.1-50%` = 0.3,
                            `>50%` = 0.75))

#Create a test dataframe with prediction outputs and patient outcomes to assess NRI
#to compare between new combined model with clinical prediction
test.df <- data.frame(mort30 = clinical_only$mort30,
                     phat.clinical = clinical_only$clin_pred,
                     phat.sort_clinical = predict(logit_SORT_clinical2, 
                                                  type = "response", 
                                                  newdata = clinical_only),
                     phat.sort = clinical_only$SORT_mort_risk) %>%
  drop_na()

clinical_roc <- roc(response = test.df$mort30,
                    predictor = test.df$phat.clinical)
SORT2_roc <- roc(response = test.df$mort30, 
                 predictor = test.df$phat.sort)
clinicalSORT_roc <- roc(response = test.df$mort30, 
                 predictor = test.df$phat.sort_clinical)

#Compute NRI
reclassification2(data = test.df, 
                 cOutcome = 1, 
                 predrisk1 = test.df$phat.clinical, 
                 predrisk2 = test.df$phat.sort_clinical, 
                 cutoff = c(0, 0.05, 1))


## ----SNAP2_Clinician_Prediction_SORT_clin--------------------------------
#Refit the model using the lrm function from the rms package in order to easily compute bootstrapped performance statistics
clinical_only_rms <- clinical_only %>%
  mutate(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days = factor(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, ordered = FALSE)) %>%
  mutate(SORT_mort_risk = SORT_mort_risk * 100) %>%
  select(CaseId, mort30, SORT_mort_risk, S03PerioperativeTeamOfTheRiskOfDeathWithin30Days)

lrm_SORT_clin2 <- lrm(mort30 ~ SORT_mort_risk + S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                      data = clinical_only_rms,
                      x = TRUE,
                      y = TRUE,
                      maxit=1000)
lrm_SORT_clin2

clinical_only_rms$predicted <- predict(lrm_SORT_clin2, type = "fitted")

clinical_only_rms <- clinical_only_rms %>% select(CaseId, mort30, predicted) %>%
  as.data.frame()

cal_lrm <- PresenceAbsence::calibration.plot(DATA = clinical_only_rms,
                                             which.model = 1,
                                             na.rm = TRUE, alpha = 0.05, N.bins = 10,
                                             main = "Combined lrm",
                                             ylab = "Observed Mortality", 
                                             xlab = "Predicted Mortality") %>%
  drop_na()

#Calibrate and validate using 1000 bootstraps
set.seed(20180912)
bootval_SORT_clin2 <- validate(lrm_SORT_clin2, B = 1000)
bootval_SORT_clin2

bootval_SORT_clin2["Dxy", "index.corrected"]/2 + 0.5

set.seed(20180912)
bootcal_SORT_clin2 <- calibrate(lrm_SORT_clin2, B = 1000, predy = seq(0, 1, length = 50))
plot(bootcal_SORT_clin2)

#Create a test dataframe with prediction outputs and patient outcomes to assess NRI
#to compare between new combined model and clinical prediction
test.df <- data.frame(mort30 = clinical_only$mort30,
                     phat.clinical = clinical_only$clin_pred,
                     phat.sort_clinical = predict(lrm_SORT_clin2, type = "fitted"),
                     phat.sort = clinical_only$SORT_mort_risk) %>%
  drop_na()

clinical_roc <- roc(response = test.df$mort30,
                    predictor = test.df$phat.clinical)
SORT2_roc <- roc(response = test.df$mort30, 
                 predictor = test.df$phat.sort)
clinicalSORT_roc <- roc(response = test.df$mort30, 
                 predictor = test.df$phat.sort_clinical)


## ----DCA-----------------------------------------------------------------
#Perform decision curve analysis as suggested by reviewers

#Write a function to compute the combined predictions
combined_model <- function(SORT_mortality, Clinical) {
  arm::invlogit(0.04028 * (SORT_mortality * 100) + 
                  1.487 * (Clinical == "1-2.5%") + 
                  2.365 * (Clinical == "2.6-5%") + 
                  3.074 * (Clinical == "5.1-10%") + 
                  4.156 * (Clinical == "10.1-50%") + 
                  5.028 * (Clinical == ">50%") -
                  6.403)
}

patients_complete$combined_mort_risk <- combined_model(SORT_mortality = 
                                                      patients_complete$SORT_mort_risk, 
                                                    Clinical = 
                                                      patients_complete$S03PerioperativeTeamOfTheRiskOfDeathWithin30Days)

patients_complete$mortality <- as.numeric(patients_complete$mort30)

dca_SORT <- decision_curve(mortality ~ SORT_mort_risk, data = patients_complete, fitted.risk = TRUE)
dca_pPOSSUM <- decision_curve(mortality ~ pPOSSUM_mort_risk, data = patients_complete, fitted.risk = TRUE)
dca_SRS <- decision_curve(mortality ~ SRS_mort_risk, data = patients_complete, fitted.risk = TRUE)
dca_combined <- decision_curve(mortality ~ combined_mort_risk, data = patients_complete, fitted.risk = TRUE)

DCA_plot <- plot_decision_curve(list(dca_SORT, dca_pPOSSUM, dca_SRS, dca_combined), confidence.intervals = FALSE,
                    curve.names = c("SORT", "P-POSSUM", "SRS", "Combined"))

#pdf("dca.pdf", width = 8, height = 6)
plot_decision_curve(list(dca_SORT, dca_pPOSSUM, dca_SRS, dca_combined), confidence.intervals = FALSE,
                    curve.names = c("SORT", "P-POSSUM", "SRS", "Combined"))
#dev.off()


## ----SNAP2_Clinician_Prediction_clin_model_highrisk----------------------
#Perform sensitivity analyses
#Subset a high risk group
clinical_higher_risk <- clinical_only %>% 
  filter(S01Age > 40) %>%
  filter(S02PlannedProcedureMainGroup != "15" ) %>%
  filter(S02PlannedProcedureSubGroup != "6-10") %>%
  filter(Specialty == 1 |
           S01Age >= 70 |
           S03PastMedicalHistoryCoronaryArteryDisease == "Y" |
           S03PastMedicalHistoryCongestiveCardiacFailure == "Y" |
           S03PastMedicalHistoryStrokeTIA == "Y" |
           S03Diabetes %in% c("1", "2I", "2O") |
           S03PastMedicalHistoryRenalDisease == "Y" |
           S03DrugTreatmentAntiAnginal == "Y" |
           S03DrugTreatmentAntiHypertensive == "Y" |
           S03ElevatedJugularVenousPressureJvp == "Y")

#Validate the performance of SORT, PPOSSUM and SRS in this group

#Use the un-recalibrated SORT predictions
val.prob(y = clinical_higher_risk$mort30, p = arm::invlogit(clinical_higher_risk$SORT_mort))

#Use the un-recalibrated PPOSSUM predictions
val.prob(y = clinical_higher_risk$mort30, p = arm::invlogit(clinical_higher_risk$pPOSSUM_mort))

#Use the un-recalibrated SRS predictions
val.prob(y = clinical_higher_risk$mort30, p = arm::invlogit(clinical_higher_risk$SRS_mort))

#Subset a high risk group
patients_higher_risk <- patients_complete %>%
  left_join(procedure_list, by = c("S02PlannedProcedure" = "Code")) %>%
  mutate(Specialty = ifelse(Specialty %in% c("Bariatric", "Colorectal", "UpperGI", "HPB", "Thoracic", "Vascular"), 1, 0)) %>%
  filter(S01Age > 40) %>%
  filter(S02PlannedProcedureMainGroup != "15" ) %>%
  filter(S02PlannedProcedureSubGroup != "6-10") %>%
  filter(Specialty == 1 |
           S01Age >= 70 |
           S03PastMedicalHistoryCoronaryArteryDisease == "Y" |
           S03PastMedicalHistoryCongestiveCardiacFailure == "Y" |
           S03PastMedicalHistoryStrokeTIA == "Y" |
           S03Diabetes %in% c("1", "2I", "2O") |
           S03PastMedicalHistoryRenalDisease == "Y" |
           S03DrugTreatmentAntiAnginal == "Y" |
           S03DrugTreatmentAntiHypertensive == "Y" |
           S03ElevatedJugularVenousPressureJvp == "Y")

#Validate the performance of SORT, PPOSSUM and SRS in this group

#Use the un-recalibrated SORT predictions
val.prob(y = patients_higher_risk$mort30, p = arm::invlogit(patients_higher_risk$SORT_mort))

#Use the un-recalibrated PPOSSUM predictions
val.prob(y = patients_higher_risk$mort30, 
         p = arm::invlogit(patients_higher_risk$pPOSSUM_mort))

#Use the un-recalibrated SRS predictions
val.prob(y = patients_higher_risk$mort30, p = arm::invlogit(patients_higher_risk$SORT_mort))


## ----SNAP2_Clinician_Prediction_clin_model_highrisk2---------------------
val.prob(y = clinical_higher_risk$mort30, p = predict(logit_clinical, type = "response", newdata = clinical_higher_risk))

val.prob(y = clinical_higher_risk$mort30, p = predict(logit_SORT_clinical, type = "response", newdata = clinical_higher_risk))

#val.prob(y = clinical_higher_risk$mort30, p = predict(logit_pPOSSUM_clinical, type = "response", newdata = clinical_higher_risk))

#val.prob(y = clinical_higher_risk$mort30, p = predict(logit_SRS_clinical, type = "response", newdata = clinical_higher_risk))


## ----SNAP2_Clinician_Prediction_clin_model_highrisk_NRI------------------
#SORT
test.df <- data.frame(mort30 = clinical_higher_risk$mort30,
                     phat.base = predict(logit_SORT, type = "response", newdata = clinical_higher_risk),
                     phat.base_clinical = predict(logit_SORT_clinical, type = "response", newdata = clinical_higher_risk)) %>%
  drop_na()

reclassification2(data = test.df, 
                 cOutcome = 1, 
                 predrisk1 = test.df$phat.base, 
                 predrisk2 = test.df$phat.base_clinical, 
                 cutoff = c(0, 0.05, 1))


## ----SNAP2_Clinician_Prediction_clin_model_countries---------------------
#Validate the performance of SORT, PPOSSUM and SRS in the UK group vs in the Aus/NZ
#Use the un-recalibrated SORT predictions
val.prob(y = filter(patients_complete, country == "UK")$mort30, 
         p = arm::invlogit(filter(patients_complete, country == "UK")$SORT_mort),
         xlab = "UK SORT Predicted Probability")
  
#Use the un-recalibrated PPOSSUM predictions
val.prob(y = filter(patients_complete, country == "UK")$mort30, 
         p = arm::invlogit(filter(patients_complete, country == "UK")$pPOSSUM_mort),
         xlab = "UK SORT Predicted Probability")
  
#Use the un-recalibrated SRS predictions
val.prob(y = filter(patients_complete, country == "UK")$mort30,
         p = arm::invlogit(filter(patients_complete, country == "UK")$SRS_mort),
         xlab = "UK SORT Predicted Probability")

# Combine Aus/NZ
val.prob(y = filter(patients_complete, country %in% c("Aus", "NZ"))$mort30, 
         p = arm::invlogit(filter(patients_complete, country %in% c("Aus", "NZ"))$SORT_mort),
         xlab = paste("Aus/NZ SORT Predicted Probability"))

val.prob(y = filter(patients_complete, country %in% c("Aus", "NZ"))$mort30, 
         p = arm::invlogit(filter(patients_complete, country %in% c("Aus", "NZ"))$pPOSSUM_mort),
         xlab = paste("Aus/NZ P-POSSUM Predicted Probability"))

val.prob(y = filter(patients_complete, country %in% c("Aus", "NZ"))$mort30, 
         p = arm::invlogit(filter(patients_complete, country %in% c("Aus", "NZ"))$SRS_mort),
         xlab = paste("Aus/NZ SRS Predicted Probability"))


## ----SNAP2_Clinician_Prediction_with_other_models------------------------
clinical_plus_others <- filter(patients_complete, clinical_only == FALSE)
#Validate clinician predictions in the 3 countries
print(roc(response = clinical_plus_others$mort30,
          predictor = clinical_plus_others$S03PerioperativeTeamOfTheRiskOfDeathWithin30Days))
  
tempDF <- clinical_plus_others %>%
  filter(!is.na(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days)) %>%
  group_by(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days) %>% 
  summarise(deaths = sum(mort30 == TRUE, na.rm = TRUE), 
            survived = sum(mort30 == FALSE, na.rm = TRUE), 
            mortality_prob = sum(mort30 == TRUE, na.rm = TRUE)/(sum(mort30 == TRUE, na.rm = TRUE) + sum(mort30 == FALSE, na.rm = TRUE)), 
            ci.lower = binconf(sum(mort30 == TRUE, na.rm = TRUE), (sum(mort30 == TRUE, na.rm = TRUE) + sum(mort30 == FALSE, na.rm = TRUE)))[2], 
            ci.upper = binconf(sum(mort30 == TRUE, na.rm = TRUE), (sum(mort30 == TRUE, na.rm = TRUE) + sum(mort30 == FALSE, na.rm = TRUE)))[3]) %>%
  mutate(predicted = c(0.005, 0.0175, 0.0375, 0.075, 0.3, 0.75))
  
with(tempDF,  errbar(x = predicted,
                     y = mortality_prob, 
                     yminus = ci.lower, 
                     yplus = ci.upper,
                     ylim = c(0, 1), 
                     xlim = c(0, 1),
                     xlab = "predicted",
                     ylab = "actual"))
lines(x = c(0,1), y = c(0,1), col = "grey")
lines(lowess(mortality_prob ~ predicted, tempDF, iter = 0), lty = 2)


## ----SNAP2_Clinician_Prediction_sensitivity_1_prep, echo=FALSE, message=FALSE, warning=FALSE, results="hide", fig.show='hide'----
patients_higher_temp <- patients_higher_risk %>% 
  mutate(mort30 = as.numeric(mort30)) %>%
  select(CaseId, mort30, pPOSSUM_mort_risk, SRS_mort_risk, SORT_mort_risk) %>% 
  drop_na() %>%
  as.data.frame()

clinical_higher_temp <- clinical_higher_risk %>%
  mutate(clinpred_lo = recode(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                            `<1%` = 0.00,
                            `1-2.5%` = 0.01,
                            `2.6-5%` = 0.025,
                            `5.1-10%` = 0.05,
                            `10.1-50%` = 0.1,
                            `>50%` = 0.5)) %>%
  mutate(clinpred_mid = recode(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                            `<1%` = 0.005,
                            `1-2.5%` = 0.0175,
                            `2.6-5%` = 0.0375,
                            `5.1-10%` = 0.075,
                            `10.1-50%` = 0.3,
                            `>50%` = 0.75)) %>%
  mutate(clinpred_hi = recode(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                            `<1%` = 0.01,
                            `1-2.5%` = 0.025,
                            `2.6-5%` = 0.05,
                            `5.1-10%` = 0.1,
                            `10.1-50%` = 0.5,
                            `>50%` = 1.0)) %>%
  mutate(mort30 = as.numeric(mort30)) %>%
  select(CaseId, mort30, clinpred_lo, clinpred_mid, clinpred_hi) %>%
  drop_na() %>%
  as.data.frame()

senscal_pPOSSUM <- PresenceAbsence::calibration.plot(DATA = patients_higher_temp,
                                                 which.model = 1,
                                                 na.rm = TRUE, alpha = 0.05, N.bins = 10,
                                                 main = "P-POSSUM",
                                                 ylab = "Observed Mortality", 
                                                 xlab = "Predicted Mortality") %>%
  drop_na()

senscal_SRS <- PresenceAbsence::calibration.plot(DATA = patients_higher_temp,
                                             which.model = 2,
                                             na.rm = TRUE, alpha = 0.05, N.bins = 10,
                                             main = "SRS",
                                             ylab = "Observed Mortality", 
                                             xlab = "Predicted Mortality") %>%
  drop_na()

senscal_SORT <- PresenceAbsence::calibration.plot(DATA = patients_higher_temp,
                                              which.model = 3,
                                              na.rm = TRUE, alpha = 0.05, N.bins = 10,
                                              main = "SORT",
                                              ylab = "Observed Mortality", 
                                              xlab = "Predicted Mortality") %>%
  drop_na()

senscal_Clin <- PresenceAbsence::calibration.plot(DATA = clinical_higher_temp,
                                              which.model = 2, 
                                              na.rm = TRUE, alpha = 0.05, N.bins = 15,
                                              main = "Clinician Prediction",
                                              ylab = "Observed Mortality", 
                                              xlab = "Predicted Mortality") %>%
  drop_na()


## ----SNAP2_Clinician_Prediction_sensitivity_2_prep, echo=FALSE, message=FALSE, warning=FALSE, results="hide", fig.show='hide'----
clinical_plus_temp <- clinical_plus_others %>%
  mutate(clinpred_lo = recode(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                            `<1%` = 0.00,
                            `1-2.5%` = 0.01,
                            `2.6-5%` = 0.025,
                            `5.1-10%` = 0.05,
                            `10.1-50%` = 0.1,
                            `>50%` = 0.5)) %>%
  mutate(clinpred_mid = recode(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                            `<1%` = 0.005,
                            `1-2.5%` = 0.0175,
                            `2.6-5%` = 0.0375,
                            `5.1-10%` = 0.075,
                            `10.1-50%` = 0.3,
                            `>50%` = 0.75)) %>%
  mutate(clinpred_hi = recode(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                            `<1%` = 0.01,
                            `1-2.5%` = 0.025,
                            `2.6-5%` = 0.05,
                            `5.1-10%` = 0.1,
                            `10.1-50%` = 0.5,
                            `>50%` = 1.0)) %>%
  mutate(mort30 = as.numeric(mort30)) %>%
  select(CaseId, mort30, clinpred_lo, clinpred_mid, clinpred_hi) %>%
  drop_na() %>%
  as.data.frame()

senscal_Clin_plus <- PresenceAbsence::calibration.plot(DATA = clinical_plus_temp,
                                              which.model = 2, 
                                              na.rm = TRUE, alpha = 0.05, N.bins = 15,
                                              main = "Clinician Prediction",
                                              ylab = "Observed Mortality", 
                                              xlab = "Predicted Mortality") %>%
  drop_na()


## ----SNAP2_Clinician_Prediction_sensitivity_3_prep, echo=FALSE, message=FALSE, warning=FALSE, results="hide", fig.show='hide'----
#Create subset of data for ANZ
patients_ANZ_temp <- patients_complete %>% 
  filter(country %in% c("Aus", "NZ")) %>%
  mutate(mort30 = as.numeric(mort30)) %>%
  select(CaseId, mort30, pPOSSUM_mort_risk, SRS_mort_risk, SORT_mort_risk, S03AsaPsClass) %>% 
  drop_na() %>%
  as.data.frame()

#Create subset of data for UK
patients_UK_temp <- patients_complete %>% 
  filter(country %in% c("UK")) %>%
  mutate(mort30 = as.numeric(mort30)) %>%
  select(CaseId, mort30, pPOSSUM_mort_risk, SRS_mort_risk, SORT_mort_risk, S03AsaPsClass) %>% 
  drop_na() %>%
  as.data.frame()

#Create subset of ANZ data with clinician judgement only
clinical_ANZ_temp <- clinical_only %>%
  filter(country %in% c("Aus", "NZ")) %>%
  mutate(clinpred_lo = recode(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                            `<1%` = 0.00,
                            `1-2.5%` = 0.01,
                            `2.6-5%` = 0.025,
                            `5.1-10%` = 0.05,
                            `10.1-50%` = 0.1,
                            `>50%` = 0.5)) %>%
  mutate(clinpred_mid = recode(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                            `<1%` = 0.005,
                            `1-2.5%` = 0.0175,
                            `2.6-5%` = 0.0375,
                            `5.1-10%` = 0.075,
                            `10.1-50%` = 0.3,
                            `>50%` = 0.75)) %>%
  mutate(clinpred_hi = recode(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                            `<1%` = 0.01,
                            `1-2.5%` = 0.025,
                            `2.6-5%` = 0.05,
                            `5.1-10%` = 0.1,
                            `10.1-50%` = 0.5,
                            `>50%` = 1.0)) %>%
  mutate(mort30 = as.numeric(mort30)) %>%
  select(CaseId, mort30, clinpred_lo, clinpred_mid, clinpred_hi) %>%
  drop_na() %>%
  as.data.frame()

#Create subset of UK data with clinician judgement only
clinical_UK_temp <- clinical_only %>%
  filter(country %in% c("UK")) %>%
  mutate(clinpred_lo = recode(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                            `<1%` = 0.00,
                            `1-2.5%` = 0.01,
                            `2.6-5%` = 0.025,
                            `5.1-10%` = 0.05,
                            `10.1-50%` = 0.1,
                            `>50%` = 0.5)) %>%
  mutate(clinpred_mid = recode(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                            `<1%` = 0.005,
                            `1-2.5%` = 0.0175,
                            `2.6-5%` = 0.0375,
                            `5.1-10%` = 0.075,
                            `10.1-50%` = 0.3,
                            `>50%` = 0.75)) %>%
  mutate(clinpred_hi = recode(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                            `<1%` = 0.01,
                            `1-2.5%` = 0.025,
                            `2.6-5%` = 0.05,
                            `5.1-10%` = 0.1,
                            `10.1-50%` = 0.5,
                            `>50%` = 1.0)) %>%
  mutate(mort30 = as.numeric(mort30)) %>%
  select(CaseId, mort30, clinpred_lo, clinpred_mid, clinpred_hi) %>%
  drop_na() %>%
  as.data.frame()

#Create calibration plot objects for the ANZ data
senscal_ANZ_pPOSSUM <- PresenceAbsence::calibration.plot(DATA = patients_ANZ_temp,
                                                 which.model = 1,
                                                 na.rm = TRUE, alpha = 0.05, N.bins = 10,
                                                 main = "P-POSSUM",
                                                 ylab = "Observed Mortality", 
                                                 xlab = "Predicted Mortality") %>%
  drop_na()

senscal_ANZ_SRS <- PresenceAbsence::calibration.plot(DATA = patients_ANZ_temp,
                                             which.model = 2,
                                             na.rm = TRUE, alpha = 0.05, N.bins = 10,
                                             main = "SRS",
                                             ylab = "Observed Mortality", 
                                             xlab = "Predicted Mortality") %>%
  drop_na()

senscal_ANZ_SORT <- PresenceAbsence::calibration.plot(DATA = patients_ANZ_temp,
                                              which.model = 3,
                                              na.rm = TRUE, alpha = 0.05, N.bins = 10,
                                              main = "SORT",
                                              ylab = "Observed Mortality", 
                                              xlab = "Predicted Mortality") %>%
  drop_na()

senscal_ANZ_Clin <- PresenceAbsence::calibration.plot(DATA = clinical_ANZ_temp,
                                              which.model = 2, 
                                              na.rm = TRUE, alpha = 0.05, N.bins = 15,
                                              main = "Clinician Prediction",
                                              ylab = "Observed Mortality", 
                                              xlab = "Predicted Mortality") %>%
  drop_na()

#Create calibration plot objects for the UK data
senscal_UK_pPOSSUM <- PresenceAbsence::calibration.plot(DATA = patients_UK_temp,
                                                 which.model = 1,
                                                 na.rm = TRUE, alpha = 0.05, N.bins = 10,
                                                 main = "P-POSSUM",
                                                 ylab = "Observed Mortality", 
                                                 xlab = "Predicted Mortality") %>%
  drop_na()

senscal_UK_SRS <- PresenceAbsence::calibration.plot(DATA = patients_UK_temp,
                                             which.model = 2,
                                             na.rm = TRUE, alpha = 0.05, N.bins = 10,
                                             main = "SRS",
                                             ylab = "Observed Mortality", 
                                             xlab = "Predicted Mortality") %>%
  drop_na()

senscal_UK_SORT <- PresenceAbsence::calibration.plot(DATA = patients_UK_temp,
                                              which.model = 3,
                                              na.rm = TRUE, alpha = 0.05, N.bins = 10,
                                              main = "SORT",
                                              ylab = "Observed Mortality", 
                                              xlab = "Predicted Mortality") %>%
  drop_na()

senscal_UK_Clin <- PresenceAbsence::calibration.plot(DATA = clinical_UK_temp,
                                              which.model = 2, 
                                              na.rm = TRUE, alpha = 0.05, N.bins = 15,
                                              main = "Clinician Prediction",
                                              ylab = "Observed Mortality", 
                                              xlab = "Predicted Mortality") %>%
  drop_na()

#Create a table of AUROCs comparing UK to ANZ
sensitivity_tab4 <- data.frame(ANZ = 
                          c(roc(response = patients_ANZ_temp$mort30, predictor = factor(patients_ANZ_temp$S03AsaPsClass, levels = c("I", "II", "III", "Iv", "V"), ordered = TRUE))$auc,
                            roc(response = patients_ANZ_temp$mort30, predictor = patients_ANZ_temp$pPOSSUM_mort)$auc, 
                            roc(response = patients_ANZ_temp$mort30, predictor = patients_ANZ_temp$SRS_mort)$auc, 
                            roc(response = patients_ANZ_temp$mort30, predictor = patients_ANZ_temp$SORT_mort)$auc,
                            roc(response = clinical_ANZ_temp$mort30, predictor = clinical_ANZ_temp$clinpred_mid)$auc),
                          UK = c(roc(response = patients_UK_temp$mort30, predictor = factor(patients_UK_temp$S03AsaPsClass, levels = c("I", "II", "III", "Iv", "V"), ordered = TRUE))$auc, 
                            roc(response = patients_UK_temp$mort30, predictor = patients_UK_temp$pPOSSUM_mort)$auc, 
                            roc(response = patients_UK_temp$mort30, predictor = patients_UK_temp$SRS_mort)$auc, 
                            roc(response = patients_UK_temp$mort30, predictor = patients_UK_temp$SORT_mort)$auc,
                            roc(response = clinical_UK_temp$mort30, predictor = clinical_UK_temp$clinpred_mid)$auc),
                          p_value = 
                            c(roc.test(roc(response = patients_ANZ_temp$mort30, 
                                           predictor = factor(patients_ANZ_temp$S03AsaPsClass, 
                                                              levels = c("I", "II", "III", "Iv", "V"), ordered = TRUE)),
                                       roc(response = patients_UK_temp$mort30, 
                                           predictor = factor(patients_UK_temp$S03AsaPsClass, 
                                                              levels = c("I", "II", "III", "Iv", "V"), ordered = TRUE)))$p.value,
                              roc.test(roc(response = patients_ANZ_temp$mort30, predictor = patients_ANZ_temp$pPOSSUM_mort),
                                       roc(response = patients_UK_temp$mort30, predictor = patients_UK_temp$pPOSSUM_mort))$p.value,
                              roc.test(roc(response = patients_ANZ_temp$mort30, predictor = patients_ANZ_temp$SRS_mort),
                                       roc(response = patients_UK_temp$mort30, predictor = patients_UK_temp$SRS_mort))$p.value,
                              roc.test(roc(response = patients_ANZ_temp$mort30, predictor = patients_ANZ_temp$SORT_mort),
                                       roc(response = patients_UK_temp$mort30, predictor = patients_UK_temp$SORT_mort))$p.value,
                              roc.test(roc(response = clinical_ANZ_temp$mort30, predictor = clinical_ANZ_temp$clinpred_mid),
                                       roc(response = clinical_UK_temp$mort30, predictor = clinical_UK_temp$clinpred_mid))$p.value))
rownames(sensitivity_tab4) <- c("ASA-PS", "P-POSSUM", "SRS", "SORT", "Clinical")
colnames(sensitivity_tab4) <- c("Australia/New Zealand",
                                "UK",
                                "p-value")


## ----SNAP2_Clinician_Prediction_sensitivity_4_prep, echo=FALSE, message=FALSE, warning=FALSE, results="hide", fig.show='hide'----
patients_full_possum <- patients_clean %>%
  select(CaseId, 
         S01Age,
         S03ElevatedJugularVenousPressureJvp,
         S03RadiologicalFindingsCardiomegaly,
         S03PeripheralOedema,
         S03DrugTreatmentWarfarin,
         S03DrugTreatmentDiureticTreatment,
         S03DrugTreatmentAntiAnginal,
         S03DrugTreatmentDigoxinTherapy,
         S03DrugTreatmentAntiHypertensive,
         S03Dyspnoea,
         S03RadiologicalFindingsConsolidation,
         S03PastMedicalHistoryPulmonaryFibrosis,
         S03PastMedicalHistoryCOPD,
         S03SystolicBloodPressureBpAtPreAssessment,
         S03PulseRateAtPreoperativeAssessment,
         S03GlasgowComaScaleGcsPreInductionOfAnaesthesia,
         S03Hb,
         S03WhiteCellCountWcc,
         S03Urea,
         S03Na,
         S03K,
         S03EcgFindings) %>%
  drop_na(S03Hb:S03K) %>%
  pull(CaseId)

patients_full_possum <- patients_clean %>%
  filter(CaseId %in% patients_full_possum) %>% 
  mutate(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days =
           recode_factor(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days,
                         `L1` = "<1%",
                         `L2` = "1-2.5%",
                         `L6` = "2.6-5%",
                         `L10` = "5.1-10%",
                         `L50` = "10.1-50%", 
                         `G50` = ">50%",
                         .ordered = TRUE)) %>%
  mutate(clinpred_mid = recode(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                            `<1%` = 0.005,
                            `1-2.5%` = 0.0175,
                            `2.6-5%` = 0.0375,
                            `5.1-10%` = 0.075,
                            `10.1-50%` = 0.3,
                            `>50%` = 0.75))

patients_missing_possum <- patients_clean %>%
  filter(!(CaseId %in% patients_full_possum$CaseId))

full_possum_table_1 <- cbind(print(CreateTableOne(vars = c("S03AsaPsClass",
                                               "S01Age",
                                               "S02OperativeUrgency",
                                               "specialty_recoded",
                                               "S02PlannedProcSeverity"), 
                                      data = patients_full_possum), quote = FALSE, printToggle = FALSE),
                 print(CreateTableOne(vars = c("S03AsaPsClass",
                                               "S01Age",
                                               "S02OperativeUrgency",
                                               "specialty_recoded",
                                               "S02PlannedProcSeverity"), data = patients_missing_possum), quote = FALSE, printToggle = FALSE))
colnames(full_possum_table_1) <- c("Full pPOSSUM data", "Missing pPOSSUM data")

pander(full_possum_table_1)

#Create calibration plot objects for the full POSSUM data
senscal_full_possum_possum <- patients_full_possum %>% 
  mutate(mort30 = as.numeric(mort30)) %>%
  select(CaseId, mort30, pPOSSUM_mort_risk, SRS_mort_risk, SORT_mort_risk) %>% 
  drop_na() %>%
  as.data.frame() %>%
  PresenceAbsence::calibration.plot(DATA = .,
                                    which.model = 1,
                                    na.rm = TRUE, alpha = 0.05, N.bins = 10,
                                    main = "P-POSSUM (full P-POSSUM data)",
                                    ylab = "Observed Mortality", 
                                    xlab = "Predicted Mortality")

senscal_full_possum_SRS <- patients_full_possum %>% 
  mutate(mort30 = as.numeric(mort30)) %>%
  select(CaseId, mort30, pPOSSUM_mort_risk, SRS_mort_risk, SORT_mort_risk) %>% 
  drop_na() %>%
  as.data.frame() %>%
  PresenceAbsence::calibration.plot(DATA = .,
                                    which.model = 2,
                                    na.rm = TRUE, alpha = 0.05, N.bins = 10,
                                    main = "SRS (full P-POSSUM data)",
                                    ylab = "Observed Mortality", 
                                    xlab = "Predicted Mortality")

senscal_full_possum_SORT <- patients_full_possum %>% 
  mutate(mort30 = as.numeric(mort30)) %>%
  select(CaseId, mort30, pPOSSUM_mort_risk, SRS_mort_risk, SORT_mort_risk) %>% 
  drop_na() %>%
  as.data.frame() %>%
  PresenceAbsence::calibration.plot(DATA = .,
                                    which.model = 3,
                                    na.rm = TRUE, alpha = 0.05, N.bins = 10,
                                    main = "SORT (full P-POSSUM data)",
                                    ylab = "Observed Mortality", 
                                    xlab = "Predicted Mortality")

senscal_full_possum_clin <- patients_full_possum %>% 
  mutate(mort30 = as.numeric(mort30)) %>%
  select(CaseId, mort30, clinpred_mid) %>% 
  drop_na() %>%
  as.data.frame() %>%
  PresenceAbsence::calibration.plot(DATA = .,
                                    which.model = 1,
                                    na.rm = TRUE, alpha = 0.05, N.bins = 10,
                                    main = "Clinician (full P-POSSUM data)",
                                    ylab = "Observed Mortality", 
                                    xlab = "Predicted Mortality")

pROC::roc(response = patients_full_possum$mort30, 
          predictor = patients_full_possum$S03PerioperativeTeamOfTheRiskOfDeathWithin30Days)

pROC::roc(response = patients_full_possum$mort30, 
          predictor = patients_full_possum$pPOSSUM_mort_risk)

pROC::roc(response = patients_full_possum$mort30, 
          predictor = patients_full_possum$SRS_mort_risk)

pROC::roc(response = patients_full_possum$mort30, 
          predictor = patients_full_possum$SORT_mort_risk)


## ----SNAP2_Clinician_Prediction_sensitivity_5_prep, echo=FALSE, message=FALSE, warning=FALSE, results="hide", fig.show='hide'----
sens_anal_5 <- function(specialty_name){
patients_group <- patients_complete %>%
  filter(specialty_recoded %in% c(specialty_name)) %>% 
  mutate(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days =
           recode_factor(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days,
                         `L1` = "<1%",
                         `L2` = "1-2.5%",
                         `L6` = "2.6-5%",
                         `L10` = "5.1-10%",
                         `L50` = "10.1-50%", 
                         `G50` = ">50%",
                         .ordered = TRUE)) %>%
  mutate(clinpred_mid = recode(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days, 
                            `<1%` = 0.005,
                            `1-2.5%` = 0.0175,
                            `2.6-5%` = 0.0375,
                            `5.1-10%` = 0.075,
                            `10.1-50%` = 0.3,
                            `>50%` = 0.75))

sens_roc <- pROC::roc(response = patients_group$mort30, 
                      predictor = patients_group$combined_mort_risk)

sens_HL <- patients_group %>% select(mort30, combined_mort_risk) %>%
  drop_na() %>%
  with(., generalhoslem::logitgof(obs = mort30, 
                          exp = combined_mort_risk))

output <- data.frame(`Specialty subgroup` = specialty_name, 
                     `n` = nrow(patients_group),
                     `AUROC` = round(sens_roc$auc, digits = 3),
                     `AUROC_CI` = paste0(round(ci(sens_roc)[1], digits = 3), 
                                         " to ", 
                                         round(ci(sens_roc)[3], digits = 3)),
                     `Hosmer-Lemeshow_statistic` = sens_HL$statistic,
                     `Hosmer-Lemeshow_p-value` = sens_HL$p.value)
rownames(output) <- NULL

return(output)
}

sensitivity_analysis_table <- sens_anal_5("GI") %>%
  bind_rows(sens_anal_5("Gynaecology-Urology")) %>%
  bind_rows(sens_anal_5("Ortho")) %>%
  bind_rows(sens_anal_5("Neuro-Spine")) %>%
  bind_rows(sens_anal_5("Thoracic")) %>%
  bind_rows(sens_anal_5("Vascular")) %>%
  bind_rows(sens_anal_5("Other"))

pander(sensitivity_analysis_table, caption = "Discrimination and calibration performance of the new combined prediction model in different specialty subgroups.")


## ----SNAP2_Clinician_Prediction_strobe, echo=FALSE, message=FALSE, warning=FALSE----
#Set up the contents for each box
a1 <- paste0('Total available patients\nfrom ',
             length(unique(patients_clean$SiteCode)),
             ' hospitals\n(n = ', 
             formatC(nrow(patients_clean), big.mark = ','), 
             ')')
b1 <- ''
c1 <- paste0('Included for risk model\nexternal validation analysis\n(n = ',
             formatC(nrow(patients_complete), big.mark = ','),
             ')')
d1 <- ''
e1 <- paste0('Included for clinician prediction\nperformance analysis and\nclinician + risk prediction tool\ncombined modelling (n = ',
             formatC(nrow(clinical_only), big.mark = ','),
             ')')
a2 <- ''
b2 <- paste0('Excluded (total n = ',
             formatC(strobe_table[1,2] - strobe_table[4,2], big.mark = ','),
             '):\n\t',
             'Obstetric patients (n = ', 
             formatC(strobe_table[1,2] - strobe_table[2,2], big.mark = ','), 
             ')\n\t',
             'Missing Predictor variables (n = ', 
             formatC(strobe_table[2,2] - strobe_table[3,2], big.mark = ','), 
             ')\n\t',
             'Missing 30-day mortality outcome (n = ',
             formatC(strobe_table[3,2] - strobe_table[4,2], big.mark = ','),
             ')')
c2 <- ''
d2 <- paste0('Excluded (total n = ',
             formatC(nrow(patients_complete) - nrow(clinical_only), big.mark = ','),
             '):\n\t',
             'Missing clinician predictions (n = 35)\n\t',
             'Aided by other tools (n = ',
             formatC(nrow(patients_complete) - nrow(clinical_only) - 35, big.mark = ','),
             ')')
e2 <- ''

ndf <- create_node_df(
  n = 10,
  label = c(a1, b1, c1, d1, e1, #Column 1
            a2, b2, c2, d2, e2), #Column 2
  style = c('solid', 'invis', 'solid', 'invis', 'solid', #Column 1
            'invis', 'solid', 'invis', 'solid', 'invis'), #Column 2
  shape = c('box', 'point', 'box', 'point', 'box', #Column 1 
            'plaintext', 'box', 'point', 'box', 'point'), #Column 2
  width = c(3, 0.001, 3, 0.001, 3.5, #Column 1
            2, 4, 0.001, 4, 0.001), #Column 2
  height = c(1, 0.001, 1, 0.001, 1.2, #Column 1
             1, 1.4, 0.001, 1, 0.001), #Column 2
  fontsize = c(rep(14, 10)),
  fontname = c(rep('Helvetica', 10)),
  penwidth = 1.5,
  fixedsize = 'true')

edf <- create_edge_df(
  from = c(1, 2, 3, 4, #Column 1
           6, 7, 8, 9, #Column 2
           2, 4 #Horizontals
           ),
  to = c(2, 3, 4, 5, #Column 1
         7, 8, 9, 10, #Column 2
         7, 9 #Horizontals
         ),
  arrowhead = c('none', 'normal', 'none', 'normal', #Column 1
                'none', 'none', 'none', 'none', #Column 2
                'normal', 'normal' #Horizontals
                ),
  color = c('black', 'black', 'black', 'black', #Column 1
            '#00000000', '#00000000', '#00000000', '#00000000', #Column 2
            'black', 'black' #Horizontals
            ),
  constraint = c(rep('true', 8), #Columns
                 rep('false', 2) #Horizontals
                 )
)
  
g <- create_graph(ndf,
                  edf,
                  attr_theme = NULL)

render_graph(g)

#export_graph(g, file_name = "../output/figures/SNAP2_Clinician_Predictions_Fig1.png", width = 1600, height = 1800)

#export_graph(g, file_name = "../output/figures/SNAP2_Clinician_Predictions_Fig1.pdf")

#STROBE_flow_diagram %>% export_svg() %>% charToRaw() %>% rsvg() %>% png::writePNG("../output/figures/Cancellations_STROBE.png", dpi = 300)

#STROBE_flow_diagram %>% export_svg() %>% charToRaw() %>% rsvg_pdf(file = "../output/figures/Cancellations_STROBE.pdf")


## ----table2_prep----
#Table 2 for the manuscript
table2 <- CreateTableOne(vars = c("S03MortalityEstimateClinicalJudgment",
                                  "S03MortalityEstimateASAPSScore",
                                  "S03MortalityEstimateDukeOtherActivityStatusIndex",
                                  "S03MortalityEstimateWalkTest",
                                  "S03MortalityEstimateCardiopulmonaryExerciseTesting",
                                  "S03MortalityEstimateFrailtyAssessment",
                                  "S03MortalityEstimateSurgicalRiskScale",
                                  "S03MortalityEstimateSurgicalOutcomeRiskTool",
                                  "S03MortalityEstimateEuroSCORE",
                                  "S03MortalityEstimatePOSSUM",
                                  "S03MortalityEstimatePPOSSUM",
                                  "S03MortalityEstimateSurgeryPPOSSUM",
                                  "S03MortalityEstimateOther"), data = patients_complete)
print(table2, printToggle = FALSE) %>% pander(caption = "Table 2: Breakdown of information sources used by clinician in estimating 30-day mortality. Clinical teams could select one or more categories, percentages (in parentheses) therefore do not sum to 100%.")


## ----table3_prep----
#Table 3 for the manuscript
pander(lrm_SORT_clin2)

## ----table4_prep----
#Table 4 for the manuscript
#Create a test dataframe with prediction outputs and patient outcomes to assess NRI
table4.df <- data.frame(mort30 = clinical_only$mort30,
                        phat.clinical = clinical_only$clin_pred,
                        phat.sort = clinical_only$SORT_mort_risk,
                        phat.sort_clinical = predict(lrm_SORT_clin2, type = "fitted")) %>%
  drop_na()

table4 <- table4.df %>% summarise(Clinician = roc(response = mort30, predictor = phat.clinical)$auc,
                                  SORT = roc(response = mort30, predictor = phat.sort)$auc,
                                  Combined = (bootval_SORT_clin2["Dxy", "index.corrected"]/2 + 0.5)) %>%
  gather(Model, AUROC, Clinician:Combined)
table4$AUROC_CI <- c(paste0(ci(roc(response = table4.df$mort30, 
                                   predictor = table4.df$phat.clinical))[1],
                            " - ",
                            ci(roc(response = table4.df$mort30, 
                                   predictor = table4.df$phat.clinical))[3]),
                     paste0(ci(roc(response = table4.df$mort30, 
                                   predictor = table4.df$phat.sort))[1],
                            " - ",
                            ci(roc(response = table4.df$mort30, 
                                   predictor = table4.df$phat.sort))[3]),
                     paste0(ci(roc(response = table4.df$mort30, 
                                   predictor = table4.df$phat.sort_clinical))[1],
                            " - ",
                            ci(roc(response = table4.df$mort30, 
                                   predictor = table4.df$phat.sort_clinical))[3]))
table4$AUROC_p <- c(NA,
                    roc.test(roc(response = table4.df$mort30, predictor = table4.df$phat.clinical), 
                             roc(response = table4.df$mort30, predictor = table4.df$phat.sort), method = "delong")$p.value,
                    roc.test(roc(response = table4.df$mort30, predictor = table4.df$phat.clinical), 
                             roc(response = table4.df$mort30, predictor = table4.df$phat.sort_clinical), method = "delong")$p.value)

table4a <- data.frame(NRI = NA, NRI_CI = NA, NRI_p = NA)

table4b <- rbind(reclassification2(data = table4.df, 
                                   cOutcome = 1, 
                                   predrisk1 = table4.df$phat.clinical, 
                                   predrisk2 = table4.df$phat.sort, 
                                   cutoff = c(0, 0.05, 1)),
                 reclassification2(data = table4.df, 
                                   cOutcome = 1, 
                                   predrisk1 = table4.df$phat.clinical, 
                                   predrisk2 = table4.df$phat.sort_clinical, 
                                   cutoff = c(0, 0.05, 1))) %>%
  filter(Type == "Continuous") %>%
  mutate(NRI_CI = paste0(NRI_CI_lower, " - ", NRI_CI_upper)) %>%
  rename(NRI_p = p) %>%
  select(NRI, NRI_CI, NRI_p)

table4a <- rbind(table4a, table4b)

table4 <- cbind(table4, table4a)

pander(table4, 
       caption = paste0("Table 4: Performance metrics for clinician prediction versus SORT risk prediction, and versus a logistic regression model combining Clinician and SORT prediction. Performance metrics were computed based on the subset of patients in whom clinician judgement alone was used to estimate risk (n = ", 
                        formatC(nrow(clinical_only), big.mark = ","),
                        "). The reported AUROC for the combined model is the optimism-corrected value from bootstrapped internal validation."))


## ----fig2_prep----
#To plot Figure 2 for the manuscript

cal_plot <- function(df, title) {
  
  ggplot(df) +
    geom_abline(slope = 1, intercept = 0, col = "grey") +
    geom_point(aes(x = BinPred, y = BinObs)) +
    geom_linerange(aes(x = BinPred, ymin = BinObsCIlower, ymax = BinObsCIupper)) +
    geom_smooth(aes(x = BinPred, y = BinObs), method = "loess", se = FALSE, span = 1) +
    #geom_text(aes(x = BinPred, y = -0.05, label = NBin)) +
    xlim(0, 1) +
    ylim(0, 1) + 
    labs(title = title,
         x = "Predicted mortality",
         y = "Observed mortality") +
    coord_fixed() +
    theme_classic()
  
}

roc3_plot <- function(roc1, roc2, roc3, showAUC = TRUE, interval = 0.25, breaks = seq(0, 1, interval), title){
  
  require(pROC)
  if(class(roc1) != "roc" | class(roc2) != "roc")
    simpleError("Please provide roc object from pROC package")
  plotx1 <- rev(roc1$specificities)
  ploty1 <- rev(roc1$sensitivities)
  plotx2 <- rev(roc2$specificities)
  ploty2 <- rev(roc2$sensitivities)
  plotx3 <- rev(roc3$specificities)
  ploty3 <- rev(roc3$sensitivities)
  
  ggplot(NULL) +
    geom_segment(aes(x = 0, y = 1, xend = 1,yend = 0), col = "grey") + 
    geom_line(aes(x = plotx1, y = ploty1, col = "P-POSSUM"), size = 1) +
    geom_line(aes(x = plotx2, y = ploty2, col = "SRS"), size = 1) +
    geom_line(aes(x = plotx3, y = ploty3, col = "SORT"), size = 1) +
    scale_x_reverse(name = "Specificity",
                    limits = c(1,0), breaks = breaks, expand = c(0.001,0.001)) + 
    scale_y_continuous(name = "Sensitivity", 
                       limits = c(0,1), breaks = breaks, expand = c(0.001, 0.001)) +
    labs(title = title) +
    scale_colour_manual(name = "", values = c("P-POSSUM" = "#3366ff", "SRS" = "#e41a1c", "SORT" = "#4daf4a")) + 
    theme_classic() + 
    theme(axis.ticks = element_line(color = "grey80")) +
    theme(legend.position = c(0.8, 0.2), 
          legend.text = element_text(size = 8)) +
    coord_fixed()
  
}

roc4_plot <- function(roc1, roc2, roc3, roc4, showAUC = TRUE, interval = 0.25, breaks = seq(0, 1, interval), title){
  
  require(pROC)
  if(class(roc1) != "roc" | class(roc2) != "roc")
    simpleError("Please provide roc object from pROC package")
  plotx1 <- rev(roc1$specificities)
  ploty1 <- rev(roc1$sensitivities)
  plotx2 <- rev(roc2$specificities)
  ploty2 <- rev(roc2$sensitivities)
  plotx3 <- rev(roc3$specificities)
  ploty3 <- rev(roc3$sensitivities)
  plotx4 <- rev(roc4$specificities)
  ploty4 <- rev(roc4$sensitivities)
  
  ggplot(NULL) +
    geom_segment(aes(x = 0, y = 1, xend = 1,yend = 0), col = "grey") +
    geom_line(aes(x = plotx1, y = ploty1, col = "ASA-PS"), size = 1) +
    geom_line(aes(x = plotx2, y = ploty2, col = "P-POSSUM"), size = 1) +
    geom_line(aes(x = plotx3, y = ploty3, col = "SRS"), size = 1) +
    geom_line(aes(x = plotx4, y = ploty4, col = "SORT"), size = 1) +
    scale_x_reverse(name = "Specificity",
                    limits = c(1,0), breaks = breaks, expand = c(0.001,0.001)) + 
    scale_y_continuous(name = "Sensitivity", 
                       limits = c(0,1), breaks = breaks, expand = c(0.001, 0.001)) +
    labs(title = title) +
    scale_colour_manual(name = "", values = c("ASA-PS" = "#d7191c", "P-POSSUM" = "#fdae61", "SRS" = "#abd9e9", "SORT" = "#2c7bb6")) + 
    theme_classic() + 
    theme(axis.ticks = element_line(color = "grey80")) +
    theme(legend.position = c(0.8, 0.22), 
          legend.text = element_text(size = 8)) +
    coord_fixed()
  
}

plot_grid(cal_plot(cal_pPOSSUM, "P-POSSUM calibration"),
          cal_plot(cal_SRS, "SRS calibration"),
          cal_plot(cal_SORT, "SORT calibration"),
          roc4_plot(ASA_roc, pPOSSUM_roc, SRS_roc, SORT_roc, title = "ROC curves"), 
          labels = c("A", "B", "C", "D"))
#ggsave("../output/figures/SNAP2_Clinician_Predictions_Fig2.pdf", width = 8, height = 8, units = "in", dpi = 600)

## ----fig3_prep----
#To plot Figure 3 for the manuscript

roc1_plot <- function(roc1, showAUC = TRUE, interval = 0.25, breaks = seq(0, 1, interval), title, leg){
  
  require(pROC)
  if(class(roc1) != "roc")
    simpleError("Please provide roc object from pROC package")
  plotx1 <- rev(roc1$specificities)
  ploty1 <- rev(roc1$sensitivities)
  
  ggplot(NULL) +
    geom_segment(aes(x = 0, y = 1, xend = 1,yend = 0), col = "grey") + 
    geom_line(aes(x = plotx1, y = ploty1, col = "#3366ff"), size = 1) +
    scale_x_reverse(name = "Specificity",limits = c(1,0), breaks = breaks, expand = c(0.001, 0.001)) + 
    scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = breaks, expand = c(0.001, 0.001)) +
    labs(title = title) +
    scale_colour_manual(name = "", values = "#3366ff", labels = leg) + 
    theme_classic() + 
    theme(axis.ticks = element_line(color = "grey80")) +
    theme(legend.position = c(0.8, 0.1), 
          legend.text = element_text(size = 8)) +
    coord_fixed()
  
}

bootcal_plot <- function(df, title) {
  
  df <- data.frame(pred_mort = df[, "predy"], 
                   apparent_mort = df[, "calibrated.orig"],
                   corrected_mort = df[, "calibrated.corrected"])
  
  ggplot(df) +
    geom_abline(slope = 1, intercept = 0, col = "grey") +
    geom_path(data = df, 
              aes(x = pred_mort, y = apparent_mort, col = "Apparent"), 
              size = 1, alpha = 0.8) +
    geom_path(data = df, 
              aes(x = pred_mort, y = corrected_mort, col = "Optimism-corrected"),
              size = 1, alpha = 0.8) +
    #geom_text(aes(x = BinPred, y = -0.05, label = NBin)) +
    xlim(0, 1) +
    ylim(0, 1) + 
    labs(title = title,
         x = "Predicted mortality",
         y = "Observed mortality") +
    scale_colour_manual(name = "", values = c("Apparent" = "#3366ff", "Optimism-corrected" = "#e41a1c")) + 
    coord_fixed() +
    theme_classic() +
    theme(legend.position = c(0.8, 0.14), 
          legend.text = element_text(size = 8))
  
}

plot_grid(cal_plot(cal_Clin, "Clinician prediction calibration"),
          roc1_plot(clinical_roc, title = "Clinician prediction ROC curve", leg = "Clinician"), 
          bootcal_plot(bootcal_SORT_clin2, "Combined model calibration"),
          roc1_plot(clinicalSORT_roc, title = "Combined model ROC curve", leg = "Combined"),
          labels = c("A", "B", "C", "D"))
#ggsave("../output/figures/SNAP2_Clinician_Predictions_Fig3.pdf", width = 8, height = 8, units = "in", dpi = 600)

## ----fig5_prep----
#To plot Figure 5 for the manuscript

plot_rms <- expand.grid(S03PerioperativeTeamOfTheRiskOfDeathWithin30Days = 
                          c(">50%", "10.1-50%","5.1-10%", "2.6-5%", "1-2.5%", "<1%"),
                        SORT_mort_risk = seq(0, 100, by = 0.01))

plot_rms$predicted <- predict(lrm_SORT_clin2, newdata = plot_rms, type = "fitted")

ggplot(filter(plot_rms, !is.na(predicted))) +
  #geom_jitter(aes(y = predicted, x = SORT_mort_risk, col = S03PerioperativeTeamOfTheRiskOfDeathWithin30Days), alpha = 0.4) +
  geom_path(aes(y = predicted, 
                x = SORT_mort_risk, 
                col = S03PerioperativeTeamOfTheRiskOfDeathWithin30Days),
            size = 1) +
  coord_fixed(100) +
  theme_classic() +
  theme(legend.position="bottom",
        legend.text = element_text(size = 8)) + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_continuous(labels = scales::unit_format(unit = "%", sep = "")) +
  labs(title = "Combined model risk predictions",
       subtitle = "(Based on SORT and clinical assessments)",
       x = "SORT-predicted risk",
       y = "Combined model risk prediction",
       colour = "Clinician prediction")
#ggsave("../output/figures/SNAP2_Clinician_Predictions_Fig5.pdf", width = 6, height = 6, units = "in", dpi = 600)

## ----SNAP2_Clinician_Prediction_sessionInfo------------------------------
sessionInfo()

## R version 3.5.2 (2018-12-20)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 18363)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United Kingdom.1252 
## [2] LC_CTYPE=English_United Kingdom.1252   
## [3] LC_MONETARY=English_United Kingdom.1252
## [4] LC_NUMERIC=C                           
## [5] LC_TIME=English_United Kingdom.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] survminer_0.4.3     ggpubr_0.1.8        magrittr_1.5       
##  [4] nricens_1.6         cowplot_0.9.3       DiagrammeRsvg_0.1  
##  [7] DiagrammeR_0.9.2    pander_0.6.2        PredictABEL_1.2-2  
## [10] PBSmodelling_2.68.6 epitools_0.5-10     ROCR_1.0-7         
## [13] gplots_3.0.1        caret_6.0-80        rms_5.1-2          
## [16] SparseM_1.77        Hmisc_4.1-1         Formula_1.2-3      
## [19] survival_2.42-6     lattice_0.20-38     pROC_1.12.1        
## [22] tableone_0.9.3      knitr_1.25          forcats_0.5.0      
## [25] stringr_1.4.0       dplyr_0.8.5         purrr_0.3.3        
## [28] readr_1.3.1         tidyr_1.0.2         tibble_2.1.3       
## [31] ggplot2_3.3.0       tidyverse_1.3.0    
## 
## loaded via a namespace (and not attached):
##   [1] utf8_1.1.4            tidyselect_0.2.5      lme4_1.1-18-1        
##   [4] htmlwidgets_1.5.1     grid_3.5.2            munsell_0.5.0        
##   [7] codetools_0.2-15      withr_2.1.2           colorspace_1.3-2     
##  [10] rgexf_0.15.3          rstudioapi_0.11       geometry_0.3-6       
##  [13] stats4_3.5.2          robustbase_0.93-2     dimRed_0.1.0         
##  [16] labeling_0.3          KMsurv_0.1-5          farver_2.0.3         
##  [19] downloader_0.4        coda_0.19-1           vctrs_0.2.3          
##  [22] generics_0.0.2        TH.data_1.0-9         ipred_0.9-7          
##  [25] xfun_0.10             R6_2.2.2              arm_1.10-1           
##  [28] rsvg_1.3              DRR_0.0.3             bitops_1.0-6         
##  [31] reshape_0.8.7         assertthat_0.2.0      scales_1.1.0         
##  [34] multcomp_1.4-8        nnet_7.3-12           gtable_0.2.0         
##  [37] ddalpha_1.3.4         sandwich_2.5-0        timeDate_3043.102    
##  [40] rlang_0.4.5           MatrixModels_0.4-1    CVST_0.2-2           
##  [43] cmprsk_2.2-7          RcppRoll_0.3.0        splines_3.5.2        
##  [46] ModelMetrics_1.2.0    acepack_1.4.1         broom_0.5.5          
##  [49] brew_1.0-6            checkmate_1.8.5       yaml_2.2.0           
##  [52] reshape2_1.4.3        abind_1.4-5           modelr_0.1.6         
##  [55] backports_1.1.2       tools_3.5.2           lava_1.6.3           
##  [58] tcltk_3.5.2           influenceR_0.1.0      ellipsis_0.3.0       
##  [61] RColorBrewer_1.1-2    Rcpp_1.0.1            plyr_1.8.6           
##  [64] base64enc_0.1-3       visNetwork_2.0.4      rpart_4.1-13         
##  [67] viridis_0.5.1         zoo_1.8-3             sfsmisc_1.1-2        
##  [70] haven_2.2.0           cluster_2.0.7-1       fs_1.3.2             
##  [73] survey_3.33-2         data.table_1.12.8     reprex_0.3.0         
##  [76] mvtnorm_1.0-8         xtable_1.8-3          hms_0.5.3            
##  [79] evaluate_0.14         XML_3.98-1.16         readxl_1.3.1         
##  [82] gridExtra_2.3         compiler_3.5.2        KernSmooth_2.23-15   
##  [85] V8_1.5                crayon_1.3.4          minqa_1.2.4          
##  [88] htmltools_0.4.0       lubridate_1.7.4       generalhoslem_1.3.2  
##  [91] DBI_1.0.0             magic_1.5-8           dbplyr_1.4.2         
##  [94] MASS_7.3-51.1         Matrix_1.2-15         cli_1.1.0            
##  [97] gdata_2.18.0          gower_0.1.2           igraph_1.2.2         
## [100] km.ci_0.5-2           pkgconfig_2.0.2       foreign_0.8-71       
## [103] recipes_0.1.3         xml2_1.2.4            foreach_1.4.4        
## [106] prodlim_2018.04.18    rvest_0.3.5           digest_0.6.25        
## [109] pls_2.7-0             rmarkdown_1.16        cellranger_1.1.0     
## [112] survMisc_0.5.5        htmlTable_1.12        Rook_1.1-1           
## [115] PresenceAbsence_1.1.9 curl_4.3              kernlab_0.9-27       
## [118] gtools_3.8.1          quantreg_5.36         nloptr_1.0.4         
## [121] lifecycle_0.2.0       nlme_3.1-137          jsonlite_1.6.1       
## [124] viridisLite_0.3.0     fansi_0.4.0           labelled_1.1.0       
## [127] pillar_1.4.3          httr_1.4.1            DEoptimR_1.0-8       
## [130] glue_1.4.0            iterators_1.0.10      class_7.3-14         
## [133] stringi_1.1.7         polspline_1.1.13      latticeExtra_0.6-28  
## [136] caTools_1.17.1.1      e1071_1.7-0

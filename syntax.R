#######################################################################
###--------------------------------------------------------------------
### DATA ###

load("data_cvd_bz.rda") ### A TOTAL OF 234957 FOR DISEASE ANALYSES
load("data_image.rda") ### A TOTAL OF 16678 FOR CMR ANALYSES



#######################################################################
###--------------------------------------------------------------------
### COX MODELS FOR MAIN ANALYSIS ###


### CATEGORICAL EXPOSURE
model1 <- coxph(Surv(htn_time,htn)~BZ_CAT+age0+sex+imd0+whr0+nosmoking+sleeph+regular+dieth+fsp2+road1+lden+glu+hba1c+chol+ldl+tg,data_cvd_bz)


### CONTINUOUS EXPOSURE
model2 <- coxph(Surv(htn_time,htn)~BZ_IQR+age0+sex+imd0+whr0+nosmoking+sleeph+regular+dieth+fsp2+road1+lden+glu+hba1c+chol+ldl+tg,data_cvd_bz)


### INTERACTION ANALYSIS
model3 <- coxph(Surv(htn_time,htn)~BZ_CAT*relevel(sex, ref='female')+age0+imd0+whr0+nosmoking+sleeph+regular+dieth+fsp2+road1+lden+glu+hba1c+chol+ldl+tg,data_cvd_bz)



#######################################################################
###--------------------------------------------------------------------
### LOGISTIC REGRESSIONS FOR CMR PHENOTYPES ###


### MAIN ANLAYSES
model4 <- glm(scale(lvedvi)~BZ_CAT+age0+sex+imd0+whr0+nosmoking+sleeph+regular+dieth+fsp2+lden+glu+hba1c+chol+ldl+tg, family='gaussian', data_image)
model5 <- glm(scale(lvedvi)~BZ_IQR+age0+sex+imd0+whr0+nosmoking+sleeph+regular+dieth+fsp2+lden+glu+hba1c+chol+ldl+tg, family='gaussian', data_image)


### STRATIFICATION BY SEX
model6 <- glm(scale(lvedvi)~BZ_CAT*relevel(sex, ref='male')+age0+imd0+whr0+nosmoking+sleeph+regular+dieth+fsp2+lden+glu+hba1c+chol+ldl+tg, family='gaussian', data_image)
model7 <- glm(scale(lvedvi)~BZ_IQR*relevel(sex, ref='female')+age0+imd0+whr0+nosmoking+sleeph+regular+dieth+fsp2+lden+glu+hba1c+chol+ldl+tg, family='gaussian', data_image)












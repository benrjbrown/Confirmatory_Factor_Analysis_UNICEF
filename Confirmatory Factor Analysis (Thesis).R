
library(lavaan)
library(tidyverse)

dt <- read.csv("YIS Calculations for Community Brief Data.csv")

head(dt)



# The code below is based on the following link 
# https://lavaan.ugent.be/tutorial/cfa.html

# specify the model
HS.model <- ' family =~ SUPPORT1 + SUPPORT2 + SUPPORT3 + SUPPORT4
              friend =~ SUPPORT5 + SUPPORT6 + SUPPORT7 + SUPPORT8
              belonging =~ BELONGING + SOC_ISO1 + SOC_ISO2 + SOC_ISO3 + SOC_ISO4
              school =~ SCH_FEEL + SCH_ATT1 + SCH_ATT2 + SCH_ATT3 + TEACHER1 + TEACHER2 + TEACHER3 + TEACHER4 + TEACHER5 + TEACHER6 + TEACHER7 + TEACHER8
              mental =~ MNTLHLTH + SADNESS + SYMPTOM1 + SYMPTOM2 + SYMPTOM3 + SYMPTOM4 + SYMPTOM5 + SYMPTOM6 + SYMPTOM7 + SYMPTOM8
              well =~ LIFESAT + DAILYSTRESS + BESTLIFE '

# fit the model for confirmatory factor analysis
fitcfa <- cfa(HS.model, data = dt, missing = "ml", estimator = "mlr")

# fit the model for full structural equation models
fitsem <- sem(HS.model, data = dt)

# fit the model for growth curve models
fitgrowth <- growth(HS.model, data = dt)


# display summary output
summary(fitcfa, fit.measures = TRUE, standardize = TRUE, modindices = TRUE)

modindices(fitcfa, sort = TRUE, maximum.number = 10)

###

HS.model2 <- 'family =~ SUPPORT1 + SUPPORT2 + SUPPORT3 + SUPPORT4
              friend =~ SUPPORT5 + SUPPORT6 + SUPPORT7 + SUPPORT8 + SOC_ISO1
              belonging =~ BELONGING + SOC_ISO1 + SOC_ISO2 + SOC_ISO3 + SOC_ISO4 + SCH_ATT3
              school =~ SCH_FEEL + SCH_ATT1 + SCH_ATT2 + SCH_ATT3 + TEACHER1 + TEACHER2 + TEACHER3 + TEACHER4 + TEACHER5 + TEACHER6 + TEACHER7 + TEACHER8
              mental =~ MNTLHLTH + SADNESS + SYMPTOM1 + SYMPTOM2 + SYMPTOM3 + SYMPTOM4 + SYMPTOM5 + SYMPTOM6 + SYMPTOM7 + SYMPTOM8 + DAILYSTRESS
              well =~ LIFESAT + DAILYSTRESS + BESTLIFE
              
              ###

              SUPPORT7 ~~ SUPPORT8
              SUPPORT5 ~~ SUPPORT6
              LIFESAT ~~ BESTLIFE
              SYMPTOM1 ~~ SYMPTOM2
              SCH_FEEL ~~ SCH_ATT2
              SCH_ATT2 ~~ SCH_ATT3 '

# fit the model for confirmatory factor analysis
fitcfa2 <- cfa(HS.model2, data = dt, missing = "ml", estimator = "mlr")

# fit the model for full structural equation models
fitsem <- sem(HS.model2, data = dt)

# fit the model for growth curve models
fitgrowth <- growth(HS.model2, data = dt)


# display summary output
summary(fitcfa2, fit.measures = TRUE, standardize = TRUE, modindices = TRUE)

modindices(fitcfa2, sort = TRUE, maximum.number = 10)

anova(fitcfa, fitcfa2)

### July 8

###

cfa.modelx <- 'family =~ SUPPORT1 + SUPPORT2 + SUPPORT3 + SUPPORT4
              friend =~ SUPPORT5 + SUPPORT6 + SUPPORT7 + SUPPORT8 + SOC_ISO1
              belonging =~ BELONGING + SOC_ISO1 + SOC_ISO2 + SOC_ISO3 + SOC_ISO4 + SCH_ATT3
              school =~ SCH_FEEL + SCH_ATT1 + SCH_ATT2 + SCH_ATT3 + TEACHER1 + TEACHER2 + TEACHER3 + TEACHER4 + TEACHER5 + TEACHER6 + TEACHER7 + TEACHER8
              mental =~ MNTLHLTH + SADNESS + SYMPTOM1 + SYMPTOM2 + SYMPTOM3 + SYMPTOM4 + SYMPTOM5 + SYMPTOM6 + SYMPTOM7 + SYMPTOM8 + DAILYSTRESS
              well =~ LIFESAT + DAILYSTRESS + BESTLIFE
              
              ###

              SUPPORT7 ~~ SUPPORT8
              SUPPORT5 ~~ SUPPORT6
              LIFESAT ~~ BESTLIFE
              SYMPTOM1 ~~ SYMPTOM2
              SCH_FEEL ~~ SCH_ATT2
              SCH_ATT2 ~~ SCH_ATT3 

              #regressions
              well ~ family + friend + belonging + school + GENDER + AGE + WELLOFF
              mental ~ family + friend + belonging + school  + GENDER + AGE + WELLOFF

'

fitcfa3x <- cfa(cfa.modelx, data = dt, missing = "ml", estimator = "mlr")

summary(fitcfa3x, fit.measures = TRUE, standardize = TRUE, modindices = TRUE)

###

HS.model4 <- 'family =~ SUPPORT1 + SUPPORT2 + SUPPORT3 + SUPPORT4
              friend =~ SUPPORT5 + SUPPORT6 + SUPPORT7 + SUPPORT8 + SOC_ISO1
              belonging =~ BELONGING + SOC_ISO1 + SOC_ISO2 + SOC_ISO3 + SOC_ISO4 + SCH_ATT3
              school =~ SCH_FEEL + SCH_ATT1 + SCH_ATT2 + SCH_ATT3 + TEACHER1 + TEACHER2 + TEACHER3 + TEACHER4 + TEACHER5 + TEACHER6 + TEACHER7 + TEACHER8
              mental =~ MNTLHLTH + SADNESS + SYMPTOM1 + SYMPTOM2 + SYMPTOM3 + SYMPTOM4 + SYMPTOM5 + SYMPTOM6 + SYMPTOM7 + SYMPTOM8 + DAILYSTRESS
              well =~ LIFESAT + DAILYSTRESS + BESTLIFE
              
              ###

              SUPPORT7 ~~ SUPPORT8
              SUPPORT5 ~~ SUPPORT6
              LIFESAT ~~ BESTLIFE
              SYMPTOM1 ~~ SYMPTOM2
              SCH_FEEL ~~ SCH_ATT2
              SCH_ATT2 ~~ SCH_ATT3 

              #regressions
              well ~ family + friend + belonging + school
              mental ~ family + friend + belonging + school
              #
              well ~ family + friend + belonging + school + mental
              mental ~ family + friend + belonging + school + well
              family ~ friend + belonging + school + well + mental
              friend ~ belonging + school + well + mental + family
              belonging ~ school + well + mental + family + friend
              school ~ well + mental + family + friend + belonging
'

fitcfa4 <- cfa(HS.model4, data = dt, missing = "ml", estimator = "mlr")

summary(fitcfa4, fit.measures = TRUE, standardize = TRUE, modindices = TRUE)







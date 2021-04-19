#Research on Covid-19 and inter-municipal consortium (Brazil)
#Keywords: consortia; health public policy; COVID-19; innovative governance; organisational resilience

################ 
#Together it is possible to go further:
#Brazilian health inter-municipal consortium as a
#collaborative and innovative governance to fight COVID-19
################ 

# Authors: Eduardo Grin, Fernando Abrucio, Mariana Costa Silveira, Ricardo Gomes
#March 2021 - EAESP-FGV

################ 

rm(list = ls())


#Install packages
if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, ggplot2, stargazer, car, rstatix, lmtest, ggpubr, readxl, writexl, expss, psych)



#Load database
load("Database_Covid_IHC.Rda")
glimpse(bd)
str(bd)

################################################################################
#Descriptive statistics
################################################################################


#Descriptive statistics 
describe(bd)
summary(bd)


# IntermunicipalHealthConsortia - IHC

# Boxplot - Cases:
c <- ggplot(bd, aes(x=IntermunicipalHealthConsortia, y=totalCases_per_100k_inhabitants, fill=IntermunicipalHealthConsortia)) + 
  geom_boxplot()
c

# Boxplot - Deaths:
d <- ggplot(bd, aes(x=IntermunicipalHealthConsortia, y=deaths_per_100k_inhabitants, fill=IntermunicipalHealthConsortia)) + 
  geom_boxplot()
d

#Histogram Covid-19 cases
hist_cases <- bd %>%
  ggplot(aes(x=totalCases_per_100k_inhabitants, fill=IntermunicipalHealthConsortia)) +
  geom_histogram( color="#e9ecef") + 
  labs(fill="Intermunicipal Health Consortia")
hist_cases

#Histogram Covid-19 deaths
hist_deaths <- bd %>%
  ggplot(aes(x=deaths_per_100k_inhabitants, fill=IntermunicipalHealthConsortia)) +
  geom_histogram( color="#e9ecef") +
  labs(fill="Intermunicipal Health Consortia")
hist_deaths


################################################################################
# REGRESSION MODELS
# Dependent variable: Covid-19 Deaths per 100thousand inhabitants
################################################################################


#Deaths (Y) - X: (consortia)  and (contextual variables)

d1 <- lm(deaths_per_100k_inhabitants ~ IntermunicipalHealthConsortia + GDPpercapita_log +
           Education_saeb + bellow_poverty_line + 
           coverage_sewage_network, data = bd)
summary(d1)

#Models - Robust standard errors 

library(foreign)
library(sandwich)
library(lmtest)

coeftest(d1, vcov = sandwich)
coeftest(d1, vcov = vcovHC(d1, "HC0"))
coeftest(d1, vcov = vcovHC(d1, "HC3"))
rob1 = coeftest(d1, vcov = vcovHC(d1, "HC1"))


#Deaths (Y) - X: (consortia)  (contextual variables) (governmental capacities) 
d2 <- lm(deaths_per_100k_inhabitants ~ IntermunicipalHealthConsortia + GDPpercapita_log +
           Education_saeb + bellow_poverty_line + coverage_sewage_network + 
           municipal_revenue + intergovernmental_transfers +
           bureaucracy_education, data = bd)
summary(d2)

coeftest(d2, vcov = sandwich)
coeftest(d2, vcov = vcovHC(d2, "HC0"))
coeftest(d2, vcov = vcovHC(d2, "HC3"))
rob2 = coeftest(d2, vcov = vcovHC(d2, "HC1"))


#Deaths (Y) - X: (consortia)  (contextual variables) (capacities) (public heatlh variables) 

d3 <- lm(deaths_per_100k_inhabitants ~ IntermunicipalHealthConsortia + GDPpercapita_log +
           Education_saeb + bellow_poverty_line + coverage_sewage_network + 
           municipal_revenue + intergovernmental_transfers +
           bureaucracy_education + 
           health_expenditures_percapita + professionals_FHS + community_health_agents + coverage_FHS + 
           doctors_100k_inhabitants + ICU_beds_100k_inhabitants + ventilators1kinhab + 
           share_comorbidities + above65_years_old+
           need_reference_hospitalizations + need_reference_exams, data = bd)
summary(d3)

coeftest(d3, vcov = sandwich)
coeftest(d3, vcov = vcovHC(d3, "HC0"))
coeftest(d3, vcov = vcovHC(d3, "HC3"))
rob3 = coeftest(d3, vcov = vcovHC(d3, "HC1"))

#Deaths (Y) - X: (consortia)  (contextual variables) (public heatlh variables + other health variables) (capacities) 
# AND dummies (population size)

d4 <- lm(deaths_per_100k_inhabitants ~ IntermunicipalHealthConsortia + GDPpercapita_log +
           Education_saeb + bellow_poverty_line + coverage_sewage_network + 
           municipal_revenue + intergovernmental_transfers +
           bureaucracy_education + 
           health_expenditures_percapita + professionals_FHS + community_health_agents + coverage_FHS + 
           doctors_100k_inhabitants + ICU_beds_100k_inhabitants + ventilators1kinhab + 
           share_comorbidities + above65_years_old+
           need_reference_hospitalizations + need_reference_exams +
           up_to_20k_inhabitants + `20k_to_50k_inhabitants` + `50k_to_100k_inhabitants`, data = bd)
summary(d4)

coeftest(d4, vcov = sandwich)
coeftest(d4, vcov = vcovHC(d4, "HC0"))
coeftest(d4, vcov = vcovHC(d4, "HC3"))
rob4 = coeftest(d4, vcov = vcovHC(d4, "HC1"))


#Exporting results - deaths models

drobust <- stargazer(rob1, rob2, rob3, rob4, type = "html", out = "deaths_robust.htm")


################################################################################
# REGRESSION MODELS
# Dependent variable: Covid-19 - CASES per 100 thousand inhabitants
################################################################################


c1 <- lm(totalCases_per_100k_inhabitants ~ IntermunicipalHealthConsortia + GDPpercapita_log +
           Education_saeb + bellow_poverty_line + 
           coverage_sewage_network, data = bd)
summary(c1)

coeftest(c1, vcov = sandwich)
coeftest(c1, vcov = vcovHC(c1, "HC0"))
coeftest(c1, vcov = vcovHC(c1, "HC3"))
crob1 = coeftest(c1, vcov = vcovHC(c1, "HC1"))


#Deaths (Y) - X: (consortia)  (contextual variables) (governmental capacities) 
c2 <- lm(totalCases_per_100k_inhabitants ~ IntermunicipalHealthConsortia + GDPpercapita_log +
           Education_saeb + bellow_poverty_line + coverage_sewage_network + 
           municipal_revenue + intergovernmental_transfers +
           bureaucracy_education, data = bd)
summary(c2)

coeftest(c2, vcov = sandwich)
coeftest(c2, vcov = vcovHC(c2, "HC0"))
coeftest(c2, vcov = vcovHC(c2, "HC3"))
crob2 = coeftest(c2, vcov = vcovHC(c2, "HC1"))


#Deaths (Y) - X: (consortia)  (contextual variables) (capacities) (public heatlh variables) 

c3 <- lm(totalCases_per_100k_inhabitants ~ IntermunicipalHealthConsortia + GDPpercapita_log +
           Education_saeb + bellow_poverty_line + coverage_sewage_network + 
           municipal_revenue + intergovernmental_transfers +
           bureaucracy_education + 
           health_expenditures_percapita + professionals_FHS + community_health_agents + coverage_FHS + 
           doctors_100k_inhabitants + 
           need_reference_hospitalizations + need_reference_exams, data = bd)
summary(c3)

coeftest(c3, vcov = sandwich)
coeftest(c3, vcov = vcovHC(c3, "HC0"))
coeftest(c3, vcov = vcovHC(c3, "HC3"))
crob3 = coeftest(c3, vcov = vcovHC(c3, "HC1"))

#Deaths (Y) - X: (consortia)  (contextual variables) (public heatlh variables + other health variables) (capacities) 
# AND dummies (population size)

c4 <- lm(totalCases_per_100k_inhabitants ~ IntermunicipalHealthConsortia + GDPpercapita_log +
           Education_saeb + bellow_poverty_line + coverage_sewage_network + 
           municipal_revenue + intergovernmental_transfers +
           bureaucracy_education + 
           health_expenditures_percapita + professionals_FHS + community_health_agents + coverage_FHS + 
           doctors_100k_inhabitants + 
           need_reference_hospitalizations + need_reference_exams +
           up_to_20k_inhabitants + `20k_to_50k_inhabitants` + `50k_to_100k_inhabitants`, data = bd)
summary(c4)

coeftest(c4, vcov = sandwich)
coeftest(c4, vcov = vcovHC(c4, "HC0"))
coeftest(c4, vcov = vcovHC(c4, "HC3"))
crob4 = coeftest(c4, vcov = vcovHC(c4, "HC1"))


#Exporting results
crobust <- stargazer(crob1, crob2, crob3, crob4, type = "html", out = "cases_Model_Robust.htm")


############
### VIF test
############

library(car)

#Covid-19 Cases
vif(c4)

#create vector of VIF values
vif_values <- vif(c4)
vif_values
View(vif_values)

barplot(vif_values, names=c4$names, main = "VIF Values - cases", 
        horiz = FALSE, col = "steelblue", las=2, cex.axis=1,
        cex.names = 0.8)


#Covid-19 Deaths
vif(d4)
vif_values <- vif(d4)
vif_values
View(vif_values)

#create horizontal bar chart to display each VIF value

barplot(vif_values, names=c4$names, main = "VIF Values - deaths", 
        horiz = FALSE, col = "steelblue", las=2, cex.axis=1,
        cex.names = 0.8)

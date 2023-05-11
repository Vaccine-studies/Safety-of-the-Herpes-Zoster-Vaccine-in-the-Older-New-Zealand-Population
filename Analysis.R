library(ggplot2)
library("ggpubr")
library(survival)
library(data.table)
library(survminer)
library(dplyr)
library(gnm)
library(readxl)
library("writexl")
library(janitor)
gnm.RR <- function(GNM.RESULT, digits = 4) {
  
  if (GNM.RESULT$family$family == "binomial") {
    LABEL <- "OR"
  } else if (GNM.RESULT$family$family == "poisson") {
    LABEL <- "RR"
  } else {
    stop("Not logistic or Poisson model")
  }
  
  COEF      <- stats::coef(GNM.RESULT)
  CONFINT   <- stats::confint(GNM.RESULT)
  TABLE     <- cbind(coef=COEF, CONFINT)
  TABLE.EXP <- round(exp(TABLE), digits)
  
  colnames(TABLE.EXP)[1] <- LABEL
  
  TABLE.EXP
}

#df <- read.csv("D:/PhD programme/Vaccine safety/Data/Secondary/Current/person_time_UPDATED.csv")
#View(df)

# Remove duplicated rows based on unique id
df <- df %>% distinct(df$ENC_NHI, .keep_all = TRUE)

#create a new column 
#df <- df %>% mutate(all_events = STRK+CARD+MENI+RAMS+MEDI, .after = MEDI_G2)
#df <- df %>% filter(all_events !=0)
#write_xlsx(df,"D:\\safety_original\\df.xlsx")
df <- read_excel("D:/Conditional_poisson/df.xlsx")
View(df)
range(df$AGE_DIS)

###describe the data
df %>%                                  # case linelist
  tabyl(AGE, EXPOSURE) %>%                  # cross-tabulate counts
  adorn_totals(where = "col") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting(digits = 2) %>%                  # convert to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Age Category",
    col_name = "Exposure")

df %>%                                  # case linelist
  tabyl(GENDER, EXPOSURE) %>%                  # cross-tabulate counts
  adorn_totals(where = "col") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting(digits = 2) %>%                  # convert to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Sex",
    col_name = "Exposure")

df %>%                                  # case linelist
  tabyl(df$ETHNICGP_Lvl1, EXPOSURE) %>%                  # cross-tabulate counts
  adorn_totals(where = "col") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting(digits = 2) %>%                  # convert to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Ethnicity",
    col_name = "Exposure")


df %>%                                  # case linelist
  tabyl(DEP13_Q, EXPOSURE) %>%                  # cross-tabulate counts
  adorn_totals(where = "col") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting(digits = 2) %>%                  # convert to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "NZDep2013",
    col_name = "Exposure")


df %>%                                  # case linelist
  tabyl(DEP18_Q, EXPOSURE) %>%                  # cross-tabulate counts
  adorn_totals(where = "col") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting(digits = 2) %>%                  # convert to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "NZDep2018",
    col_name = "Exposure")

df %>%                                  # case linelist
  tabyl(IMD, EXPOSURE) %>%                  # cross-tabulate counts
  adorn_totals(where = "col") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting(digits = 2) %>%                  # convert to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "IMD",
    col_name = "Exposure")


df %>%                                  # case linelist
  tabyl(LOCATION, EXPOSURE) %>%                  # cross-tabulate counts
  adorn_totals(where = "col") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting(digits = 2) %>%                  # convert to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Location",
    col_name = "Exposure")


df %>%                                  # case linelist
  tabyl(DIAG_YEAR, EXPOSURE) %>%                  # cross-tabulate counts
  adorn_totals(where = "col") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting(digits = 2) %>%                  # convert to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Year",
    col_name = "Exposure")





dfh <- df %>% filter(EXPOSURE == "1")
dfc <- df %>% filter(EXPOSURE == "0")

#Stroke
dplyr::count(dfh,STRK, sort = TRUE)
dplyr::count(dfh,STRK_G1, sort = TRUE)
dplyr::count(dfh,STRK_G2, sort = TRUE)
dplyr::count(dfh,STRK_G3, sort = TRUE)
dplyr::count(dfh,STRK_G4, sort = TRUE)
dplyr::count(dfc,STRK, sort = TRUE)
dplyr::count(dfc,STRK_G1, sort = TRUE)
dplyr::count(dfc,STRK_G2, sort = TRUE)
dplyr::count(dfc,STRK_G3, sort = TRUE)
dplyr::count(dfc,STRK_G4, sort = TRUE)

#Cardiovascular
dplyr::count(dfh,CARD, sort = TRUE)
dplyr::count(dfh,CARD_G1, sort = TRUE)
dplyr::count(dfh,CARD_G2, sort = TRUE)
dplyr::count(dfh,CARD_G3, sort = TRUE)
dplyr::count(dfh,CARD_G4, sort = TRUE)
dplyr::count(dfh,CARD_G5, sort = TRUE)
dplyr::count(dfc,CARD, sort = TRUE)
dplyr::count(dfc,CARD_G1, sort = TRUE)
dplyr::count(dfc,CARD_G2, sort = TRUE)
dplyr::count(dfc,CARD_G3, sort = TRUE)
dplyr::count(dfc,CARD_G4, sort = TRUE)
dplyr::count(dfc,CARD_G5, sort = TRUE)

#MENI
dplyr::count(dfh,dfh$MENI, sort = TRUE)
dplyr::count(dfc,dfc$MENI, sort = TRUE)

#Ramsey Hunt and Bell's Palsy
dplyr::count(dfh,dfh$RAMS, sort = TRUE)
dplyr::count(dfc,dfc$RAMS, sort = TRUE)

# Medically attended events
dplyr::count(dfh,MEDI, sort = TRUE)
dplyr::count(dfh,MEDI_G1, sort = TRUE)
dplyr::count(dfh,MEDI_G2, sort = TRUE)

dplyr::count(dfc,MEDI, sort = TRUE)
dplyr::count(dfc,MEDI_G1, sort = TRUE)
dplyr::count(dfc,MEDI_G2, sort = TRUE)

#Create data set for TCI (Transient cerebral ischaemia)
#tci <- df %>% filter (STRK_G2 == 1) %>% select(MASTER_ENC,GENDER,AGE_DIS,EXPOSURE,STRK_G2,STRK_G2_PT)
#write_xlsx(tci,"D:\\Conditional_poisson\\tci.xlsx")

#Import data
tci <- read_excel("D:/Conditional_poisson/tci.xlsx")

# create loginterval (in this case, logPERSON TIME)
tci <- tci %>% mutate(logPERSONTIME = log(PERSONTIME))

#Count 
tci1 <- tci %>% filter(EXPOSURE == "1")
tci2 <- tci %>% filter(EXPOSURE == "0")
sum(tci1$PERSONTIME)
sum(tci2$PERSONTIME)

#fit a conditional poisson regression model with eliminate = indiv
gnm.RR(tci_model <- gnm(EVENTS ~ EXPOSURE, data=tci, 
                        offset = logPERSONTIME,
                        family = poisson(),
                        eliminate=factor(MASTER_ENC)))
#survival fit model
tci_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = tci)
summary(tci_model1)

#Create data set for stroke (Haemorrhagic and ischaemic)
#stroke <- df %>% filter (STRK_G4 == 1) %>% select(MASTER_ENC,VACCINATION_DATE, EVSTDATE,  GENDER,AGE_DIS,EXPOSURE,STRK_G4,STRK_G4_PT)
#write_xlsx(stroke,"D:\\Conditional_poisson\\stroke.xlsx")

#Import data
stroke <- read_excel("D:/Conditional_poisson/stroke.xlsx")

# create loginterval (in this case, logPERSON TIME)
stroke <- stroke %>% mutate(logPERSONTIME = log(PERSONTIME))

#count 
stroke1 <- stroke %>% filter(EXPOSURE == "1")
stroke2 <- stroke %>% filter(EXPOSURE == "0")
dplyr::count(stroke1,EVENTS, sort = TRUE)
dplyr::count(stroke2,EVENTS, sort = TRUE)
sum(stroke1$PERSONTIME)
sum(stroke2$PERSONTIME)


#fit a conditional poisson regression model with eliminate = indiv
gnm.RR(stroke_model <- gnm(EVENTS ~ EXPOSURE, data=stroke, 
                           offset = logPERSONTIME,
                           family = poisson(),
                           eliminate=factor(MASTER_ENC)))
#survival fit model
stroke_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = stroke)
summary(stroke_model1)

#Cerebrovascular diseases (CVD)
CVD <- rbind(tci,stroke)
CVD <- CVD %>% mutate(logPERSONTIME = log(PERSONTIME))

#count 
CVD1 <- CVD %>% filter(EXPOSURE == "1")
CVD2 <- CVD %>% filter(EXPOSURE == "0")
dplyr::count(CVD1,EVENTS, sort = TRUE)
dplyr::count(CVD2,EVENTS, sort = TRUE)
sum(CVD1$PERSONTIME)
sum(CVD2$PERSONTIME)
#fit a conditional poisson regression model with eliminate = indiv
gnm.RR(CVD_model <- gnm(EVENTS ~ EXPOSURE, data=CVD, 
                        offset = logPERSONTIME,
                        family = poisson(),
                        eliminate=factor(MASTER_ENC)))
#survival fit model
CVD_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = CVD)
summary(CVD_model1)

#Create data set for acute myocardial infarction
#aminfarction <- df %>% filter (CARD_G1 == 1) %>% select(MASTER_ENC,GENDER,AGE_DIS,EXPOSURE,CARD_G1,CARD_G1_PT)
#write_xlsx(aminfarction,"D:\\safety_original\\aminfarction.xlsx")

#Import data
aminfarction <- read_excel("D:/Conditional_poisson/aminfarction.xlsx")

# create loginterval (in this case, logPERSON TIME)
aminfarction <- aminfarction %>% mutate(logPERSONTIME = log(PERSONTIME))

#count 
aminfarction1 <- aminfarction %>% filter(EXPOSURE == "1")
aminfarction2 <- aminfarction %>% filter(EXPOSURE == "0")
dplyr::count(aminfarction1,EVENTS, sort = TRUE)
dplyr::count(aminfarction2,EVENTS, sort = TRUE)
sum(aminfarction1$PERSONTIME)
sum(aminfarction2$PERSONTIME)

#fit a conditional poisson regression model with eliminate = indiv
gnm.RR(aminfarction_model <- gnm(EVENTS ~ EXPOSURE, data=aminfarction, 
                                 offset = logPERSONTIME,
                                 family = poisson(),
                                 eliminate=factor(MASTER_ENC)))
#survival fit model
aminfarction_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = aminfarction)
summary(aminfarction_model1)

#Create data set for acute pericarditis
#pericarditis_1 <- df %>% filter (CARD_G2 == 1) %>% select(MASTER_ENC,GENDER,AGE_DIS,EXPOSURE,CARD_G2,CARD_G2_PT)
#write_xlsx(pericarditis_1,"D:\\safety_original\\pericarditis_1.xlsx")

#Import data
pericarditis <- read_excel("D:/Conditional_poisson/pericarditis.xlsx")

# create loginterval (in this case, logPERSON TIME)
pericarditis <- pericarditis %>% mutate(logPERSONTIME = log(PERSONTIME))

#count 
pericarditis1 <- pericarditis %>% filter(EXPOSURE == "1")
pericarditis2 <- pericarditis %>% filter(EXPOSURE == "0")
dplyr::count(pericarditis1,EVENTS, sort = TRUE)
dplyr::count(pericarditis2,EVENTS, sort = TRUE)
sum(pericarditis1$PERSONTIME)
sum(pericarditis2$PERSONTIME)


#fit a conditional poisson regression model with eliminate = indiv
gnm.RR(pericarditis_model <- gnm(EVENTS ~ EXPOSURE, data=pericarditis, 
                                 offset = logPERSONTIME,
                                 family = poisson(),
                                 eliminate=factor(MASTER_ENC)))
#survival fit model
pericarditis_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = pericarditis)
summary(pericarditis_model1)


#Create data set for acute myocarditis
#myocarditis <- df %>% filter (CARD_G3 == 1) %>% select(MASTER_ENC,GENDER,AGE_DIS,EXPOSURE,CARD_G3,CARD_G3_PT)
#write_xlsx(myocarditis,"D:\\safety_original\\myocarditis.xlsx")

#Import data
myocarditis <- read_excel("D:/Conditional_poisson/myocarditis.xlsx")

# create loginterval (in this case, logPERSON TIME)
myocarditis <- myocarditis %>% mutate(logPERSONTIME = log(PERSONTIME))

#count 
myocarditis1 <- myocarditis %>% filter(EXPOSURE == "1")
myocarditis2 <- myocarditis %>% filter(EXPOSURE == "0")
dplyr::count(myocarditis1,EVENTS, sort = TRUE)
dplyr::count(myocarditis2,EVENTS, sort = TRUE)
sum(myocarditis1$PERSONTIME)
sum(myocarditis2$PERSONTIME)

#fit a conditional poisson regression model with eliminate = indiv
gnm.RR(myocarditis_model <- gnm(EVENTS ~ EXPOSURE, data=myocarditis, 
                                offset = logPERSONTIME,
                                family = poisson(),
                                eliminate=factor(MASTER_ENC)))
#survival fit model
myocarditis_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = myocarditis)
summary(myocarditis_model1)

#Create data set for cardiomyopathy
#cardiomyopathy <- df %>% filter (CARD_G4 == 1) %>% select(MASTER_ENC,GENDER,AGE_DIS,EXPOSURE,CARD_G4,CARD_G4_PT)
#write_xlsx(cardiomyopathy,"D:\\safety_original\\cardiomyopathy.xlsx")

#Import data
cardiomyopathy <- read_excel("D:/Conditional_poisson/cardiomyopathy.xlsx")

# create loginterval (in this case, logPERSON TIME)
cardiomyopathy <- cardiomyopathy %>% mutate(logPERSONTIME = log(PERSONTIME))

#count 
cardiomyopathy1 <- cardiomyopathy %>% filter(EXPOSURE == "1")
cardiomyopathy2 <- cardiomyopathy %>% filter(EXPOSURE == "0")
dplyr::count(cardiomyopathy1,EVENTS, sort = TRUE)
dplyr::count(cardiomyopathy2,EVENTS, sort = TRUE)
sum(cardiomyopathy1$PERSONTIME)
sum(cardiomyopathy2$PERSONTIME)


#fit a conditional poisson regression model with eliminate = indiv
gnm.RR(cardiomyopathy_model <- gnm(EVENTS ~ EXPOSURE, data=cardiomyopathy, 
                                   offset = logPERSONTIME,
                                   family = poisson(),
                                   eliminate=factor(MASTER_ENC)))
#survival fit model
cardiomyopathy_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = cardiomyopathy)
summary(cardiomyopathy_model1)

#Create data set for heart failure 
#hfailure <- df %>% filter (CARD_G5 == 1) %>% select(MASTER_ENC,GENDER,AGE_DIS,EXPOSURE,CARD_G5,CARD_G5_PT)
#write_xlsx(hfailure,"D:\\safety_original\\hfailure.xlsx")

#Import data
hfailure <- read_excel("D:/Conditional_poisson/hfailure.xlsx")

# create loginterval (in this case, logPERSON TIME)
hfailure <- hfailure %>% mutate(logPERSONTIME = log(PERSONTIME))

#count 
hfailure1 <- hfailure %>% filter(EXPOSURE == "1")
hfailure2 <- hfailure %>% filter(EXPOSURE == "0")
dplyr::count(hfailure1,EVENTS, sort = TRUE)
dplyr::count(hfailure2,EVENTS, sort = TRUE)
sum(hfailure1$PERSONTIME)
sum(hfailure2$PERSONTIME)


#fit a conditional poisson regression model with eliminate = indiv
gnm.RR(hfailure_model <- gnm(EVENTS ~ EXPOSURE, data=hfailure, 
                             offset = logPERSONTIME,
                             family = poisson(),
                             eliminate=factor(MASTER_ENC)))
#survival fit model
hfailure_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = hfailure)
summary(hfailure_model1)

#Carduivascular diseases
CARD <- rbind(aminfarction,pericarditis,myocarditis,cardiomyopathy,hfailure)

CARD1 <- CARD %>% filter(EXPOSURE == "1")
CARD2 <- CARD %>% filter(EXPOSURE == "0")
dplyr::count(CARD1,EVENTS, sort = TRUE)
dplyr::count(CARD2,EVENTS, sort = TRUE)
sum(CARD1$PERSONTIME)
sum(CARD2$PERSONTIME)


#fit a conditional poisson regression model with eliminate = indiv
gnm.RR(CARD_model <- gnm(EVENTS ~ EXPOSURE, data=CARD, 
                         offset = logPERSONTIME,
                         family = poisson(),
                         eliminate=factor(MASTER_ENC)))
#survival fit model
CARD_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = CARD)
summary(CARD_model1)

#Create data set for MENIGITIS
#No hospitalisation occurred during the study period


#Create data set for Ramsey-Hunt Syndrome and Bell's Palsy
#palsy <- df %>% filter (RAMS == 1) %>% select(MASTER_ENC,GENDER,AGE_DIS,EXPOSURE,RAMS,RAMS_PT)
#write_xlsx(palsy1,"D:\\safety_original\\palsy1.xlsx")

#Import data
palsy <- read_excel("D:/Conditional_poisson/palsy.xlsx")

# create loginterval (in this case, logPERSON TIME)
palsy <- palsy %>% mutate(logPERSONTIME = log(PERSONTIME))

#count
palsy1 <- palsy %>% filter(EXPOSURE == "1")
palsy2 <- palsy %>% filter(EXPOSURE == "0")
dplyr::count(palsy1,EVENTS, sort = TRUE)
dplyr::count(palsy2,EVENTS, sort = TRUE)
sum(palsy1$PERSONTIME)
sum(palsy2$PERSONTIME)

#fit a conditional poisson regression model with eliminate = indiv
gnm.RR(palsy_model <- gnm(EVENTS ~ EXPOSURE, data=palsy, 
                          offset = logPERSONTIME,
                          family = poisson(),
                          eliminate=factor(MASTER_ENC)))
#survival fit model
palsy_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = palsy)
summary(palsy_model1)


#Create data set for Cellulitis and infections
#infections <- df %>% filter (MEDI_G1 == 1) %>% select(MASTER_ENC,GENDER,AGE_DIS,EXPOSURE,MEDI_G1,MEDI_G1_PT)
#write_xlsx(infections,"D:\\safety_original\\infections.xlsx")

#Import data
#infections <- read_excel("D:/safety_original/infections.xlsx")

# create loginterval (in this case, logPERSON TIME)
#infections <- infections %>% mutate(logPERSONTIME = log(PERSONTIME))

#fit a conditional poisson regression model with eliminate = indiv
#gnm.RR(infections_model <- gnm(EVENTS ~ EXPOSURE, data=infections, 
#offset = logPERSONTIME,
#family = poisson(),
#eliminate=factor(MASTER_ENC)))
#survival fit model
#infections_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = infections)
#summary(infections_model1)


#Create data set for Allergic reactions
#allergicrxns <- df %>% filter (MEDI_G2 == 1) %>% select(MASTER_ENC,GENDER,AGE_DIS,EXPOSURE,MEDI_G2,MEDI_G2_PT)
#write_xlsx(allergicrxns,"D:\\safety_original\\allergicrxns.xlsx")

#Import data
#allergicrxns <- read_excel("D:/safety_original/allergicrxns.xlsx")

# create loginterval (in this case, logPERSON TIME)
#allergicrxns <- allergicrxns %>% mutate(logPERSONTIME = log(PERSONTIME))

#fit a conditional poisson regression model with eliminate = indiv
#gnm.RR(allergicrxns_model <- gnm(EVENTS ~ EXPOSURE, data=allergicrxns, 
#offset = logPERSONTIME,
#family = poisson(),
#eliminate=factor(MASTER_ENC)))
#survival fit model
#allergicrxns_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = allergicrxns)
#summary(allergicrxns_model1)

#Total
data <- rbind(tci,stroke,aminfarction,pericarditis,myocarditis,cardiomyopathy,hfailure,palsy)
View(data)


#count
data1 <- data %>% filter(EXPOSURE == "1")
data2 <- data %>% filter(EXPOSURE == "0")
dplyr::count(data1,EVENTS, sort = TRUE)
dplyr::count(data2,EVENTS, sort = TRUE)
sum(data1$PERSONTIME)
sum(data2$PERSONTIME)

#fit a conditional poisson regression model with eliminate = indiv
gnm.RR(data_model <- gnm(EVENTS ~ EXPOSURE, data=data, 
                         offset = logPERSONTIME,
                         family = poisson(),
                         eliminate=factor(MASTER_ENC)))
#survival fit model
data_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = data)
summary(data_model1)



### Sensitivity analysis
dfh <- df %>% filter(EXPOSURE == "1")
dfc <- df %>% filter(EXPOSURE == "0")

dplyr::count(dfh,WHOC, sort = TRUE)
dplyr::count(dfh,APPE, sort = TRUE)
dplyr::count(dfh,HERN, sort = TRUE)
dplyr::count(dfh,DIVE, sort = TRUE)
dplyr::count(dfh,FEMF, sort = TRUE)
dplyr::count(dfh,CHOC, sort = TRUE)
dplyr::count(dfh,PAND, sort = TRUE)
dplyr::count(dfh,SEPS, sort = TRUE)
dplyr::count(dfh,HAEM, sort = TRUE)
dplyr::count(dfh,RENC, sort = TRUE)
dplyr::count(dfh,BURN, sort = TRUE)
dplyr::count(dfh,LIPO, sort = TRUE)
dplyr::count(dfh,EPIS, sort = TRUE)


dplyr::count(dfc,WHOC, sort = TRUE)
dplyr::count(dfc,APPE, sort = TRUE)
dplyr::count(dfc,HERN, sort = TRUE)
dplyr::count(dfc,DIVE, sort = TRUE)
dplyr::count(dfc,FEMF, sort = TRUE)
dplyr::count(dfc,CHOC, sort = TRUE)
dplyr::count(dfc,PAND, sort = TRUE)
dplyr::count(dfc,SEPS, sort = TRUE)
dplyr::count(dfc,HAEM, sort = TRUE)
dplyr::count(dfc,RENC, sort = TRUE)
dplyr::count(dfc,BURN, sort = TRUE)
dplyr::count(dfc,LIPO, sort = TRUE)
dplyr::count(dfc,EPIS, sort = TRUE)

####controls
#Create data set for diverticulitis
#dfdive <- df %>% filter (df$DIVE == 1) %>% select(MASTER_ENC,GENDER,AGE_DIS,EXPOSURE,DIVE,DIVE_PT)
#write_xlsx(dfdive,"D:\\safety_original\\dfdive.xlsx")
dfdive <- read_excel("D:/safety_original/dfdive.xlsx")
# create loginterval (in this case, logPERSON TIME)
dfdive <- dfdive %>% mutate(logPERSONTIME = log(PERSONTIME))
View(dfdive)
dfdive %>% tabyl(EXPOSURE, PERSONTIME) %>%
  adorn_totals(where = "col")

dfdive1 <- dfdive %>% filter(EXPOSURE == "1")
dfdive2 <- dfdive %>% filter(EXPOSURE == "0")
dplyr::count(dfdive1,EVENTS, sort = TRUE)
dplyr::count(dfdive2,EVENTS, sort = TRUE)
sum(dfdive1$PERSONTIME)
sum(dfdive2$PERSONTIME)


#fit a conditional poisson regression model with eliminate = indiv
gnm.RR(dfdive_model <- gnm(EVENTS ~ EXPOSURE, data=dfdive, 
                           offset = logPERSONTIME,
                           family = poisson(),
                           eliminate=factor(MASTER_ENC)))
#survival fit model
dfdive_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = dfdive)
summary(dfdive_model1)


#Create data set for Femural fractures
#dffem <- df %>% filter (FEMF == 1) %>% select(MASTER_ENC,GENDER,AGE_DIS,EXPOSURE,FEMF,FEMF_PT)
#write_xlsx(dffem,"D:\\safety_original\\dffem.xlsx")
dffem <- read_excel("D:/safety_original/dffem.xlsx")
# create loginterval (in this case, logPERSON TIME)
dffem <- dffem %>% mutate(logPERSONTIME = log(PERSONTIME))
View(dffem)
dffem1 <- dffem %>% filter(EXPOSURE == "1")
dffem2 <- dffem %>% filter(EXPOSURE == "0")
dplyr::count(dffem1,EVENTS, sort = TRUE)
dplyr::count(dffem2,EVENTS, sort = TRUE)
sum(dffem1$PERSONTIME)
sum(dffem2$PERSONTIME)

#fit a conditional poisson regression model with eliminate = indiv
gnm.RR(dffem_model <- gnm(EVENTS ~ EXPOSURE, data=dffem, 
                          offset = logPERSONTIME,
                          family = poisson(),
                          eliminate=factor(MASTER_ENC)))
#survival fit model
dffem_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = dffem)
summary(dffem_model1)

#Create data set for cholecystits and pancreatitis
#dfchoc <- df %>% filter (CHOC == 1) %>% select(MASTER_ENC,GENDER,AGE_DIS,EXPOSURE,CHOC,CHOC_PT)
#write_xlsx(dfchoc,"D:\\safety_original\\dfchoc.xlsx")
dfpand <- df %>% filter (PAND == 1) %>% select(MASTER_ENC,GENDER,AGE_DIS,EXPOSURE,PAND,PAND_PT)
write_xlsx(dfpand,"D:\\safety_original\\dfpand.xlsx")


dfcp <- read_excel("D:/safety_original/dfchoc_pand.xlsx")
# create loginterval (in this case, logPERSON TIME)
dfcp <- dfcp %>% mutate(logPERSONTIME = log(PERSONTIME))

dfcp1 <- dfcp %>% filter(EXPOSURE == "1")
dfcp2 <- dfcp %>% filter(EXPOSURE == "0")
dplyr::count(dfcp1,EVENTS, sort = TRUE)
dplyr::count(dfcp2,EVENTS, sort = TRUE)
sum(dfcp1$PERSONTIME)
sum(dfcp2$PERSONTIME)

#fit a conditional poisson regression model with eliminate = indiv
gnm.RR(dfcp_model <- gnm(EVENTS ~ EXPOSURE, data=dfcp, 
                         offset = logPERSONTIME,
                         family = poisson(),
                         eliminate=factor(MASTER_ENC)))
#survival fit model
dfcp_model1 <- clogit(EVENTS ~ EXPOSURE + strata(MASTER_ENC) + offset(logPERSONTIME), data = dfcp)
summary(dfcp_model1)


#Figure for sensitivity analysis
library(data.table)
library(ggplot2)
library(ggrepel)

sensitivity <- data.table(var = c(rep("Sensitivity analysis",3)),
                          group = factor(c("Diverticulitis", "Femoral fracture", 
                                           "Cholecyst & pancreatic diseases")),
                          RR = c(0.65,0.95,4.38),
                          LQ = c(0.08, 0.30,0.99),
                          UQ = c(3.78, 2.70, 22.15))

ggplot(sensitivity, aes(RR, factor(group))) +
  geom_vline(aes(xintercept = 1), col = "red", lty=2, lwd=1)+
  geom_errorbarh(aes(xmax = UQ, xmin = LQ), size = 0.7, height = 0.2)+
  geom_point(aes(col="red",  size = 0.1)) + 
  theme(strip.text.y = element_text(angle = 0))+
  labs(title = "",
       x = "Rate Ratio",
       y = "Indicators")+
  theme_bw()+
  theme(legend.position = "none")

###END






























































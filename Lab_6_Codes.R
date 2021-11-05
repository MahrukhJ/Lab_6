Mahrukh Jaura
Lab_6

attach(acs2017_ny)
model_v1 <- lm(INCWAGE ~ AGE)
detach()

model_v2 <- lm(acs2017_ny$INCWAGE ~ acs2017_ny$AGE)

summary(model_v2)
'Call:
  lm(formula = acs2017_ny$INCWAGE ~ acs2017_ny$AGE)

Residuals:
  Min     1Q Median     3Q    Max 
-39492 -31602 -25429  12541 612359 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)    42296.810    439.916   96.15   <2e-16 ***
  acs2017_ny$AGE  -175.328      8.422  -20.82   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 66080 on 163156 degrees of freedom
(33427 observations deleted due to missingness)
Multiple R-squared:  0.002649,	Adjusted R-squared:  0.002643 
F-statistic: 433.4 on 1 and 163156 DF,  p-value: < 2.2e-16'

model_v3 <- lm(INCWAGE ~ AGE, data = acs2017_ny)

attach(Household_Pulse_data)

#This week we move to logit and probit models. These are suited for when the dependent y variable takes values of just 0 or 1.
#A logit regression is used to model dichotomous outcome variables. In the logit model the log odds of the outcome is modeled as a linear combination of the predictor variables.
#creating the data to be used in the models
Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA") 

model_logit1 <- glm(vaxx ~ EEDUC,
                    family = binomial, data = Household_Pulse_data)
summary(model_logit1)

'Call:
  glm(formula = vaxx ~ EEDUC, family = binomial, data = Household_Pulse_data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.4785   0.3081   0.4054   0.6029   0.8312  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)      0.92495    0.11020   8.393  < 2e-16 ***
  EEDUCsome hs    -0.03962    0.13189  -0.300  0.76390    
EEDUCHS diploma  0.38389    0.11364   3.378  0.00073 ***
  EEDUCsome coll   0.68799    0.11245   6.118 9.46e-10 ***
  EEDUCassoc deg   0.78620    0.11483   6.846 7.57e-12 ***
  EEDUCbach deg    1.53254    0.11330  13.526  < 2e-16 ***
  EEDUCadv deg     2.09910    0.11590  18.111  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 49071  on 68262  degrees of freedom
Residual deviance: 46446  on 68256  degrees of freedom
(851 observations deleted due to missingness)
AIC: 46460

Number of Fisher Scoring iterations: 5'

#observing the fraction vaxxed through education levels
table(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC)

'less than hs some hs HS diploma some coll assoc deg bach deg adv deg
  FALSE          115     269       1647      2396      1132     1565     813
  TRUE           290     652       6097     12022      6266    18272   16727'

#comparing summary() vs summary(as.numeric())
summary(Household_Pulse_data$vaxx)
' Mode   FALSE    TRUE    NAs 
logical    7937   60326     851'
summary(as.numeric(Household_Pulse_data$vaxx))
'Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NAs 
0.0000  1.0000  1.0000  0.8837  1.0000  1.0000   851'

#Assigning labels to the factor
vaxx_factor <- as.factor(Household_Pulse_data$vaxx)
levels(vaxx_factor) <- c("not vaxxed","yes are vaxxed")
summary(vaxx_factor)
'not vaxxed   yes are vaxxed   NAs 
7937          60326            851'

#creating a subset of people in the Northeast region
pick_use1 <- (Household_Pulse_data$REGION == "Northeast") 
# just for example!
dat_use1 <- subset(Household_Pulse_data, pick_use1)

# and to be finicky, might want to use this for factors after subsetting in case some get lost
dat_use1$RECVDVACC <- droplevels(dat_use1$RECVDVACC) 

#model_logit2 will observe vaccination rate as the dependent variable and education level, marital status, race, Hispanic and gender as the independent variables. 
model_logit2 <- glm(vaxx ~ EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE,
                    family = binomial, data = dat_use1)
#I had to remove 'TBIRTH_YEAR' because there was no category of age or birth year in Household_Pulse_data or Household_Pulse_data_v2.
summary(model_logit2)

'Call:
glm(formula = vaxx ~ EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE, 
    family = binomial, data = dat_use1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.9241   0.2398   0.3022   0.4535   1.4522  

Coefficients:
                          Estimate Std. Error z value Pr(>|z|)    
(Intercept)                0.47171    0.58016   0.813 0.416179    
EEDUCsome hs               0.94998    0.41274   2.302 0.021355 *  
EEDUCHS diploma            0.80062    0.33752   2.372 0.017689 *  
EEDUCsome coll             1.06983    0.33660   3.178 0.001481 ** 
EEDUCassoc deg             1.10661    0.34361   3.221 0.001280 ** 
EEDUCbach deg              1.91062    0.33928   5.631 1.79e-08 ***
EEDUCadv deg               2.38182    0.34508   6.902 5.12e-12 ***
MSmarried                  0.41594    0.39673   1.048 0.294436    
MSwidowed                  0.59912    0.42887   1.397 0.162421    
MSdivorced                 0.12307    0.40274   0.306 0.759930    
MSseparated                0.46563    0.47287   0.985 0.324777    
MSnever                   -0.14637    0.39884  -0.367 0.713622    
RRACEBlack                -0.43067    0.12289  -3.504 0.000458 ***
RRACEAsian                 1.01932    0.30006   3.397 0.000681 ***
RRACEOther                -0.85249    0.15723  -5.422 5.90e-08 ***
RHISPANICHispanic         -0.30383    0.11928  -2.547 0.010861 *  
GENID_DESCRIBEmale         0.43126    0.34185   1.262 0.207114    
GENID_DESCRIBEfemale       0.26524    0.33935   0.782 0.434445    
GENID_DESCRIBEtransgender -0.09887    0.60939  -0.162 0.871115    
GENID_DESCRIBEother       -0.70843    0.41503  -1.707 0.087836 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 5772.2  on 10359  degrees of freedom
Residual deviance: 5306.2  on 10340  degrees of freedom
  (118 observations deleted due to missingness)
AIC: 5346.2

Number of Fisher Scoring iterations: 6'


#Adding more variables to make the model further interesting!
#In this model, along with the variables from model_logit2, I thought it would be interesting to include people with kids under the age 5, people with kids aged 5-11, 
#people with kids 12-17 and where they live. Identical to the model_logit2, the data used consists of people who reside in the Northeast region.

model_logit3 <- glm(vaxx ~ EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE + KIDS_LT5Y + KIDS_5_11Y + KIDS_12_17Y +LIVQTRRV,
                    family = binomial, data = dat_use1)
summary(model_logit3)

'Call:
glm(formula = vaxx ~ EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE + 
    KIDS_LT5Y + KIDS_5_11Y + KIDS_12_17Y + LIVQTRRV, family = binomial, 
    data = dat_use1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.1705   0.2240   0.3158   0.4268   1.6311  

Coefficients:
                                             Estimate Std. Error z value Pr(>|z|)    
(Intercept)                                  0.544631   0.597846   0.911  0.36230    
EEDUCsome hs                                 0.778113   0.437771   1.777  0.07550 .  
EEDUCHS diploma                              0.432059   0.359249   1.203  0.22910    
EEDUCsome coll                               0.664994   0.358433   1.855  0.06356 .  
EEDUCassoc deg                               0.686879   0.365424   1.880  0.06015 .  
EEDUCbach deg                                1.444970   0.361426   3.998 6.39e-05 ***
EEDUCadv deg                                 1.926579   0.367158   5.247 1.54e-07 ***
MSmarried                                    0.478274   0.409374   1.168  0.24268    
MSwidowed                                    0.403572   0.441576   0.914  0.36075    
MSdivorced                                   0.001748   0.415180   0.004  0.99664    
MSseparated                                  0.538857   0.490622   1.098  0.27207    
MSnever                                     -0.220971   0.411310  -0.537  0.59110    
RRACEBlack                                  -0.261071   0.128998  -2.024  0.04299 *  
RRACEAsian                                   1.206285   0.306638   3.934 8.36e-05 ***
RRACEOther                                  -0.707575   0.164505  -4.301 1.70e-05 ***
RHISPANICHispanic                           -0.133328   0.125482  -1.063  0.28799    
GENID_DESCRIBEmale                           0.519067   0.348654   1.489  0.13655    
GENID_DESCRIBEfemale                         0.431232   0.346519   1.244  0.21333    
GENID_DESCRIBEtransgender                    0.141923   0.640073   0.222  0.82452    
GENID_DESCRIBEother                         -0.511139   0.427452  -1.196  0.23178    
KIDS_LT5YYes children under 5 in HH         -0.695774   0.116310  -5.982 2.20e-09 ***
KIDS_5_11YYes children 5 - 11 in HH         -0.774538   0.098741  -7.844 4.36e-15 ***
KIDS_12_17YYes children 12 - 17 in HH       -0.509680   0.096106  -5.303 1.14e-07 ***
LIVQTRRVlive in mobile home                 -0.083225   0.236285  -0.352  0.72467    
LIVQTRRVlive in detached 1 family            0.685428   0.096376   7.112 1.14e-12 ***
LIVQTRRVlive in 1 family attached to others  0.615383   0.155676   3.953 7.72e-05 ***
LIVQTRRVlive in bldg w 2 apartments          0.496514   0.187757   2.644  0.00818 ** 
LIVQTRRVlive in building with 3-4 apts       0.362972   0.188919   1.921  0.05469 .  
LIVQTRRVlive in bldg w 5+ apts               0.826141   0.150361   5.494 3.92e-08 ***
LIVQTRRVlive in boat, RV, etc               -1.024148   0.426604  -2.401  0.01636 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 5772.2  on 10359  degrees of freedom
Residual deviance: 5049.7  on 10330  degrees of freedom
  (118 observations deleted due to missingness)
AIC: 5109.7

Number of Fisher Scoring iterations: 6'

#In model_logit3, EEDUCbach deg, EEDUCadv deg, RaceAsion, RaceOther, KIDS_LT5YYes children under 5 in HH, KIDS_5_11YYes children 5 - 11 in HH, KIDS_12_17YYes children 12 - 17 in HH,
#LIVQTRRVlive in detached 1 family, LIVQTRRVlive in 1 family attached to others and LIVQTRRVlive in bldg w 5+ apts are all very statistically significant. Specifically, EEDUCbach deg, EEDUCadv deg,
#RACEAsian, LIVQTRRVlive in detached 1 family, LIVQTRRVlive in 1 family attached to others and LIVQTRRVlive in bldg w 5+ apts have coefficient estimates of 1.444970, 1.926579, 1.206285, 0.685428,
#0.615383 and 0.826141, respectively. That indicates that people in these categories are more likely to get vaccinated whereas negative coefficient estimates that are statistically significant imply that
#those people are not likely to get vaccinated. 

#I'm going to create a different subset and observe the differences between the models when the variables are identical. 
#This subset will consist of people who have public health insurance and reside in the West. 
dat_use2 <- subset(Household_Pulse_data, (Household_Pulse_data$PUBHLTH == "has public health ins") & (Household_Pulse_data$REGION == "West"))

model_logit4 <- glm(vaxx ~ EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE,
                    family = binomial, data = dat_use2)
summary(model_logit4)
'Call:
glm(formula = vaxx ~ EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE, 
    family = binomial, data = dat_use2)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.9030   0.2336   0.3406   0.4711   1.1294  

Coefficients:
                          Estimate Std. Error z value Pr(>|z|)    
(Intercept)                1.64143    0.85133   1.928 0.053846 .  
EEDUCsome hs              -0.29884    0.43542  -0.686 0.492511    
EEDUCHS diploma            0.01014    0.38452   0.026 0.978966    
EEDUCsome coll             0.40733    0.37999   1.072 0.283745    
EEDUCassoc deg             0.50044    0.38959   1.285 0.198950    
EEDUCbach deg              1.14880    0.38742   2.965 0.003024 ** 
EEDUCadv deg               1.80152    0.39920   4.513  6.4e-06 ***
MSmarried                  0.64944    0.63368   1.025 0.305420    
MSwidowed                  0.77165    0.64639   1.194 0.232560    
MSdivorced                 0.35054    0.63664   0.551 0.581905    
MSseparated               -0.39675    0.66400  -0.598 0.550161    
MSnever                    0.03146    0.63759   0.049 0.960643    
RRACEBlack                -0.31185    0.19861  -1.570 0.116374    
RRACEAsian                 1.08160    0.29928   3.614 0.000302 ***
RRACEOther                -0.49322    0.13262  -3.719 0.000200 ***
RHISPANICHispanic         -0.19769    0.11819  -1.673 0.094397 .  
GENID_DESCRIBEmale        -0.32236    0.53136  -0.607 0.544064    
GENID_DESCRIBEfemale      -0.50488    0.52953  -0.953 0.340360    
GENID_DESCRIBEtransgender  0.46430    1.17612   0.395 0.693010    
GENID_DESCRIBEother       -0.32992    0.63039  -0.523 0.600729    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 4589.0  on 7723  degrees of freedom
Residual deviance: 4232.5  on 7704  degrees of freedom
  (26 observations deleted due to missingness)
AIC: 4272.5

Number of Fisher Scoring iterations: 6'

#Comparing model_logit2 which engaged dat_use1 (a subset of people in the Northeast region) and model_logit4 which utilized dat_use2 
#(people who have public health insurance and reside in the West), one of the main differences that I noted was the difference in the quantity 
#of significant values. The dependent and the independent variables were identical for both models (the independent variable being 'vaxx' and the
#dependent variable being EEDUC, MS, RRACE, RHISPANIC and GENID_DESCRIBE). Given this, in model_logit2, EEDUCsome hs, EEDUCHS diploma and RHISPANICHispanic 
#are categorized as being significant at the 0.05 level, EEDUCsome coll and EEDUCassoc deg are significant at 0.01, EEDUCbach deg, EEDUCadv deg, RRACEBlack 
#RRACEAsian and RRACEOther are significant at 0.001. In contrast, for model_logit4, EEDUCbach deg is significant at 0.01 and EEDUCadv deg, RRACEAsian and RRACEOther
#are significant at 0.001. To analyze, in model_logit2, someone with a bachelors degree and someone with an advanced degree are more likely to be vaccinated than someone
#without a bachelors degree or without an advanced degree since the estimate for EEDUCbach deg is 1.91062 and the estimate for EEDUCadv deg is 2.38182. In contrast, RACEOther 
#has an estimate value of -0.85249 which indicates that people in this category are very likely to not get vaccinated. 

I attempted to predict values from model_logit2, however, an error was found where variable lengths differed for 'RHISPANIC' which is why the following model omits RHISPANIC. 

model_logit2_again <- glm(vaxx ~ EEDUC + MS + RRACE + GENID_DESCRIBE,
                          family = binomial, data = dat_use1)
summary(model_logit2_again)

#I will predict the probability of a White woman who is married and has a bachelors degree with the folowing code. 
to_be_predicted_1 <- data.frame(EEDUC = "bach deg", MS = "married", RRACE = "White", GENID_DESCRIBE = "female", data = dat_use1)
to_be_predicted_1$yhat <- predict(model_logit2_again, to_be_predicted_1, type = "response")
summary(to_be_predicted_1$yhat)
'Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.9547  0.9547  0.9547  0.9547  0.9547  0.9547'

To analyze the results, the probability of a white, married woman who has a bachelors degree in the subset of people in the Northeast is 95.47. 
A very high probability which honestly! I am interested in observing the difference between the code output if EEDUC was equal to "adv deg" instead of "bach deg".

to_be_predicted1 <- data.frame(EEDUC = "adv deg", MS = "married", RRACE = "White", GENID_DESCRIBE = "female", data = dat_use1)
to_be_predicted1$yhat <- predict(model_logit2_again, to_be_predicted1, type = "response")
summary(to_be_predicted1$yhat)

'Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.9712  0.9712  0.9712  0.9712  0.9712  0.9712'
The probability increases to 97.12! 
More specifically, a white, married woman who lives in the Northeast region and has obtained an adanced degree has a predicted probability of 97.12 percent of getting vaccinated. 

Attempting to predict the probability of a divorced Black male with a HS diploma in a subset of people residing in the West who have public health insurance.
model_logit4 <- glm(vaxx ~ EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE,
                    family = binomial, data = dat_use2)
to_be_predicted3 <- data.frame(EEDUC = "HS diploma", MS = "divorced", RRACE = "Black", GENID_DESCRIBE = "male", data = dat_use2)
to_be_predicted3$yhat <- predict(model_logit2_again, to_be_predicted3, type = "response")
summary(to_be_predicted3$yhat)
'Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.7942  0.7942  0.7942  0.7942  0.7942  0.7942' 

The probability of a divorced Black male with a HS diploma in a subset of people residing in the West and who have public health insurance getting vaccinated is 79.42. 
A probability prediction significantly less than the predictions generated from to_be_predicted_1 and to_be_predicted1. 

#Moving on to probit models!
A probit regression is used to model binary outcome variables where the inverse standard normal distribution of the probability is modeled as a linear combination of the predictors.
The code below estimates a probit regression model using the generalized linear model(glm) function. The dependent and independent variables are almost identical to model_logit2_again,
the difference being the addition of INCOME as an independent variable in model_probit_1. 

model_probit_1 <- glm(vaxx ~ EEDUC + MS + RRACE + GENID_DESCRIBE + INCOME, family = binomial(link = 'probit'), data = dat_use1)
summary(model_probit_1)

"Call:
glm(formula = vaxx ~ EEDUC + MS + RRACE + GENID_DESCRIBE + INCOME, 
    family = binomial(link = "probit"), data = dat_use1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.1431   0.2102   0.3197   0.4465   1.4822  

Coefficients:
                               Estimate Std. Error z value Pr(>|z|)    
(Intercept)                     0.25214    0.31548   0.799  0.42417    
EEDUCsome hs                    0.57859    0.23937   2.417  0.01565 *  
EEDUCHS diploma                 0.46279    0.19973   2.317  0.02050 *  
EEDUCsome coll                  0.58001    0.19870   2.919  0.00351 ** 
EEDUCassoc deg                  0.58365    0.20195   2.890  0.00385 ** 
EEDUCbach deg                   0.93569    0.19913   4.699 2.62e-06 ***
EEDUCadv deg                    1.10878    0.20102   5.516 3.47e-08 ***
MSmarried                       0.09239    0.21081   0.438  0.66119    
MSwidowed                       0.24316    0.22581   1.077  0.28157    
MSdivorced                      0.01912    0.21410   0.089  0.92882    
MSseparated                     0.14016    0.24901   0.563  0.57353    
MSnever                        -0.11207    0.21226  -0.528  0.59751    
RRACEBlack                     -0.19322    0.06644  -2.908  0.00364 ** 
RRACEAsian                      0.53631    0.13351   4.017 5.89e-05 ***
RRACEOther                     -0.46770    0.08812  -5.307 1.11e-07 ***
GENID_DESCRIBEmale              0.15612    0.17898   0.872  0.38305    
GENID_DESCRIBEfemale            0.10089    0.17774   0.568  0.57027    
GENID_DESCRIBEtransgender      -0.10301    0.33608  -0.306  0.75923    
GENID_DESCRIBEother            -0.44786    0.22589  -1.983  0.04740 *  
INCOMEHH income less than $25k  0.23514    0.07695   3.056  0.00225 ** 
INCOMEHH income $25k - $34.9k   0.29681    0.07197   4.124 3.73e-05 ***
INCOMEHH income $35k - 49.9     0.34846    0.06136   5.679 1.36e-08 ***
INCOMEHH income $50k - 74.9     0.35820    0.06936   5.164 2.41e-07 ***
INCOMEHH income $75 - 99.9      0.47664    0.06749   7.063 1.63e-12 ***
INCOMEHH income $100k - 149     0.49565    0.09218   5.377 7.58e-08 ***
INCOMEHH income $150 - 199      0.63587    0.09430   6.743 1.55e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 5772.2  on 10359  degrees of freedom
Residual deviance: 5201.0  on 10334  degrees of freedom
  (118 observations deleted due to missingness)
AIC: 5253

Number of Fisher Scoring iterations: 6"

In this model, the estimate for EEDUCbach deg, EEDUCadv deg, RRACEAsian, RRACEOther, INCOMEHH income $25k - $34.9k, INCOMEHH income $35k - 49.9, INCOMEHH income $50k - 74.9,
INCOMEHH income $75 - 99.9, INCOMEHH income $100k - 149, INCOMEHH income $150 - 199 are all very statistically significant. 
To analyze, the estimate for EEDUCbach deg is 0.93569, which indicates a high liklihood that someone with a bachelors degree will get vaccinated in a subset of people from the Northeast. 
Likewise, the coefficient estimate for INCOMEHH income $25k - $34.9k, INCOMEHH income $35k - 49.9, INCOMEHH income $50k - 74.9, INCOMEHH income $75 - 99.9, INCOMEHH income $100k - 149 and
INCOMEHH income $150 - 199 are all positive and also demosntrate a strong liklihood of receiving vaccinations. RRACEOther, however, has a correlation coefficient of -0.46770 which is also 
statistically significant and indicates a strong liklihood not to get vaccinated. 

We can use the confint function to obtain confidence intervals for the coefficient estimates.
confint(model_probit_1)

"Waiting for profiling to be done...
                                    2.5 %      97.5 %
(Intercept)                    -0.35097028  0.87967858
EEDUCsome hs                    0.11089420  1.04479990
EEDUCHS diploma                 0.06953539  0.84730316
EEDUCsome coll                  0.18873357  0.96245480
EEDUCassoc deg                  0.18629313  0.97254160
EEDUCbach deg                   0.54385817  1.31872668
EEDUCadv deg                    0.71324456  1.49577525
MSmarried                      -0.33980631  0.48959554
MSwidowed                      -0.21561883  0.67266231
MSdivorced                     -0.41874580  0.42325295
MSseparated                    -0.35887610  0.61900156
MSnever                        -0.54682253  0.28832271
RRACEBlack                     -0.32229882 -0.06155511
RRACEAsian                      0.28627671  0.81417513
RRACEOther                     -0.63770102 -0.29412329
GENID_DESCRIBEmale             -0.20524401  0.49439228
GENID_DESCRIBEfemale           -0.25824740  0.43658739
GENID_DESCRIBEtransgender      -0.74523388  0.56412518
GENID_DESCRIBEother            -0.89411296 -0.01240076
INCOMEHH income less than $25k  0.08639322  0.38753334
INCOMEHH income $25k - $34.9k   0.15735933  0.43961601
INCOMEHH income $35k - 49.9     0.22926084  0.46985613
INCOMEHH income $50k - 74.9     0.22382377  0.49597839
INCOMEHH income $75 - 99.9      0.34592778  0.61057492
INCOMEHH income $100k - 149     0.31930161  0.68072954
INCOMEHH income $150 - 199      0.45563980  0.82646728"

From this model, I will predict the probabilities of a divorced Black male with a bachelors degree in a subset of people residing in the Northeast, with varying degrees of income in order to observe any differences. 
to_be_predicted4 <- data.frame(EEDUC = "bach deg", MS = "divorced", RRACE = "Black", GENID_DESCRIBE = "male",INCOME= "HH income less than $25k", data = dat_use1)
to_be_predicted4$yhat <- predict(model_probit_1, to_be_predicted4, type = "response")
summary(to_be_predicted4$yhat)
'Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.92    0.92    0.92    0.92    0.92    0.92'
to_be_predicted5 <- data.frame(EEDUC = "bach deg", MS = "divorced", RRACE = "Black", GENID_DESCRIBE = "male",INCOME= "HH income $25k - $34.9k", data = dat_use1)
to_be_predicted5$yhat <- predict(model_probit_1, to_be_predicted5, type = "response")
summary(to_be_predicted5$yhat)
"Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.9288  0.9288  0.9288  0.9288  0.9288  0.9288"
to_be_predicted6 <- data.frame(EEDUC = "bach deg", MS = "divorced", RRACE = "Black", GENID_DESCRIBE = "male",INCOME= "HH income $35k - 49.9", data = dat_use1)
to_be_predicted6$yhat <- predict(model_probit_1, to_be_predicted6, type = "response")
summary(to_be_predicted6$yhat)
"Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.9355  0.9355  0.9355  0.9355  0.9355  0.9355"
to_be_predicted7 <- data.frame(EEDUC = "bach deg", MS = "divorced", RRACE = "Black", GENID_DESCRIBE = "male",INCOME= "HH income $75 - 99.9", data = dat_use1)
to_be_predicted7$yhat <- predict(model_probit_1, to_be_predicted7, type = "response")
summary(to_be_predicted7$yhat)
" Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.9502  0.9502  0.9502  0.9502  0.9502  0.9502"
to_be_predicted8 <- data.frame(EEDUC = "bach deg", MS = "divorced", RRACE = "Black", GENID_DESCRIBE = "male",INCOME= "HH income $100k - 149", data = dat_use1)
to_be_predicted8$yhat <- predict(model_probit_1, to_be_predicted8, type = "response")
summary(to_be_predicted8$yhat)
"Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.9521  0.9521  0.9521  0.9521  0.9521  0.9521"
to_be_predicted9 <- data.frame(EEDUC = "bach deg", MS = "divorced", RRACE = "Black", GENID_DESCRIBE = "male",INCOME= "HH income $150 - 199", data = dat_use1)
to_be_predicted9$yhat <- predict(model_probit_1, to_be_predicted9, type = "response")
summary(to_be_predicted9$yhat)
"Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.9645  0.9645  0.9645  0.9645  0.9645  0.9645"

After a series of probability predictions on a divorced Black male with a bachelors degree in a subset of people residing in the Northeast with differing levels of income, I noticed that as the household INCOME
increased, the prediction probability for the individual to get vaccinated also increased. For simplicity sake, all else remaining equal, when INCOME= "HH income less than $25k", the corresponding probability is 92 percent. When
HH income= "$25k - $34.9k", the probability is 92.88, INCOME= "HH income $35k - 49.9", INCOME= "HH income $75 - 99.9", INCOME= "HH income $100k - 149", and when INCOME= "HH income $150 - 199", the probabilities are 
93.55, 95.02, 95.21 and 96.45, respectively. This indicates a correlation between income and probability to get vaccinated, when all of the other variables in the prediction remain identical during comparisons. 


d = read.csv("trimmed.csv",header=T)
summary(d)
dim(d)

# calculate proportions of confirmed, deceased, and tested cases (new/total)
d$prop_confirmed = d$new_confirmed/d$total_confirmed
d$prop_deceased = d$new_deceased/d$total_deceased
d$prop_tested = d$new_tested/d$total_tested
summary(d)
tested_na_idxs = which(is.na(d$new_tested))
summary(tested_na_idxs==which(is.na(d$total_tested))) # all 17 TRUE

names(d)
summary(d$prop_confirmed) # looks good
summary(d$prop_deceased) # 15 NAs
summary(d$prop_tested) # 17 NAs
which(d$new_deceased<0) # 79
which(is.na(d$prop_deceased)) # 1:15
which(d$total_deceased==0) # 1:15, corresponding to 15 NAs in prop_deceased
# the data point which has negative new_deceased was assumed to be an error and imputed with the mean
d$new_deceased[79] = mean(d$new_deceased[-79])
summary(d$new_deceased)
d$prop_deceased = d$new_deceased/d$total_deceased
# replace NAs in prop_deceased with 0 (since nobody died)
d$prop_deceased[is.na(d$prop_deceased)] <- 0
summary(d$prop_deceased)
# impute NAs in prop_tested with mean
d$prop_tested[tested_na_idxs] <- mean(d$prop_tested[-tested_na_idxs])
summary(d$prop_tested)

# col 17-22: mobility, percent change (+/-), no NAs
# col 23-40: governmental response; 38-39 have NAs, so impute
hc_investment_na_idxs = which(is.na(d$emergency_investment_in_healthcare))
d$emergency_investment_in_healthcare[hc_investment_na_idxs] <- mean(d$emergency_investment_in_healthcare[-hc_investment_na_idxs])
v_investment_na_idxs = which(is.na(d$investment_in_vaccines))
d$investment_in_vaccines[v_investment_na_idxs] <- mean(d$investment_in_vaccines[-v_investment_na_idxs])
summary(d$emergency_investment_in_healthcare)
summary(d$investment_in_vaccines)
summary(d)
# fiscal_measures has 1 NA: impute with mean
fm_na_idxs = which(is.na(d$fiscal_measures))
d$fiscal_measures[fm_na_idxs] = mean(d$fiscal_measures[-fm_na_idxs])

summary(d)
# col 43-49: weather info (excluded noaa params), 47 has mostly NAs, so exclude
# col 51: prop_confirmed (cases)
# col 52: prop_deceased
# col 53: prop_tested
# col 12: new_deceased - RESPONSE
x.matrix = d[,c(17:22,23:36,38:40,43:46,48,49,53)]
DATA = d[,c(12,17:22,23:36,38:40,43:46,48,49,53)]

library(usdm)
vifstep(x.matrix,th=2)
# from the line above:
# 19 variables have collinearity
# income_support stringency_index average_temperature dew_point restrictions_on_internal_movement mobility_transit_stations testing_policy public_transport_closing cancel_public_events mobility_residential minimum_temperature restrictions_on_gatherings school_closing public_information_campaigns mobility_retail_and_recreation mobility_parks emergency_investment_in_healthcare debt_relief workplace_closing
# After excluding the collinear variables, the linear correlation coefficients ranges between: 
# min correlation ( rainfall ~ investment_in_vaccines ):  0.003841911  
# max correlation ( stay_at_home_requirements ~ mobility_workplaces ):  -0.5476402
vifstep(x.matrix,th=5)
vifstep(x.matrix,th=10)
vifstep(x.matrix,th=20)

# we deleted the th=2 variables
x.matrix = x.matrix[,-c(15,23,24,28,13,4,20,11,9,6,25,10,7,19,1,3,21,16,8)]
y.var = d$new_deceased
DATA = cbind(x.matrix,y.var)

# MODEL BUILDING STARTS HERE
full.m = lm(y.var~.,x.matrix)
summary(full.m)
model_aic = step(full.m,direction="both",k=2)
model_bic = step(full.m,direction="both",k=log(nrow(DATA)))
summary(model_aic)
# mobility_workplaces + stay_at_home_requirements + international_travel_controls + fiscal_measures + maximum_temperature + relative_humidity + prop_tested
summary(model_bic)
# mobility_workplaces + stay_at_home_requirements + maximum_temperature + relative_humidity + prop_tested

# apply cross validation to determine best aic/bic
library("asbio")
set.seed(1)
r2_full = press(full.m) # rank deficiencies

# some of the ranking variables are too skewed to be useful
par(mfrow=c(1,2))
boxplot(x.matrix$stay_at_home_requirements,main="stay at home")
boxplot(x.matrix$international_travel_controls,main="international travel")

x.matrix = x.matrix[,-c(3,4)]
DATA = cbind(x.matrix,y.var)
full.m = lm(y.var~.,x.matrix)
summary(full.m)
model_aic = step(full.m,direction="both",k=2)
model_bic = step(full.m,direction="both",k=log(nrow(DATA)))
summary(model_aic)
# mobility_workplaces + maximum_temperature + relative_humidity + prop_tested
summary(model_bic)
# mobility_workplaces + maximum_temperature + relative_humidity + prop_tested

set.seed(1)
r2_full = press(full.m) # rank issues

# plotting
par(mfrow=c(3,3))
hist(x.matrix$mobility_grocery_and_pharmacy,main="GroceryandPharmacyMobility",
	xlab="")
hist(x.matrix$mobility_workplaces,main="WorkplaceMobility",
	xlab="")
hist(x.matrix$fiscal_measures,main="FiscalMeasures",
	xlab="")
hist(x.matrix$international_support,main="InternationalSupport",
	xlab="")
hist(x.matrix$investment_in_vaccines,main="VaccineInvestment",
	xlab="")
hist(x.matrix$maximum_temperature,main="MaxTemperature",
	xlab="")
hist(x.matrix$rainfall,main="Rainfall",
	xlab="") # log this?
hist(x.matrix$relative_humidity,main="RelativeHumidity",
	xlab="")
hist(x.matrix$prop_tested,main="PropTestedCases (new/total)",
	xlab="") #log
# TRANSFORMATION
x.matrix$fiscal_measures = log(x.matrix$fiscal_measures+0.1)
x.matrix$international_support = log(x.matrix$international_support+0.1)
x.matrix$investment_in_vaccines = log(x.matrix$investment_in_vaccines+0.1)
x.matrix$rainfall = log(x.matrix$rainfall+0.1)
x.matrix$prop_tested = log(x.matrix$prop_tested+0.1)

DATA = cbind(x.matrix,y.var)
full.m = lm(y.var~.,x.matrix)
summary(full.m)
model_aic = step(full.m,direction="both",k=2)
model_bic = step(full.m,direction="both",k=log(nrow(DATA)))
summary(model_aic)
# mobility_workplaces + maximum_temperature + relative_humidity + prop_tested
summary(model_bic)
# mobility_workplaces + maximum_temperature + relative_humidity + prop_tested

par(mfrow=c(2,2))
plot(full.m)
par(mfrow=c(2,2))
plot(model_aic)
par(mfrow=c(2,2))
plot(model_bic)

x.matrix = x.matrix[-c(15,61,77,32,34,328),]
y.var = y.var[-c(15,61,77,32,34,328)]
full.m = lm(y.var~.,x.matrix)
summary(full.m)
model_aic = step(full.m,direction="both",k=2)
model_bic = step(full.m,direction="both",k=log(nrow(DATA)))
summary(model_aic)
# mobility_grocery_and_pharmacy + mobility_workplaces + maximum_temperature + relative_humidity + prop_tested
summary(model_bic)
# mobility_workplaces + maximum_temperature + relative_humidity + prop_tested

set.seed(1)
press(full.m) # rank deficiencies

# fine, we choose the most parsomonious bic model
final_x = x.matrix[,c(2,6,8,9)]
summary(model_bic)

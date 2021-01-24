# dataCleaning.R
d = read.csv("states.csv",header=T)
summary(d)
attach(d)

summary(population)
summary(gdp) # 1212120 NAs
summary(gdp_per_capita) # 1212120 NAs
summary(human_capital_index) # 1212120 NAs

gdp_na_idxs = which(is.na(gdp))
summary(gdp_na_idxs==which(is.na(gdp_per_capita))) # all TRUE
summary(gdp_na_idxs==which(is.na(human_capital_index))) # all TRUE

# get rid of gdp, gdp_per_capita, and human_capital_index because so much of it is missing
# 49:51

# rural_population, urban_population, largest_city_population, clustered_population, population_density, human_development_index all have 1212120 NAs
pop_na_idxs = which(is.na(rural_population))
summary(gdp_na_idxs==pop_na_idxs) # all TRUE
summary(pop_na_idxs==which(is.na(urban_population))) # all TRUE
summary(pop_na_idxs==which(is.na(largest_city_population))) # all TRUE
summary(pop_na_idxs==which(is.na(clustered_population))) # all TRUE
summary(pop_na_idxs==which(is.na(population_density))) # all TRUE
summary(pop_na_idxs==which(is.na(human_development_index))) # all TRUE
# 32:37

# rural_area, urban_area, smoking_prevalence, diabetes_prevalence, infant_mortality_rate, adult_male_mortality_rate, adult_female_mortality_rate, pollution_mortality_rate, comorbidity_mortality_rate, nurses, physicians, health_expenditure, out_of_pocket_health_expenditure
smoking_na_idxs = which(is.na(smoking_prevalence))
summary(smoking_na_idxs==gdp_na_idxs) # all TRUE
# all are all TRUE
summary(smoking_na_idxs==which(is.na(rural_area)))
summary(smoking_na_idxs==which(is.na(urban_area)))
summary(smoking_na_idxs==which(is.na(diabetes_prevalence)))
summary(smoking_na_idxs==which(is.na(infant_mortality_rate)))
summary(smoking_na_idxs==which(is.na(adult_male_mortality_rate)))
summary(smoking_na_idxs==which(is.na(adult_female_mortality_rate)))
summary(smoking_na_idxs==which(is.na(pollution_mortality_rate)))
summary(smoking_na_idxs==which(is.na(comorbidity_mortality_rate)))
summary(smoking_na_idxs==which(is.na(nurses)))
summary(smoking_na_idxs==which(is.na(physicians)))
summary(smoking_na_idxs==which(is.na(health_expenditure)))
summary(smoking_na_idxs==which(is.na(out_of_pocket_health_expenditure)))
# 57,58,60:66,68:71

# population_80_89 and 90_99 are unnecessary with population_age_80_and_older.
# 46,47

trim_d = d[-gdp_na_idxs,]
summary(trim_d) # everything is pretty much the same data. No point in keeping
unique(trim_d$key) # all the same

trim_d = d[-gdp_na_idxs,-c(32:37,46,47,49:51,57,58,60:66,68:71)]
trim_d = trim_d[,-c(11,14,18,20:47)]
trim_D = d[gdp_na_idxs,]
attach(trim_d)
summary(trim_d)
# further trimming
mobility_rar_na_idxs = which(is.na(mobility_retail_and_recreation))
summary(mobility_rar_na_idxs==which(is.na(mobility_grocery_and_pharmacy))) # all TRUE
summary(mobility_rar_na_idxs==which(is.na(mobility_parks))) # all TRUE
summary(mobility_rar_na_idxs==which(is.na(mobility_transit_stations))) # all TRUE
summary(mobility_rar_na_idxs==which(is.na(mobility_workplaces))) # all TRUE
summary(mobility_rar_na_idxs==which(is.na(mobility_residential))) # all TRUE
trim_d_sub1 = trim_d[-mobility_rar_na_idxs,]

summary(trim_d_sub1)
dim(trim_d_sub1)

write.csv(trim_d_sub1,"trimmed.csv",row.names=F)
# testing
test_d = read.csv("trimmed.csv",header=T)
summary(test_d) # great

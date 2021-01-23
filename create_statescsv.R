# yalehack2020.R
# original dataset citation:
#@article{Wahltinez2020,
#  author = "O. Wahltinez and others",
#  year = 2020,
#  title = "COVID-19 Open-Data: curating a fine-grained, global-scale data #repository for SARS-CoV-2",
#  note = "Work in progress",
#  url = {https://goo.gle/covid-19-open-data},
#}

d = read.csv("main.csv",header=T)
names(d)
head(d)
# col 1: key
# col 2: date
# col 3: wikidata
# col 4: datacommons
# col 5-6: country_code, country_name
# col 7-10: subregion1_code, subregion1_name, subregion2_code, subregion2_name
# col 11-12: locality_code, locality_name
# col 13-14: X3166.1.alpha.2, X3166.1.alpha.3
# col 15: aggregation_level
# col 16-19: new_confirmed, new_deceased, new_recovered, new_tested
# col 20-23: total_confirmed, total_deceased, total_recovered, total_tested
# col 24-26: new_hospitalized, total_hospitalized, current_hospitalized
# col 27-29: new_intensive_care, total_intensive_care, current_intensive_care
# col 30-32: new_ventilator, total_ventilator, current_ventilator
# col 33-35: population, population_male, population_female
# col 36-38: rural_population, urban_population, largest_city_population
# col 39-41: clustered_population, population_density, human_development_index
# col 42-52: population_age_(00_09,10_19,20_29,30_39,...,90_99,80_and_older)
# col 53-55: gdp, gdp_per_capita, human_capital_index
# col 56: open_street_maps
# col 57-59: latitude, longitude, elevation
# col 60-62: area, rural_area, urban_area
# col 63: life_expectancy
# col 64-65: smoking_prevalence, diabetes_prevalence
# col 66-70: (infant,adult_male,adult_female,pollution,comorbidity)_mortality_rate
# col 71-73: hospital_beds, nurses, physicians
# col 74-75: health_expenditure, out_of_pocket_health_expenditure
# col 76-81: mobility_(retail_and_recreation,grocery_and_pharmacy,parks,transit_stations,workplaces,residential)
# col 82-88: (school,workplace)_closing, cancel_public_events, restrictions_on_gatherings, public_transport_closing, stay_at_home_requirements, restrictions_on_internal_movement
# col 89: international_travel_controls
# col 90-93: income_support, debt_relief, fiscal_measures, international_support
# col 94: public_information_campaigns
# col 95-96: testing_policy, contact_tracing
# col 97-98: emergency_investment_in_healthcare, investment_in_vaccines
# col 99: stringency_index
# col 100-101: noaa_station, noaa_distance
# col 102-104: (average,minimum,maximum)_temperature
# col 105-108: rainfall, snowfall, dew_point, relative_humidity

attach(d)
summary(key)
us_data = d[which(substr(key,1,2)=="US"),] # probably could have used country_code, oops.

attach(us_data)
unique(key)

# get rid of US territories because they tend to be different in many ways
# included D.C.
us_states_data = us_data[which(substr(key,4,5)!="AS" & substr(key,4,5)!="FM" & substr(key,4,5)!="GU" & substr(key,4,5)!="MH" & substr(key,4,5)!="MP" & substr(key,4,5)!="PR" & substr(key,4,5)!="PW" & substr(key,4,5)!="VI" & substr(key,4,5)!="UM"),]
us_states_data$original_idxs = as.integer(rownames(us_states_data))
rownames(us_states_data) <- 1:nrow(us_states_data)

attach(us_states_data)
unique(country_code) # all "US"
unique(country_name) # all "United States of America"
unique(X3166.1.alpha.2) # all "US"
unique(X3166.1.alpha.3) # all "USA"
# so, we can get rid of columns 5-6, 13-14
new_states_data = us_states_data[,-c(5,6,13,14)]
names(new_states_data)
write.csv(new_states_data,"states.csv",row.names=F)

# check that things are correct
nsd = read.csv("states.csv",header=T)
names(nsd)
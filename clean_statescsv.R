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
d2 = d[,-c(49:51)]
attach(d2)
names(d2)

summary(d2)

# rural_population, urban_population, largest_city_population, clustered_population, population_density, human_development_index all have 1212120 NAs
pop_na_idxs = which(is.na(rural_population))
summary(gdp_na_idxs==pop_na_idxs) # all TRUE
summary(pop_na_idxs==which(is.na(urban_population))) # all TRUE
summary(pop_na_idxs==which(is.na(largest_city_population))) # all TRUE
summary(pop_na_idxs==which(is.na(clustered_population))) # all TRUE
summary(pop_na_idxs==which(is.na(population_density))) # all TRUE
summary(pop_na_idxs==which(is.na(human_development_index))) # all TRUE

d3 = d2[,-c(32:37)]
names(d3)
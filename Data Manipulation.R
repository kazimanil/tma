# Source Files Import ----
rm(list = ls()); gc();
source("https://raw.githubusercontent.com/kazimanil/tma/master/functions.R") # Base functions for data manipulation
source("https://raw.githubusercontent.com/kazimanil/project_kaf/master/gg_theme.R") # My Theme for GGPlot2

# Raw Data Import ----
# The Data is shared by TURKSTAT on the promise that it will not be shared publicly.
# Thus, I will only be able share aggregated data after the manipulation & aggregation steps.
library(foreign); # STATA Data Input
data_2004 = as.data.table(read.dta("data/2004_fh.dta"))
data_2005 = as.data.table(read.dta("data/2005_fh.dta"))
data_2006 = as.data.table(read.dta("data/2006_fh.dta"))
data_2007 = as.data.table(read.dta("data/2007_fh.dta"))
data_2008 = as.data.table(read.dta("data/2008_fh.dta"))
data_2009 = as.data.table(read.dta("data/2009_fh.dta"))
data_2010 = as.data.table(read.dta("data/2010_fh.dta"))
data_2011 = as.data.table(read.dta("data/2011_fh.dta"))
data_2012 = as.data.table(read.dta("data/2012_fh.dta"))
data_2013 = as.data.table(read.dta("data/2013_fh.dta"))
data_2014 = as.data.table(read.dta("data/2014_fh.dta"))
fert_2015 = fread("data/yma2015_fert_mikroveri.csv")
hane_2015 = fread("data/yma2015_fert_mikroveri.csv")
fert_2016 = fread("data/yma2016_fert_mikroveri.csv")
hane_2016 = fread("data/yma2016_hane_mikroveri.csv")
fert_2017 = fread("data/yma2017_fert_mikroveri.csv")
hane_2017 = fread("data/yma2017_hane_mikroveri.csv")

logit_2004 = data_2004[, .(
  weight = ff, # weight determines how many people the respondee represents.
  happiness = relevel(as.factor(happiness_transformation(b07)), "Not Happy"),
  sex = relevel(as.factor(gender(h03)), "Male"),
  age = h04,
  age_squared = h04 * h04,
  marital_status = relevel(as.factor(marital_status(b01)), "Single"),
  level_of_education = relevel(as.factor(education_level(b02)), "No Schooling"),
  employment = relevel(as.factor(employment(b03)), "Unemployed"),
  materialistic_values = relevel(as.factor(materialism(b09)), "Not Materialistic"),
  comparison_to_5years_before = relevel(as.factor(better_or_worse(b41)), "Same as Before"),
  expectations_from_next_5years = relevel(as.factor(better_or_worse(b42)), "Same as Before"),
  household_income_level = relevel(as.factor(household_income_transformation(h18)), "Household Income Tier 1"),
  # enter household income sufficiency...,
  subjective_health = relevel(as.factor(likert_categoric(b10_01)), "Neutral"),
  satisfaction_from_marriage = relevel(as.factor(likert_categoric(b10_02)), "Neutral"),
  satisfaction_from_housing = relevel(as.factor(likert_categoric(b10_03)), "Neutral"),
  satisfaction_from_education = relevel(as.factor(likert_categoric(b10_04)), "Neutral"),
  satisfaction_from_neighbourhood = relevel(as.factor(likert_categoric(b10_05)), "Neutral"),
  satisfaction_from_job = relevel(as.factor(likert_categoric(b10_06)), "Neutral"),
  satisfaction_from_household_income = relevel(as.factor(likert_categoric(b10_08)), "Neutral"),
  satisfaction_from_friends_network = relevel(as.factor(likert_categoric(b11_03)), "Neutral"),
  satisfaction_from_neighbours = relevel(as.factor(likert_categoric(b11_04)), "Neutral"),
  perception_of_safety1 = relevel(as.factor(likert_categoric(b36, "Safe")), "Neutral"),
  perception_of_safety2 = relevel(as.factor(likert_categoric(b37, "Safe")), "Neutral"),
  # degree_of_hope ...
  # subjective_welfare ...
  
)]

# Aggregated Data on Happiness and GDP per Capita in Turkey ----
happiness = rbind(
	fh_2004[, .(happiness = likert(b07), weights = ff, year = 2004)],
	fh_2005[, .(happiness = likert(bsoru7), weights = ff, year = 2005)],
	fh_2006[, .(happiness = likert(bs7), weights = faktor_fert, year = 2006)],
	fh_2007[, .(happiness = likert(bs7), weights = faktor_fert, year = 2007)],
	fh_2008[, .(happiness = likert(bs7), weights = faktor_fert, year = 2008)],
	fh_2009[, .(happiness = likert(b9), weights = ff, year = 2009)],
	fh_2010[, .(happiness = likert(b9), weights = faktor_fert, year = 2010)],
	fh_2011[, .(happiness = likert(b9), weights = ff, year = 2011)],
	fh_2012[, .(happiness = likert(b08), weights = ff, year = 2012)],
	fh_2013[, .(happiness = likert(b9), weights = ff, year = 2013)],
	fh_2014[, .(happiness = likert(b9), weights = ff, year = 2014)],
	fh_2015[, .(happiness = likert(MUTLULUK), weights = as.numeric(gsub(",", ".", FAKTOR_FERT)), year = 2015)],
	fh_2016[, .(happiness = likert(MUTLULUK), weights = as.numeric(gsub(",", ".", FAKTOR_FERT)), year = 2016)],
	fh_2017[, .(happiness = likert(MUTLULUK), weights = as.numeric(gsub(",", ".", FAKTOR_FERT)), year = 2017)]
								)

comparison = data.table(
	year = happiness[, .N, .(year)]$year,
	happiness = happiness[, .(avg_happiness = sum(happiness * weights) / sum(weights))
												, .(year)]$avg_happiness,
	gdp_pc = c(5775, 7035.8, 7596.9, 9247, 10444.4, 8560.7, 10002.6, 10427.6, 10459.2, 10821.7, 10394.5, NA, NA, NA)
	)

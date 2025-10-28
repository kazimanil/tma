# Source Files Import ----
rm(list = ls()); gc();
source("https://raw.githubusercontent.com/kazimanil/tma/master/functions.R") # Base functions for data manipulation
source("https://raw.githubusercontent.com/kazimanil/project_kaf/refs/heads/master/R_functions/gg_theme.R") # My Theme for GGPlot2

# Raw Data Import ----
# The Data is shared by TURKSTAT on the promise that it will not be shared publicly.
# Thus, I will only be able share aggregated data after the manipulation & aggregation steps.
data_2003 = fread("data/data_2003.csv")
# year 2003 was the first release of the survey and thus the variables are not fully compatible with the later years.
data_2004 = fread("data/data_2004.csv")
data_2005 = fread("data/data_2005.csv")
data_2006 = fread("data/data_2006.csv")
data_2007 = fread("data/data_2007.csv")
data_2008 = fread("data/data_2008.csv")
data_2009 = fread("data/data_2009.csv")
data_2010 = fread("data/data_2010.csv")
data_2011 = fread("data/data_2011.csv")
data_2012 = fread("data/data_2012.csv")
data_2013 = fread("data/data_2013.csv")
data_2014 = fread("data/data_2014.csv")
data_2015 = fread("data/data_2015.csv")
data_2016 = fread("data/data_2016.csv")
data_2017 = fread("data/data_2017.csv")

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
    data_2003[,.( happiness = scale_transformation(s8, minimum = 5, maximum = 1),
                  weights = ff,
                  year = 2003)],
    data_2004[, .(happiness = scale_transformation(b07, minimum = 5, maximum = 1),
	              weights = ff,
	              year = 2004)],
	data_2005[, .(happiness = scale_transformation(bsoru7, minimum = 5, maximum = 1),
	              weights = ff,
	              year = 2005)],
	data_2006[, .(happiness = scale_transformation(bs7, minimum = 5, maximum = 1),
	              weights = faktor_fert,
	              year = 2006)],
	data_2007[, .(happiness = scale_transformation(bs7, minimum = 5, maximum = 1),
	              weights = faktor_fert,
	              year = 2007)],
	data_2008[, .(happiness = scale_transformation(bs7, minimum = 5, maximum = 1),
	              weights = faktor_fert,
	              year = 2008)],
	data_2009[, .(happiness = scale_transformation(b9, minimum = 5, maximum = 1),
	              weights = ff,
	              year = 2009)],
	data_2010[, .(happiness = scale_transformation(b9, minimum = 5, maximum = 1),
	              weights = faktor_fert,
	              year = 2010)],
	data_2011[, .(happiness = scale_transformation(b9, minimum = 5, maximum = 1),
	              weights = ff,
	              year = 2011)],
	data_2012[, .(happiness = scale_transformation(b08, minimum = 5, maximum = 1),
	              weights = ff,
	              year = 2012)],
	data_2013[, .(happiness = scale_transformation(b9, minimum = 5, maximum = 1),
	              weights = ff,
	              year = 2013)],
	data_2014[, .(happiness = scale_transformation(b9, minimum = 5, maximum = 1),
	              weights = ff,
	              year = 2014)],
	data_2015[, .(happiness = scale_transformation(MUTLULUK, minimum = 5, maximum = 1),
	              weights = as.numeric(gsub(",", ".", FAKTOR_FERT)),
	              year = 2015)],
	data_2016[, .(happiness = scale_transformation(MUTLULUK, minimum = 5, maximum = 1),
	              weights = as.numeric(gsub(",", ".", FAKTOR_FERT)),
	              year = 2016)],
	data_2017[, .(happiness = scale_transformation(MUTLULUK, minimum = 5, maximum = 1),
	              weights = as.numeric(gsub(",", ".", FAKTOR_FERT)),
	              year = 2017)]
								)

comparison = data.table(
	year = happiness[, .N, .(year)]$year,
	# somehow some people escaped to answer the main question in the experimental year of 2003
	# we'll omit those 25 observations representing 180k people, or %4 of the relevant year's target population.
	# we'll assume that bias is negligible.
	happiness = happiness[!is.na(happiness), .(avg_happiness = sum(happiness * weights) / sum(weights))
												, .(year)]$avg_happiness,
	# source : World Bank
	# https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?end=2024&locations=TR&start=2003&view=chart
	gdp_pc = c(4637.9, 5960.8, 7303, 7953.1, 9711.2, 10843.5, 9013, 10622.7, 11300.8, 11713.3,
	           12578.2, 12165.2, 11050, 10970, 10695.6)
	)

rm(happiness); fwrite(comparison, "agg_data/happiness_gdp_turkey_2003_2017.csv")

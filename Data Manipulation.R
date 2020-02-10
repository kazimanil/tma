# Source Files Import ----
source("functions.R") # Manipulating TURKSTAT Data into
source("https://raw.githubusercontent.com/kazimanil/project_kaf/master/gg_theme.R") # My Theme for GGPlot2

# Raw Data Import ----
# The Data is shared by TURKSTAT on the premise that it will not be shared publicly.
# Thus, I will only be able share aggregated data after the manipulation & aggregation steps.
library(foreign); # STATA Data Input
data_2004 = read.dta("data/2004_fh.dta")
data_2005 = read.dta("data/2005_fh.dta")
data_2006 = read.dta("data/2006_fh.dta")
data_2007 = read.dta("data/2007_fh.dta")
data_2008 = read.dta("data/2008_fh.dta")
data_2009 = read.dta("data/2009_fh.dta")
data_2010 = read.dta("data/2010_fh.dta")
data_2011 = read.dta("data/2011_fh.dta")
data_2012 = read.dta("data/2012_fh.dta")
data_2013 = read.dta("data/2013_fh.dta")
data_2014 = read.dta("data/2014_fh.dta")
fert_2015 = fread("data/yma2015_fert_mikroveri.csv")
hane_2015 = fread("data/yma2015_fert_mikroveri.csv")
fert_2016 = fread("data/yma2016_fert_mikroveri.csv")
hane_2016 = fread("data/yma2016_hane_mikroveri.csv")
fert_2017 = fread("data/yma2017_fert_mikroveri.csv")
hane_2017 = fread("data/yma2017_hane_mikroveri.csv")

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

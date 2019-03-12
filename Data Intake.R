library(foreign); library(data.table)
source("functions.R")
setwd("Documents/R_Projects/tma")
# Data Input ---- 
fh_2004 = as.data.table(read.dta("data/2004_fh.dta"))
fh_2005 = as.data.table(read.dta("data/2005_fh.dta"))
fh_2006 = as.data.table(read.dta("data/2006_fh.dta"))
fh_2007 = as.data.table(read.dta("data/2007_fh.dta"))
fh_2008 = as.data.table(read.dta("data/2008_fh.dta"))
fh_2009 = as.data.table(read.dta("data/2009_fh.dta"))
fh_2010 = as.data.table(read.dta("data/2010_fh.dta"))
fh_2011 = as.data.table(read.dta("data/2011_fh.dta"))
fh_2012 = as.data.table(read.dta("data/2012_fh.dta"))
fh_2013 = as.data.table(read.dta("data/2013_fh.dta"))
fh_2014 = as.data.table(read.dta("data/2014_fh.dta"))
f_2015 = fread("data/yma2015_fert_mikroveri.csv")
f_2016 = fread("data/yma2016_fert_mikroveri.csv")
f_2017 = fread("data/yma2017_fert_mikroveri.csv")
h_2015 = fread("data/yma2015_hane_mikroveri.csv")
h_2016 = fread("data/yma2016_hane_mikroveri.csv")
h_2017 = fread("data/yma2017_hane_mikroveri.csv")
fh_2015 = merge(f_2015, h_2015, all.x = TRUE, by = "BIRIMNO")
fh_2016 = merge(f_2016, h_2016, all.x = TRUE, by = "BIRIMNO")
fh_2017 = merge(f_2017, h_2017, all.x = TRUE, by = "BIRIMNO")
rm(f_2015, f_2016, h_2015, h_2016, f_2017, h_2017); gc()

# Data Standardisation for Happiness ----
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
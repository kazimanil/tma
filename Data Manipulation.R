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

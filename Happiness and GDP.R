# Source Files Import ----
rm(list = ls()); gc();
library("data.table"); library(ggplot2);
happiness_gdpr = fread(input = "https://raw.githubusercontent.com/kazimanil/tma/refs/heads/master/agg_data/happiness_gdp_turkey_2004_2017.csv",
      verbose = FALSE)

# Source Files Import ----
rm(list = ls())
gc()
setwd("C:/Users/kazim/Documents/tma/")
library("data.table")
source("https://raw.githubusercontent.com/kazimanil/tma/master/functions.R") # Base functions for data manipulation
source("https://raw.githubusercontent.com/kazimanil/project_kaf/refs/heads/master/R_functions/gg_theme.R") # My Theme for GGPlot2

# Raw Data Import ----
# The Data is shared by TURKSTAT on the promise that it will not be shared publicly.
# Thus, I will only be able share aggregated data after the manipulation & aggregation steps.
data_2003 <- fread("data/data_2003.csv")
# year 2003 was the first release of the survey and thus the variables are not fully compatible with the later years.
data_2004 <- fread("data/data_2004.csv")
data_2005 <- fread("data/data_2005.csv")
data_2006 <- fread("data/data_2006.csv")
data_2007 <- fread("data/data_2007.csv")
data_2008 <- fread("data/data_2008.csv")
data_2009 <- fread("data/data_2009.csv")
data_2010 <- fread("data/data_2010.csv")
data_2011 <- fread("data/data_2011.csv")
data_2012 <- fread("data/data_2012.csv")
data_2013 <- fread("data/data_2013.csv")
data_2014 <- fread("data/data_2014.csv")
data_2015 <- fread("data/data_2015.csv")
data_2016 <- fread("data/data_2016.csv")
data_2017 <- fread("data/data_2017.csv")

# SWBI
## 2004 ----
swbi_2004 <- data_2004[, .(
  unique_id = paste(formno, h01, sep = "_"),
  weight = ff,
  od1 = scale_transformation(h15_01, minimum = 1, maximum = 2),
  od2 = scale_transformation(h15_02, minimum = 2, maximum = 1),
  od3 = scale_transformation(h15_03, minimum = 2, maximum = 1),
  od4 = scale_transformation(h15_04, minimum = 2, maximum = 1),
  od5 = scale_transformation(h15_05, minimum = 2, maximum = 1),
  od6 = scale_transformation(h15_06, minimum = 1, maximum = 2),
  od7 = scale_transformation(h15_07, minimum = 3, maximum = 1),
  od8 = scale_transformation(h15_08, minimum = 3, maximum = 1),
  od9 = scale_transformation(h15_09, minimum = 3, maximum = 1),
  hhg = scale_transformation(h18, minimum = 1, maximum = 6),
  hhgy = scale_transformation(h19),
  ym = scale_transformation(b07),
  sm = scale_transformation(b10_01),
  evs = scale_transformation(b10_02),
  egs = scale_transformation(b10_03),
  okm = scale_transformation(b10_04),
  semt = scale_transformation(b10_05),
  ism = scale_transformation(b10_06),
  isgm = scale_transformation(b10_07),
  hhg2 = scale_transformation(b10_08),
  tul = NA_real_,
  tuc = NA_real_,
  ark = scale_transformation(b11_03),
  kom = scale_transformation(b11_04),
  isam = scale_transformation(b11_05),
  issm = scale_transformation(b12a_03, minimum = 1, maximum = 2),
  isck = scale_transformation(b12a_04, minimum = 1, maximum = 2),
  sshm = scale_transformation(b13_01),
  sghm = scale_transformation(b13_02),
  sahm = scale_transformation(b13_03),
  sehm = scale_transformation(b13_04),
  sihm = scale_transformation(b13_05),
  bel1 = fcase(b15_01 == 1, 1, b15_01 == 2, 0.25, b15_01 == 4, 0, default = NA_real_),
  bel2 = fcase(b15_02 == 1, 1, b15_02 == 2, 0.25, b15_02 == 4, 0, default = NA_real_),
  bel3 = fcase(b15_03 == 1, 1, b15_03 == 2, 0.25, b15_03 == 4, 0, default = NA_real_),
  bel4 = fcase(b15_04 == 1, 1, b15_04 == 2, 0.25, b15_04 == 4, 0, default = NA_real_),
  bel5 = fcase(b15_05 == 1, 1, b15_05 == 2, 0.25, b15_05 == 4, 0, default = NA_real_),
  bel6 = fcase(b15_06 == 1, 1, b15_06 == 2, 0.25, b15_06 == 4, 0, default = NA_real_),
  bel7 = fcase(b15_07 == 1, 1, b15_07 == 2, 0.25, b15_07 == 4, 0, default = NA_real_),
  bel8 = fcase(b15_08 == 1, 1, b15_08 == 2, 0.25, b15_08 == 4, 0, default = NA_real_),
  bel9 = fcase(b15_09 == 1, 1, b15_09 == 2, 0.25, b15_09 == 4, 0, default = NA_real_),
  bel10 = fcase(b15_10 == 1, 1, b15_10 == 2, 0.25, b15_10 == 4, 0, default = NA_real_),
  bel11 = fcase(b15_11 == 1, 1, b15_11 == 2, 0.25, b15_11 == 4, 0, default = NA_real_),
  bel12 = fcase(b15_12 == 1, 1, b15_12 == 2, 0.25, b15_12 == 4, 0, default = NA_real_),
  bel13 = fcase(b15_14 == 1, 1, b15_14 == 2, 0.25, b15_14 == 4, 0, default = NA_real_),
  shm1 = scale_transformation(b25_01, minimum = 1, maximum = 2),
  shm2 = scale_transformation(b25_02, minimum = 1, maximum = 2),
  shm3 = scale_transformation(b25_03, minimum = 1, maximum = 2),
  shm4 = scale_transformation(b25_04, minimum = 2, maximum = 1),
  shm5 = scale_transformation(b25_06, minimum = 1, maximum = 2),
  shm6 = scale_transformation(b25_07, minimum = 1, maximum = 2),
  shm7 = scale_transformation(b25_09, minimum = 1, maximum = 2),
  shm8 = scale_transformation(b25_10, minimum = 2, maximum = 1),
  shm9 = scale_transformation(b25_11, minimum = 1, maximum = 2),
  ah = scale_transformation(b33_03, minimum = 3, maximum = 1),
  guv1 = scale_transformation(b36),
  guv2 = scale_transformation(b37),
  ud = scale_transformation(fifelse(b39 >= 3, b39 + 1, b39)),
  sw = scale_transformation(b40, minimum = 1, maximum = 11),
  ssb1 = scale_transformation(b43_01, minimum = 3, maximum = 1),
  ssb2 = scale_transformation(b43_03, minimum = 3, maximum = 1),
  ssb3 = scale_transformation(b43_05, minimum = 3, maximum = 1),
  ssb4 = scale_transformation(b43_02, minimum = 3, maximum = 1),
  ssb5 = scale_transformation(b43_04, minimum = 3, maximum = 1),
  spi = NA_real_,
  nei1 = NA_real_,
  nei2 = NA_real_,
  nei3 = NA_real_,
  nei4 = NA_real_
)]

## 2005 ----
swbi_2005 <- data_2005[, .(
  unique_id = paste(formno_1, rowid(formno_1), sep = "_"),
  weight = ff,
  od1 = scale_transformation(hsoru151, minimum = 1, maximum = 2),
  od2 = scale_transformation(hsoru152, minimum = 2, maximum = 1),
  od3 = scale_transformation(hsoru153, minimum = 2, maximum = 1),
  od4 = scale_transformation(hsoru154, minimum = 2, maximum = 1),
  od5 = scale_transformation(hsoru155, minimum = 2, maximum = 1),
  od6 = scale_transformation(hsoru156, minimum = 1, maximum = 2),
  od7 = scale_transformation(hsoru157, minimum = 3, maximum = 1),
  od8 = scale_transformation(hsoru158, minimum = 3, maximum = 1),
  od9 = scale_transformation(hsoru159, minimum = 3, maximum = 1),
  hhg = scale_transformation(hsoru18, minimum = 1, maximum = 6),
  hhgy = scale_transformation(hsoru19),
  ym = scale_transformation(bsoru7),
  sm = scale_transformation(bsoru101),
  evs = scale_transformation(bsoru102),
  egs = scale_transformation(bsoru103),
  okm = scale_transformation(bsoru104),
  semt = scale_transformation(bsoru105),
  ism = scale_transformation(bsoru106),
  isgm = scale_transformation(bsoru107),
  hhg2 = scale_transformation(bsoru108),
  tul = NA_real_,
  tuc = NA_real_,
  ark = scale_transformation(bsoru113),
  kom = scale_transformation(bsoru114),
  isam = scale_transformation(bsoru115),
  issm = scale_transformation(bsoru123, minimum = 1, maximum = 2),
  isck = scale_transformation(bsoru124, minimum = 1, maximum = 2),
  sshm = scale_transformation(bsoru131),
  sghm = scale_transformation(bsoru132),
  sahm = scale_transformation(bsoru133),
  sehm = scale_transformation(bsoru134),
  sihm = scale_transformation(bsoru135),
  bel1 = fcase(bsoru151 == 1, 1, bsoru151 == 2, 0.25, bsoru151 == 4, 0, default = NA_real_),
  bel2 = fcase(bsoru152 == 1, 1, bsoru152 == 2, 0.25, bsoru152 == 4, 0, default = NA_real_),
  bel3 = fcase(bsoru153 == 1, 1, bsoru153 == 2, 0.25, bsoru153 == 4, 0, default = NA_real_),
  bel4 = fcase(bsoru154 == 1, 1, bsoru154 == 2, 0.25, bsoru154 == 4, 0, default = NA_real_),
  bel5 = fcase(bsoru155 == 1, 1, bsoru155 == 2, 0.25, bsoru155 == 4, 0, default = NA_real_),
  bel6 = fcase(bsoru156 == 1, 1, bsoru156 == 2, 0.25, bsoru156 == 4, 0, default = NA_real_),
  bel7 = fcase(bsoru157 == 1, 1, bsoru157 == 2, 0.25, bsoru157 == 4, 0, default = NA_real_),
  bel8 = fcase(bsoru158 == 1, 1, bsoru158 == 2, 0.25, bsoru158 == 4, 0, default = NA_real_),
  bel9 = fcase(bsoru159 == 1, 1, bsoru159 == 2, 0.25, bsoru159 == 4, 0, default = NA_real_),
  bel10 = fcase(bsoru1510 == 1, 1, bsoru1510 == 2, 0.25, bsoru1510 == 4, 0, default = NA_real_),
  bel11 = fcase(bsoru1511 == 1, 1, bsoru1511 == 2, 0.25, bsoru1511 == 4, 0, default = NA_real_),
  bel12 = fcase(bsoru1512 == 1, 1, bsoru1512 == 2, 0.25, bsoru1512 == 4, 0, default = NA_real_),
  bel13 = fcase(bsoru1513 == 1, 1, bsoru1513 == 2, 0.25, bsoru1513 == 4, 0, default = NA_real_),
  shm1 = scale_transformation(bsoru251, minimum = 1, maximum = 2),
  shm2 = scale_transformation(bsoru252, minimum = 1, maximum = 2),
  shm3 = scale_transformation(bsoru253, minimum = 1, maximum = 2),
  shm4 = scale_transformation(bsoru254, minimum = 2, maximum = 1),
  shm5 = scale_transformation(bsoru256, minimum = 1, maximum = 2),
  shm6 = scale_transformation(bsoru257, minimum = 1, maximum = 2),
  shm7 = scale_transformation(bsoru259, minimum = 1, maximum = 2),
  shm8 = scale_transformation(bsoru210, minimum = 2, maximum = 1),
  shm9 = scale_transformation(bsoru211, minimum = 1, maximum = 2),
  ah = scale_transformation(bsoru333, minimum = 3, maximum = 1),
  guv1 = scale_transformation(bsoru36),
  guv2 = scale_transformation(bsoru37),
  ud = scale_transformation(fifelse(bsoru39 >= 3, bsoru39 + 1, bsoru39)),
  sw = scale_transformation(bsoru40, minimum = 1, maximum = 11),
  ssb1 = scale_transformation(bsoru431, minimum = 3, maximum = 1),
  ssb2 = scale_transformation(bsoru433, minimum = 3, maximum = 1),
  ssb3 = scale_transformation(bsoru435, minimum = 3, maximum = 1),
  ssb4 = scale_transformation(bsoru432, minimum = 3, maximum = 1),
  ssb5 = scale_transformation(bsoru434, minimum = 3, maximum = 1),
  spi = NA_real_,
  nei1 = NA_real_,
  nei2 = NA_real_,
  nei3 = NA_real_,
  nei4 = NA_real_
)]

## 2006 ----
swbi_2006 <- data_2006[, .(
  unique_id = paste(formno, fertno, sep = "_"),
  weight = faktor_fert,
  od1 = scale_transformation(hs15_1, minimum = 1, maximum = 2),
  od2 = scale_transformation(hs15_2, minimum = 2, maximum = 1),
  od3 = scale_transformation(hs15_3, minimum = 2, maximum = 1),
  od4 = scale_transformation(hs15_4, minimum = 2, maximum = 1),
  od5 = scale_transformation(hs15_5, minimum = 2, maximum = 1),
  od6 = scale_transformation(hs15_6, minimum = 1, maximum = 2),
  od7 = scale_transformation(hs15_7, minimum = 3, maximum = 1),
  od8 = scale_transformation(hs15_8, minimum = 3, maximum = 1),
  od9 = scale_transformation(hs15_9, minimum = 3, maximum = 1),
  hhg = scale_transformation(hs18, minimum = 1, maximum = 6),
  hhgy = scale_transformation(hs19),
  ym = scale_transformation(bs7),
  sm = scale_transformation(bs10_1),
  evs = scale_transformation(bs10_2),
  egs = scale_transformation(bs10_3),
  okm = scale_transformation(bs10_4),
  semt = scale_transformation(bs10_5),
  ism = scale_transformation(bs10_6),
  isgm = scale_transformation(bs10_7),
  hhg2 = scale_transformation(bs10_8),
  tul = NA_real_,
  tuc = NA_real_,
  ark = scale_transformation(bs11_3),
  kom = scale_transformation(bs11_4),
  isam = scale_transformation(bs11_5),
  issm = scale_transformation(bs12a_3, minimum = 1, maximum = 2),
  isck = scale_transformation(bs12a_4, minimum = 1, maximum = 2),
  sshm = scale_transformation(bs13_1),
  sghm = scale_transformation(bs13_2),
  sahm = scale_transformation(bs13_3),
  sehm = scale_transformation(bs13_4),
  sihm = scale_transformation(bs13_5),
  bel1 = fcase(bs15_1 == 1, 1, bs15_1 == 2, 0.25, bs15_1 == 4, 0, default = NA_real_),
  bel2 = fcase(bs15_2 == 1, 1, bs15_2 == 2, 0.25, bs15_2 == 4, 0, default = NA_real_),
  bel3 = fcase(bs15_3 == 1, 1, bs15_3 == 2, 0.25, bs15_3 == 4, 0, default = NA_real_),
  bel4 = fcase(bs15_4 == 1, 1, bs15_4 == 2, 0.25, bs15_4 == 4, 0, default = NA_real_),
  bel5 = fcase(bs15_5 == 1, 1, bs15_5 == 2, 0.25, bs15_5 == 4, 0, default = NA_real_),
  bel6 = fcase(bs15_6 == 1, 1, bs15_6 == 2, 0.25, bs15_6 == 4, 0, default = NA_real_),
  bel7 = fcase(bs15_7 == 1, 1, bs15_7 == 2, 0.25, bs15_7 == 4, 0, default = NA_real_),
  bel8 = fcase(bs15_8 == 1, 1, bs15_8 == 2, 0.25, bs15_8 == 4, 0, default = NA_real_),
  bel9 = fcase(bs15_9 == 1, 1, bs15_9 == 2, 0.25, bs15_9 == 4, 0, default = NA_real_),
  bel10 = fcase(bs15_10 == 1, 1, bs15_10 == 2, 0.25, bs15_10 == 4, 0, default = NA_real_),
  bel11 = fcase(bs15_11 == 1, 1, bs15_11 == 2, 0.25, bs15_11 == 4, 0, default = NA_real_),
  bel12 = fcase(bs15_12 == 1, 1, bs15_12 == 2, 0.25, bs15_12 == 4, 0, default = NA_real_),
  bel13 = fcase(bs15_13 == 1, 1, bs15_13 == 2, 0.25, bs15_13 == 4, 0, default = NA_real_),
  shm1 = scale_transformation(bs25_1, minimum = 1, maximum = 2),
  shm2 = scale_transformation(bs25_2, minimum = 1, maximum = 2),
  shm3 = scale_transformation(bs25_3, minimum = 1, maximum = 2),
  shm4 = scale_transformation(bs25_4, minimum = 2, maximum = 1),
  shm5 = scale_transformation(bs25_6, minimum = 1, maximum = 2),
  shm6 = scale_transformation(bs25_7, minimum = 1, maximum = 2),
  shm7 = scale_transformation(bs25_9, minimum = 1, maximum = 2),
  shm8 = scale_transformation(bs25_10, minimum = 2, maximum = 1),
  shm9 = scale_transformation(bs25_11, minimum = 1, maximum = 2),
  ah = scale_transformation(bs32_3, minimum = 3, maximum = 1),
  guv1 = scale_transformation(bs35),
  guv2 = scale_transformation(bs36),
  ud = scale_transformation(fifelse(bs37 >= 3, bs37 + 1, bs37)),
  sw = scale_transformation(bs38, minimum = 1, maximum = 11),
  ssb1 = scale_transformation(bs41_1, minimum = 3, maximum = 1),
  ssb2 = scale_transformation(bs41_3, minimum = 3, maximum = 1),
  ssb3 = scale_transformation(bs41_5, minimum = 3, maximum = 1),
  ssb4 = scale_transformation(bs41_2, minimum = 3, maximum = 1),
  ssb5 = scale_transformation(bs41_4, minimum = 3, maximum = 1),
  spi = NA_real_,
  nei1 = NA_real_,
  nei2 = NA_real_,
  nei3 = NA_real_,
  nei4 = NA_real_
)]

## 2007 ----
swbi_2007 <- data_2007[, .(
  unique_id = paste(formno, bireysira, sep = "_"),
  weight = faktor_fert,
  od1 = scale_transformation(hs15_1, minimum = 1, maximum = 2),
  od2 = scale_transformation(hs15_2, minimum = 2, maximum = 1),
  od3 = scale_transformation(hs15_3, minimum = 2, maximum = 1),
  od4 = scale_transformation(hs15_4, minimum = 2, maximum = 1),
  od5 = scale_transformation(hs15_5, minimum = 2, maximum = 1),
  od6 = scale_transformation(hs15_6, minimum = 1, maximum = 2),
  od7 = scale_transformation(hs15_7, minimum = 3, maximum = 1),
  od8 = scale_transformation(hs15_8, minimum = 3, maximum = 1),
  od9 = scale_transformation(hs15_9, minimum = 3, maximum = 1),
  hhg = scale_transformation(hs18, minimum = 1, maximum = 6),
  hhgy = scale_transformation(hs19),
  ym = scale_transformation(bs7),
  sm = scale_transformation(bs10_1),
  evs = scale_transformation(bs10_2),
  egs = scale_transformation(bs10_3),
  okm = scale_transformation(bs10_4),
  semt = scale_transformation(bs10_5),
  ism = scale_transformation(bs10_6),
  isgm = scale_transformation(bs10_7),
  hhg2 = scale_transformation(bs10_8),
  tul = NA_real_,
  tuc = NA_real_,
  ark = scale_transformation(bs11_3),
  kom = scale_transformation(bs11_4),
  isam = scale_transformation(bs11_5),
  issm = scale_transformation(bs12a_3, minimum = 1, maximum = 2),
  isck = scale_transformation(bs12a_4, minimum = 1, maximum = 2),
  sshm = scale_transformation(bs13_1),
  sghm = scale_transformation(bs13_2),
  sahm = scale_transformation(bs13_3),
  sehm = scale_transformation(bs13_4),
  sihm = scale_transformation(bs13_5),
  bel1 = fcase(bs15_1 == 1, 1, bs15_1 == 2, 0.25, bs15_1 == 4, 0, default = NA_real_),
  bel2 = fcase(bs15_2 == 1, 1, bs15_2 == 2, 0.25, bs15_2 == 4, 0, default = NA_real_),
  bel3 = fcase(bs15_3 == 1, 1, bs15_3 == 2, 0.25, bs15_3 == 4, 0, default = NA_real_),
  bel4 = fcase(bs15_4 == 1, 1, bs15_4 == 2, 0.25, bs15_4 == 4, 0, default = NA_real_),
  bel5 = fcase(bs15_5 == 1, 1, bs15_5 == 2, 0.25, bs15_5 == 4, 0, default = NA_real_),
  bel6 = fcase(bs15_6 == 1, 1, bs15_6 == 2, 0.25, bs15_6 == 4, 0, default = NA_real_),
  bel7 = fcase(bs15_7 == 1, 1, bs15_7 == 2, 0.25, bs15_7 == 4, 0, default = NA_real_),
  bel8 = fcase(bs15_8 == 1, 1, bs15_8 == 2, 0.25, bs15_8 == 4, 0, default = NA_real_),
  bel9 = fcase(bs15_9 == 1, 1, bs15_9 == 2, 0.25, bs15_9 == 4, 0, default = NA_real_),
  bel10 = fcase(bs15_10 == 1, 1, bs15_10 == 2, 0.25, bs15_10 == 4, 0, default = NA_real_),
  bel11 = fcase(bs15_11 == 1, 1, bs15_11 == 2, 0.25, bs15_11 == 4, 0, default = NA_real_),
  bel12 = fcase(bs15_12 == 1, 1, bs15_12 == 2, 0.25, bs15_12 == 4, 0, default = NA_real_),
  bel13 = fcase(bs15_13 == 1, 1, bs15_13 == 2, 0.25, bs15_13 == 4, 0, default = NA_real_),
  shm1 = scale_transformation(bs25_1, minimum = 1, maximum = 2),
  shm2 = scale_transformation(bs25_2, minimum = 1, maximum = 2),
  shm3 = scale_transformation(bs25_3, minimum = 1, maximum = 2),
  shm4 = scale_transformation(bs25_4, minimum = 2, maximum = 1),
  shm5 = scale_transformation(bs25_6, minimum = 1, maximum = 2),
  shm6 = scale_transformation(bs25_7, minimum = 1, maximum = 2),
  shm7 = scale_transformation(bs25_9, minimum = 1, maximum = 2),
  shm8 = scale_transformation(bs25_10, minimum = 2, maximum = 1),
  shm9 = scale_transformation(bs25_11, minimum = 1, maximum = 2),
  ah = scale_transformation(bs32_3, minimum = 3, maximum = 1),
  guv1 = scale_transformation(bs35),
  guv2 = scale_transformation(bs36),
  ud = scale_transformation(fifelse(bs37 >= 3, bs37 + 1, bs37)),
  sw = scale_transformation(bs38, minimum = 1, maximum = 11),
  ssb1 = scale_transformation(bs41_1, minimum = 3, maximum = 1),
  ssb2 = scale_transformation(bs41_3, minimum = 3, maximum = 1),
  ssb3 = scale_transformation(bs41_5, minimum = 3, maximum = 1),
  ssb4 = scale_transformation(bs41_2, minimum = 3, maximum = 1),
  ssb5 = scale_transformation(bs41_4, minimum = 3, maximum = 1),
  spi = NA_real_,
  nei1 = NA_real_,
  nei2 = NA_real_,
  nei3 = NA_real_,
  nei4 = NA_real_
)]

## 2008 ----
swbi_2008 <- data_2008[, .(
  unique_id = paste(formno, bireysir, sep = "_"),
  weight = faktor_fert,
  od1 = scale_transformation(hs15_1, minimum = 1, maximum = 2),
  od2 = scale_transformation(hs15_2, minimum = 2, maximum = 1),
  od3 = scale_transformation(hs15_3, minimum = 2, maximum = 1),
  od4 = scale_transformation(hs15_4, minimum = 2, maximum = 1),
  od5 = scale_transformation(hs15_5, minimum = 2, maximum = 1),
  od6 = scale_transformation(hs15_6, minimum = 1, maximum = 2),
  od7 = scale_transformation(hs15_7, minimum = 3, maximum = 1),
  od8 = scale_transformation(hs15_8, minimum = 3, maximum = 1),
  od9 = scale_transformation(hs15_9, minimum = 3, maximum = 1),
  hhg = scale_transformation(hs18, minimum = 1, maximum = 6),
  hhgy = scale_transformation(hs19),
  ym = scale_transformation(bs7),
  sm = scale_transformation(bs10_1),
  evs = scale_transformation(bs10_2),
  egs = scale_transformation(bs10_3),
  okm = scale_transformation(bs10_4),
  semt = scale_transformation(bs10_5),
  ism = scale_transformation(bs10_6),
  isgm = scale_transformation(bs10_7),
  hhg2 = scale_transformation(bs10_8),
  tul = NA_real_,
  tuc = NA_real_,
  ark = scale_transformation(bs11_3),
  kom = scale_transformation(bs11_4),
  isam = scale_transformation(bs11_5),
  issm = scale_transformation(bs12a_3, minimum = 1, maximum = 2),
  isck = scale_transformation(bs12a_4, minimum = 1, maximum = 2),
  sshm = scale_transformation(bs13_1),
  sghm = scale_transformation(bs13_2),
  sahm = scale_transformation(bs13_3),
  sehm = scale_transformation(bs13_4),
  sihm = scale_transformation(bs13_5),
  bel1 = fcase(bs15_1 == 1, 1, bs15_1 == 2, 0.25, bs15_1 == 4, 0, default = NA_real_),
  bel2 = fcase(bs15_2 == 1, 1, bs15_2 == 2, 0.25, bs15_2 == 4, 0, default = NA_real_),
  bel3 = fcase(bs15_3 == 1, 1, bs15_3 == 2, 0.25, bs15_3 == 4, 0, default = NA_real_),
  bel4 = fcase(bs15_4 == 1, 1, bs15_4 == 2, 0.25, bs15_4 == 4, 0, default = NA_real_),
  bel5 = fcase(bs15_5 == 1, 1, bs15_5 == 2, 0.25, bs15_5 == 4, 0, default = NA_real_),
  bel6 = fcase(bs15_6 == 1, 1, bs15_6 == 2, 0.25, bs15_6 == 4, 0, default = NA_real_),
  bel7 = fcase(bs15_7 == 1, 1, bs15_7 == 2, 0.25, bs15_7 == 4, 0, default = NA_real_),
  bel8 = fcase(bs15_8 == 1, 1, bs15_8 == 2, 0.25, bs15_8 == 4, 0, default = NA_real_),
  bel9 = fcase(bs15_9 == 1, 1, bs15_9 == 2, 0.25, bs15_9 == 4, 0, default = NA_real_),
  bel10 = fcase(bs15_10 == 1, 1, bs15_10 == 2, 0.25, bs15_10 == 4, 0, default = NA_real_),
  bel11 = fcase(bs15_11 == 1, 1, bs15_11 == 2, 0.25, bs15_11 == 4, 0, default = NA_real_),
  bel12 = fcase(bs15_12 == 1, 1, bs15_12 == 2, 0.25, bs15_12 == 4, 0, default = NA_real_),
  bel13 = fcase(bs15_13 == 1, 1, bs15_13 == 2, 0.25, bs15_13 == 4, 0, default = NA_real_),
  shm1 = scale_transformation(bs25_1, minimum = 1, maximum = 2),
  shm2 = scale_transformation(bs25_2, minimum = 1, maximum = 2),
  shm3 = scale_transformation(bs25_3, minimum = 1, maximum = 2),
  shm4 = scale_transformation(bs25_4, minimum = 2, maximum = 1),
  shm5 = scale_transformation(bs25_6, minimum = 1, maximum = 2),
  shm6 = scale_transformation(bs25_7, minimum = 1, maximum = 2),
  shm7 = scale_transformation(bs25_9, minimum = 1, maximum = 2),
  shm8 = scale_transformation(bs25_10, minimum = 2, maximum = 1),
  shm9 = scale_transformation(bs25_11, minimum = 1, maximum = 2),
  ah = scale_transformation(bs32_3, minimum = 3, maximum = 1),
  guv1 = scale_transformation(bs35),
  guv2 = scale_transformation(bs36),
  ud = scale_transformation(fifelse(bs37 >= 3, bs37 + 1, bs37)),
  sw = scale_transformation(bs38, minimum = 1, maximum = 11),
  ssb1 = scale_transformation(bs41_1, minimum = 3, maximum = 1),
  ssb2 = scale_transformation(bs41_3, minimum = 3, maximum = 1),
  ssb3 = scale_transformation(bs41_5, minimum = 3, maximum = 1),
  ssb4 = scale_transformation(bs41_2, minimum = 3, maximum = 1),
  ssb5 = scale_transformation(bs41_4, minimum = 3, maximum = 1),
  spi = NA_real_,
  nei1 = NA_real_,
  nei2 = NA_real_,
  nei3 = NA_real_,
  nei4 = NA_real_
)]

## 2009 ----
# Schema overhaul: od1-od9 and several bel items are now the *average* of a
# "government provider" and a "private provider" version of the same question
# (Stata's `egen ... = rowmean(a b)`); spi and nei1-4 get real formulas for the
# first time (previously NA placeholders).

swbi_2009 <- data_2009[, .(
  unique_id = paste(formno, fertsirano, sep = "_"),
  weight = ff,
  od1 = rowmean2(scale_transformation(h15_1, minimum = 1, maximum = 2), scale_transformation(h15_10, minimum = 1, maximum = 2)),
  od2 = rowmean2(scale_transformation(h15_2, minimum = 2, maximum = 1), scale_transformation(h15_11, minimum = 2, maximum = 1)),
  od3 = rowmean2(scale_transformation(h15_3, minimum = 2, maximum = 1), scale_transformation(h15_12, minimum = 2, maximum = 1)),
  od4 = rowmean2(scale_transformation(h15_4, minimum = 2, maximum = 1), scale_transformation(h15_13, minimum = 2, maximum = 1)),
  od5 = rowmean2(scale_transformation(h15_5, minimum = 2, maximum = 1), scale_transformation(h15_14, minimum = 2, maximum = 1)),
  od6 = rowmean2(scale_transformation(h15_6, minimum = 1, maximum = 2), scale_transformation(h15_15, minimum = 1, maximum = 2)),
  od7 = rowmean2(scale_transformation(h15_7, minimum = 3, maximum = 1), scale_transformation(h15_16, minimum = 3, maximum = 1)),
  od8 = rowmean2(scale_transformation(h15_8, minimum = 3, maximum = 1), scale_transformation(h15_17, minimum = 3, maximum = 1)),
  od9 = rowmean2(scale_transformation(h15_9, minimum = 3, maximum = 1), scale_transformation(h15_18, minimum = 3, maximum = 1)),
  hhg = scale_transformation(h17, minimum = 1, maximum = 6),
  hhgy = scale_transformation(h18),
  ym = scale_transformation(b9),
  sm = scale_transformation(b12_1),
  evs = scale_transformation(b12_2),
  egs = scale_transformation(b12_3),
  okm = scale_transformation(b12_4),
  semt = scale_transformation(b12_5),
  ism = scale_transformation(b12_6),
  isgm = scale_transformation(b12_7),
  hhg2 = scale_transformation(b12_8),
  tul = NA_real_,
  tuc = NA_real_,
  ark = scale_transformation(b13_3),
  kom = scale_transformation(b13_4),
  isam = scale_transformation(b13_5),
  issm = scale_transformation(b16_3, minimum = 1, maximum = 2),
  isck = scale_transformation(b16_4, minimum = 1, maximum = 2),
  sshm = scale_transformation(b14_1),
  sghm = scale_transformation(b14_2),
  sahm = scale_transformation(b14_3),
  sehm = scale_transformation(b14_4),
  sihm = scale_transformation(b14_6),
  bel1 = fcase(b18_1 == 1, 1, b18_1 == 2, 0.25, b18_1 == 4, 0, default = NA_real_),
  bel2 = rowmean2(
    fcase(b18_2 == 1, 1, b18_2 == 2, 0.25, b18_2 == 4, 0, default = NA_real_),
    fcase(b19_1 == 1, 1, b19_1 == 2, 0.25, b19_1 == 4, 0, default = NA_real_)
  ),
  bel3 = rowmean2(
    fcase(b18_3 == 1, 1, b18_3 == 2, 0.25, b18_3 == 4, 0, default = NA_real_),
    fcase(b19_2 == 1, 1, b19_2 == 2, 0.25, b19_2 == 4, 0, default = NA_real_)
  ),
  bel4 = fcase(b18_4 == 1, 1, b18_4 == 2, 0.25, b18_4 == 4, 0, default = NA_real_),
  bel5 = fcase(b18_5 == 1, 1, b18_5 == 2, 0.25, b18_5 == 4, 0, default = NA_real_),
  bel6 = rowmean2(
    fcase(b18_6 == 1, 1, b18_6 == 2, 0.25, b18_6 == 4, 0, default = NA_real_),
    fcase(b19_3 == 1, 1, b19_3 == 2, 0.25, b19_3 == 4, 0, default = NA_real_)
  ),
  bel7 = fcase(b18_7 == 1, 1, b18_7 == 2, 0.25, b18_7 == 4, 0, default = NA_real_),
  bel8 = fcase(b18_8 == 1, 1, b18_8 == 2, 0.25, b18_8 == 4, 0, default = NA_real_),
  bel9 = rowmean2(
    fcase(b18_14 == 1, 1, b18_14 == 2, 0.25, b18_14 == 4, 0, default = NA_real_),
    fcase(b19_8 == 1, 1, b19_8 == 2, 0.25, b19_8 == 4, 0, default = NA_real_)
  ),
  bel10 = rowmean2(
    fcase(b18_13 == 1, 1, b18_13 == 2, 0.25, b18_13 == 4, 0, default = NA_real_),
    fcase(b19_7 == 1, 1, b19_7 == 2, 0.25, b19_7 == 4, 0, default = NA_real_)
  ),
  bel11 = rowmean2(
    fcase(b18_12 == 1, 1, b18_12 == 2, 0.25, b18_12 == 4, 0, default = NA_real_),
    fcase(b19_6 == 1, 1, b19_6 == 2, 0.25, b19_6 == 4, 0, default = NA_real_)
  ),
  bel12 = fcase(b18_9 == 1, 1, b18_9 == 2, 0.25, b18_9 == 4, 0, default = NA_real_),
  bel13 = rowmean2(
    fcase(b18_10 == 1, 1, b18_10 == 2, 0.25, b18_10 == 4, 0, default = NA_real_),
    fcase(b19_4 == 1, 1, b19_4 == 2, 0.25, b19_4 == 4, 0, default = NA_real_)
  ),
  shm1 = scale_transformation(b28_1, minimum = 1, maximum = 2),
  shm2 = scale_transformation(b28_2, minimum = 1, maximum = 2),
  shm3 = scale_transformation(b28_3, minimum = 1, maximum = 2),
  shm4 = scale_transformation(b28_4, minimum = 2, maximum = 1),
  shm5 = scale_transformation(b28_6, minimum = 1, maximum = 2),
  shm6 = scale_transformation(b28_7, minimum = 1, maximum = 2),
  shm7 = scale_transformation(b28_11, minimum = 1, maximum = 2),
  shm8 = scale_transformation(b28_9, minimum = 2, maximum = 1),
  shm9 = scale_transformation(b28_10, minimum = 1, maximum = 2),
  ah = scale_transformation(b34_3, minimum = 3, maximum = 1),
  guv1 = scale_transformation(b37),
  guv2 = scale_transformation(b38),
  ud = scale_transformation(fifelse(b39 >= 3, b39 + 1, b39)),
  sw = scale_transformation(b40, minimum = 1, maximum = 11),
  ssb1 = scale_transformation(b43_1, minimum = 3, maximum = 1),
  ssb2 = scale_transformation(b43_3, minimum = 3, maximum = 1),
  ssb3 = scale_transformation(b43_2, minimum = 3, maximum = 1),
  ssb4 = scale_transformation(b43_5, minimum = 3, maximum = 1),
  ssb5 = scale_transformation(b43_4, minimum = 3, maximum = 1),
  spi = {
    t <- list(
      scale_transformation(b49_1, minimum = 4, maximum = 1),
      scale_transformation(b49_2, minimum = 4, maximum = 1),
      scale_transformation(b49_3, minimum = 4, maximum = 1),
      scale_transformation(b49_4, minimum = 4, maximum = 1),
      scale_transformation(b49_5, minimum = 4, maximum = 1),
      scale_transformation(b49_6, minimum = 4, maximum = 1),
      scale_transformation(b49_7, minimum = 4, maximum = 1),
      scale_transformation(b49_8, minimum = 4, maximum = 1),
      scale_transformation(b49_9, minimum = 4, maximum = 1),
      fifelse(b49_11 == 5, NA_real_, (4 - b49_10) / 3), # ported bug: guards on b49_11, not b49_10
      scale_transformation(b49_11, minimum = 4, maximum = 1)
    )
    spi1 <- rowMeans(do.call(cbind, t), na.rm = TRUE)
    fifelse(!is.na(spi1) & spi1 > 10 / 11, 1, 0)
  },
  nei1 = scale_transformation(b51_1, minimum = 3, maximum = 1),
  nei2 = scale_transformation(b51_2, minimum = 3, maximum = 1),
  nei3 = scale_transformation(b51_3, minimum = 3, maximum = 1),
  nei4 = scale_transformation(b51_4, minimum = 3, maximum = 1)
)]

## 2010 ----
swbi_2010 <- data_2010[, .(
  unique_id = paste(formno, fert_sira_no, sep = "_"),
  weight = faktor_fert,
  od1 = rowmean2(scale_transformation(h15_1, minimum = 1, maximum = 2), scale_transformation(h15_10, minimum = 1, maximum = 2)),
  od2 = rowmean2(scale_transformation(h15_2, minimum = 2, maximum = 1), scale_transformation(h15_11, minimum = 2, maximum = 1)),
  od3 = rowmean2(scale_transformation(h15_3, minimum = 2, maximum = 1), scale_transformation(h15_12, minimum = 2, maximum = 1)),
  od4 = rowmean2(scale_transformation(h15_4, minimum = 2, maximum = 1), scale_transformation(h15_13, minimum = 2, maximum = 1)),
  od5 = rowmean2(scale_transformation(h15_5, minimum = 2, maximum = 1), scale_transformation(h15_14, minimum = 2, maximum = 1)),
  od6 = rowmean2(scale_transformation(h15_6, minimum = 1, maximum = 2), scale_transformation(h15_15, minimum = 1, maximum = 2)),
  od7 = rowmean2(scale_transformation(h15_7, minimum = 3, maximum = 1), scale_transformation(h15_16, minimum = 3, maximum = 1)),
  od8 = rowmean2(scale_transformation(h15_8, minimum = 3, maximum = 1), scale_transformation(h15_17, minimum = 3, maximum = 1)),
  od9 = rowmean2(scale_transformation(h15_9, minimum = 3, maximum = 1), scale_transformation(h15_18, minimum = 3, maximum = 1)),
  hhg = scale_transformation(h17, minimum = 1, maximum = 6),
  hhgy = scale_transformation(h18),
  ym = scale_transformation(b9),
  sm = scale_transformation(b12_1),
  evs = scale_transformation(b12_2),
  egs = scale_transformation(b12_3),
  okm = scale_transformation(b12_4),
  semt = scale_transformation(b12_5),
  ism = scale_transformation(b12_6),
  isgm = scale_transformation(b12_7),
  hhg2 = scale_transformation(b12_8),
  tul = NA_real_,
  tuc = NA_real_,
  ark = scale_transformation(b13_3),
  kom = scale_transformation(b13_4),
  isam = scale_transformation(b13_5),
  issm = scale_transformation(b16_3, minimum = 1, maximum = 2),
  isck = scale_transformation(b16_4, minimum = 1, maximum = 2),
  sshm = scale_transformation(b14_1),
  sghm = scale_transformation(b14_2),
  sahm = scale_transformation(b14_3),
  sehm = scale_transformation(b14_4),
  sihm = scale_transformation(b14_6),
  bel1 = fcase(b18_1 == 1, 1, b18_1 == 2, 0.25, b18_1 == 4, 0, default = NA_real_),
  bel2 = rowmean2(
    fcase(b18_2 == 1, 1, b18_2 == 2, 0.25, b18_2 == 4, 0, default = NA_real_),
    fcase(b19_1 == 1, 1, b19_1 == 2, 0.25, b19_1 == 4, 0, default = NA_real_)
  ),
  bel3 = rowmean2(
    fcase(b18_3 == 1, 1, b18_3 == 2, 0.25, b18_3 == 4, 0, default = NA_real_),
    fcase(b19_2 == 1, 1, b19_2 == 2, 0.25, b19_2 == 4, 0, default = NA_real_)
  ),
  bel4 = fcase(b18_4 == 1, 1, b18_4 == 2, 0.25, b18_4 == 4, 0, default = NA_real_),
  bel5 = fcase(b18_5 == 1, 1, b18_5 == 2, 0.25, b18_5 == 4, 0, default = NA_real_),
  bel6 = rowmean2(
    fcase(b18_6 == 1, 1, b18_6 == 2, 0.25, b18_6 == 4, 0, default = NA_real_),
    fcase(b19_3 == 1, 1, b19_3 == 2, 0.25, b19_3 == 4, 0, default = NA_real_)
  ),
  bel7 = fcase(b18_7 == 1, 1, b18_7 == 2, 0.25, b18_7 == 4, 0, default = NA_real_),
  bel8 = fcase(b18_8 == 1, 1, b18_8 == 2, 0.25, b18_8 == 4, 0, default = NA_real_),
  bel9 = rowmean2(
    fcase(b18_14 == 1, 1, b18_14 == 2, 0.25, b18_14 == 4, 0, default = NA_real_),
    fcase(b19_8 == 1, 1, b19_8 == 2, 0.25, b19_8 == 4, 0, default = NA_real_)
  ),
  bel10 = rowmean2(
    fcase(b18_13 == 1, 1, b18_13 == 2, 0.25, b18_13 == 4, 0, default = NA_real_),
    fcase(b19_7 == 1, 1, b19_7 == 2, 0.25, b19_7 == 4, 0, default = NA_real_)
  ),
  bel11 = rowmean2(
    fcase(b18_12 == 1, 1, b18_12 == 2, 0.25, b18_12 == 4, 0, default = NA_real_),
    fcase(b19_6 == 1, 1, b19_6 == 2, 0.25, b19_6 == 4, 0, default = NA_real_)
  ),
  bel12 = fcase(b18_9 == 1, 1, b18_9 == 2, 0.25, b18_9 == 4, 0, default = NA_real_),
  bel13 = rowmean2(
    fcase(b18_10 == 1, 1, b18_10 == 2, 0.25, b18_10 == 4, 0, default = NA_real_),
    fcase(b19_4 == 1, 1, b19_4 == 2, 0.25, b19_4 == 4, 0, default = NA_real_)
  ),
  shm1 = scale_transformation(b28_1, minimum = 1, maximum = 2),
  shm2 = scale_transformation(b28_2, minimum = 1, maximum = 2),
  shm3 = scale_transformation(b28_3, minimum = 1, maximum = 2),
  shm4 = scale_transformation(b28_4, minimum = 2, maximum = 1),
  shm5 = scale_transformation(b28_6, minimum = 1, maximum = 2),
  shm6 = scale_transformation(b28_7, minimum = 1, maximum = 2),
  shm7 = scale_transformation(b28_11, minimum = 1, maximum = 2),
  shm8 = scale_transformation(b28_9, minimum = 2, maximum = 1),
  shm9 = scale_transformation(b28_10, minimum = 1, maximum = 2),
  ah = scale_transformation(b34_3, minimum = 3, maximum = 1),
  guv1 = scale_transformation(b37),
  guv2 = scale_transformation(b38),
  ud = scale_transformation(fifelse(b39 >= 3, b39 + 1, b39)),
  sw = scale_transformation(b40, minimum = 1, maximum = 11),
  ssb1 = scale_transformation(b43_1, minimum = 3, maximum = 1),
  ssb2 = scale_transformation(b43_3, minimum = 3, maximum = 1),
  ssb3 = scale_transformation(b43_2, minimum = 3, maximum = 1),
  ssb4 = scale_transformation(b43_5, minimum = 3, maximum = 1),
  ssb5 = scale_transformation(b43_4, minimum = 3, maximum = 1),
  spi = {
    t <- list(
      scale_transformation(b49_1, minimum = 4, maximum = 1),
      scale_transformation(b49_2, minimum = 4, maximum = 1),
      scale_transformation(b49_3, minimum = 4, maximum = 1),
      scale_transformation(b49_4, minimum = 4, maximum = 1),
      scale_transformation(b49_5, minimum = 4, maximum = 1),
      scale_transformation(b49_6, minimum = 4, maximum = 1),
      scale_transformation(b49_7, minimum = 4, maximum = 1),
      scale_transformation(b49_8, minimum = 4, maximum = 1),
      scale_transformation(b49_9, minimum = 4, maximum = 1),
      fifelse(b49_11 == 5, NA_real_, (4 - b49_10) / 3), # ported bug: guards on b49_11, not b49_10
      scale_transformation(b49_11, minimum = 4, maximum = 1)
    )
    spi1 <- rowMeans(do.call(cbind, t), na.rm = TRUE)
    fifelse(!is.na(spi1) & spi1 > 10 / 11, 1, 0)
  },
  nei1 = scale_transformation(b51_1, minimum = 3, maximum = 1),
  nei2 = scale_transformation(b51_2, minimum = 3, maximum = 1),
  nei3 = scale_transformation(b51_3, minimum = 3, maximum = 1),
  nei4 = scale_transformation(b51_4, minimum = 3, maximum = 1)
)]

## 2011 ----
# Column numbering shifts by one throughout (an extra "b8" question was added), the
# shm domain gets a new source item (b22_3), and od1/od3/od6 gain an explicit NA-guard
# for raw value 3 before their normal recode.
swbi_2011 <- data_2011[, .(
  unique_id = paste(formno, fertsirano, sep = "_"),
  weight = ff,
  od1 = rowmean2(scale_transformation(h15_1, minimum = 1, maximum = 2), scale_transformation(h15_10, minimum = 1, maximum = 2)),
  od2 = rowmean2(scale_transformation(h15_2, minimum = 2, maximum = 1), scale_transformation(h15_11, minimum = 2, maximum = 1)),
  od3 = rowmean2(scale_transformation(h15_3, minimum = 2, maximum = 1), scale_transformation(h15_12, minimum = 2, maximum = 1)),
  od4 = rowmean2(scale_transformation(h15_4, minimum = 2, maximum = 1), scale_transformation(h15_13, minimum = 2, maximum = 1)),
  od5 = rowmean2(scale_transformation(h15_5, minimum = 2, maximum = 1), scale_transformation(h15_14, minimum = 2, maximum = 1)),
  od6 = rowmean2(scale_transformation(fifelse(h15_6 == 3, NA_real_, h15_6), minimum = 1, maximum = 2), scale_transformation(fifelse(h15_15 == 3, NA_real_, h15_15), minimum = 1, maximum = 2)),
  od7 = rowmean2(scale_transformation(h15_7, minimum = 3, maximum = 1), scale_transformation(h15_16, minimum = 3, maximum = 1)),
  od8 = rowmean2(scale_transformation(h15_8, minimum = 3, maximum = 1), scale_transformation(h15_17, minimum = 3, maximum = 1)),
  od9 = rowmean2(scale_transformation(h15_9, minimum = 3, maximum = 1), scale_transformation(h15_18, minimum = 3, maximum = 1)),
  hhg = scale_transformation(h17, minimum = 1, maximum = 6),
  hhgy = scale_transformation(h18),
  ym = scale_transformation(b9),
  sm = scale_transformation(b12_1),
  evs = scale_transformation(b12_2),
  egs = scale_transformation(b12_3),
  okm = scale_transformation(b12_4),
  semt = scale_transformation(b12_5),
  ism = scale_transformation(b12_6),
  isgm = scale_transformation(b12_7),
  hhg2 = scale_transformation(b12_8),
  tul = NA_real_,
  tuc = NA_real_,
  ark = scale_transformation(b13_3),
  kom = scale_transformation(b13_4),
  isam = scale_transformation(b13_5),
  issm = scale_transformation(b16_3, minimum = 1, maximum = 2),
  isck = scale_transformation(b16_4, minimum = 1, maximum = 2),
  sshm = scale_transformation(b14_1),
  sghm = scale_transformation(b14_2),
  sahm = scale_transformation(b14_3),
  sehm = scale_transformation(b14_4),
  sihm = scale_transformation(b14_6),
  bel1 = fcase(b18_1 == 1, 1, b18_1 == 2, 0.25, b18_1 == 4, 0, default = NA_real_),
  bel2 = rowmean2(
    fcase(b18_2 == 1, 1, b18_2 == 2, 0.25, b18_2 == 4, 0, default = NA_real_),
    fcase(b19_1 == 1, 1, b19_1 == 2, 0.25, b19_1 == 4, 0, default = NA_real_)
  ),
  bel3 = rowmean2(
    fcase(b18_3 == 1, 1, b18_3 == 2, 0.25, b18_3 == 4, 0, default = NA_real_),
    fcase(b19_2 == 1, 1, b19_2 == 2, 0.25, b19_2 == 4, 0, default = NA_real_)
  ),
  bel4 = fcase(b18_4 == 1, 1, b18_4 == 2, 0.25, b18_4 == 4, 0, default = NA_real_),
  bel5 = fcase(b18_5 == 1, 1, b18_5 == 2, 0.25, b18_5 == 4, 0, default = NA_real_),
  bel6 = rowmean2(
    fcase(b18_6 == 1, 1, b18_6 == 2, 0.25, b18_6 == 4, 0, default = NA_real_),
    fcase(b19_3 == 1, 1, b19_3 == 2, 0.25, b19_3 == 4, 0, default = NA_real_)
  ),
  bel7 = fcase(b18_7 == 1, 1, b18_7 == 2, 0.25, b18_7 == 4, 0, default = NA_real_),
  bel8 = fcase(b18_8 == 1, 1, b18_8 == 2, 0.25, b18_8 == 4, 0, default = NA_real_),
  bel9 = rowmean2(
    fcase(b18_14 == 1, 1, b18_14 == 2, 0.25, b18_14 == 4, 0, default = NA_real_),
    fcase(b19_8 == 1, 1, b19_8 == 2, 0.25, b19_8 == 4, 0, default = NA_real_)
  ),
  bel10 = rowmean2(
    fcase(b18_13 == 1, 1, b18_13 == 2, 0.25, b18_13 == 4, 0, default = NA_real_),
    fcase(b19_7 == 1, 1, b19_7 == 2, 0.25, b19_7 == 4, 0, default = NA_real_)
  ),
  bel11 = rowmean2(
    fcase(b18_12 == 1, 1, b18_12 == 2, 0.25, b18_12 == 4, 0, default = NA_real_),
    fcase(b19_6 == 1, 1, b19_6 == 2, 0.25, b19_6 == 4, 0, default = NA_real_)
  ),
  bel12 = fcase(b18_9 == 1, 1, b18_9 == 2, 0.25, b18_9 == 4, 0, default = NA_real_),
  bel13 = rowmean2(
    fcase(b18_10 == 1, 1, b18_10 == 2, 0.25, b18_10 == 4, 0, default = NA_real_),
    fcase(b19_4 == 1, 1, b19_4 == 2, 0.25, b19_4 == 4, 0, default = NA_real_)
  ),
  shm1 = scale_transformation(b28_1, minimum = 1, maximum = 2),
  shm2 = scale_transformation(b22_3, minimum = 1, maximum = 2),
  shm3 = scale_transformation(b28_2, minimum = 1, maximum = 2),
  shm4 = scale_transformation(b28_3, minimum = 2, maximum = 1),
  shm5 = scale_transformation(b28_4, minimum = 1, maximum = 2),
  shm6 = scale_transformation(b28_5, minimum = 1, maximum = 2),
  shm7 = scale_transformation(b28_9, minimum = 1, maximum = 2),
  shm8 = scale_transformation(b28_7, minimum = 2, maximum = 1),
  shm9 = scale_transformation(b28_8, minimum = 1, maximum = 2),
  ah = scale_transformation(b34_3, minimum = 3, maximum = 1),
  guv1 = scale_transformation(b38),
  guv2 = scale_transformation(b39),
  ud = scale_transformation(fifelse(b40 >= 3, b40 + 1, b40)),
  sw = scale_transformation(b41, minimum = 1, maximum = 11),
  ssb1 = scale_transformation(b44_1, minimum = 3, maximum = 1),
  ssb2 = scale_transformation(b44_3, minimum = 3, maximum = 1),
  ssb3 = scale_transformation(b44_2, minimum = 3, maximum = 1),
  ssb4 = scale_transformation(b44_5, minimum = 3, maximum = 1),
  ssb5 = scale_transformation(b44_4, minimum = 3, maximum = 1),
  spi = {
    t <- list(
      scale_transformation(b50_1, minimum = 4, maximum = 1),
      scale_transformation(b50_2, minimum = 4, maximum = 1),
      scale_transformation(b50_3, minimum = 4, maximum = 1),
      scale_transformation(b50_4, minimum = 4, maximum = 1),
      scale_transformation(b50_5, minimum = 4, maximum = 1),
      scale_transformation(b50_6, minimum = 4, maximum = 1),
      scale_transformation(b50_7, minimum = 4, maximum = 1),
      scale_transformation(b50_8, minimum = 4, maximum = 1),
      scale_transformation(b50_9, minimum = 4, maximum = 1),
      fifelse(b50_11 == 5, NA_real_, (4 - b50_10) / 3), # ported bug: guards on b50_11, not b50_10
      scale_transformation(b50_11, minimum = 4, maximum = 1)
    )
    spi1 <- rowMeans(do.call(cbind, t), na.rm = TRUE)
    fifelse(!is.na(spi1) & spi1 > 10 / 11, 1, 0)
  },
  nei1 = scale_transformation(b52_1, minimum = 3, maximum = 1),
  nei2 = scale_transformation(b52_2, minimum = 3, maximum = 1),
  nei3 = scale_transformation(b52_3, minimum = 3, maximum = 1),
  nei4 = scale_transformation(b52_4, minimum = 3, maximum = 1)
)]

## 2012 ----
# Another full renumbering; sihm draws from b13_6 (not b13_5 like every other year).
swbi_2012 <- data_2012[, .(
  unique_id = paste(formno, fertsirano, sep = "_"),
  weight = ff,
  od1 = rowmean2(scale_transformation(h15_1, minimum = 1, maximum = 2), scale_transformation(h15_10, minimum = 1, maximum = 2)),
  od2 = rowmean2(scale_transformation(h15_2, minimum = 2, maximum = 1), scale_transformation(h15_11, minimum = 2, maximum = 1)),
  od3 = rowmean2(scale_transformation(h15_3, minimum = 2, maximum = 1), scale_transformation(h15_12, minimum = 2, maximum = 1)),
  od4 = rowmean2(scale_transformation(h15_4, minimum = 2, maximum = 1), scale_transformation(h15_13, minimum = 2, maximum = 1)),
  od5 = rowmean2(scale_transformation(h15_5, minimum = 2, maximum = 1), scale_transformation(h15_14, minimum = 2, maximum = 1)),
  od6 = rowmean2(scale_transformation(fifelse(h15_6 == 3, NA_real_, h15_6), minimum = 1, maximum = 2), scale_transformation(fifelse(h15_15 == 3, NA_real_, h15_15), minimum = 1, maximum = 2)),
  od7 = rowmean2(scale_transformation(h15_7, minimum = 3, maximum = 1), scale_transformation(h15_16, minimum = 3, maximum = 1)),
  od8 = rowmean2(scale_transformation(h15_8, minimum = 3, maximum = 1), scale_transformation(h15_17, minimum = 3, maximum = 1)),
  od9 = rowmean2(scale_transformation(h15_9, minimum = 3, maximum = 1), scale_transformation(h15_18, minimum = 3, maximum = 1)),
  hhg = scale_transformation(h17, minimum = 1, maximum = 6),
  hhgy = scale_transformation(h18),
  ym = scale_transformation(b08),
  sm = scale_transformation(b11_1),
  evs = scale_transformation(b11_2),
  egs = scale_transformation(b11_3),
  okm = scale_transformation(b11_4),
  semt = scale_transformation(b11_5),
  ism = scale_transformation(b11_6),
  isgm = scale_transformation(b11_7),
  hhg2 = scale_transformation(b11_8),
  tul = NA_real_,
  tuc = NA_real_,
  ark = scale_transformation(b12_2),
  kom = scale_transformation(b12_3),
  isam = scale_transformation(b12_4),
  issm = scale_transformation(b15_3, minimum = 1, maximum = 2),
  isck = scale_transformation(b15_4, minimum = 1, maximum = 2),
  sshm = scale_transformation(b13_1),
  sghm = scale_transformation(b13_2),
  sahm = scale_transformation(b13_3),
  sehm = scale_transformation(b13_4),
  sihm = scale_transformation(b13_6),
  bel1 = fcase(b17_1 == 1, 1, b17_1 == 2, 0.25, b17_1 == 4, 0, default = NA_real_),
  bel2 = rowmean2(
    fcase(b17_2 == 1, 1, b17_2 == 2, 0.25, b17_2 == 4, 0, default = NA_real_),
    fcase(b18_1 == 1, 1, b18_1 == 2, 0.25, b18_1 == 4, 0, default = NA_real_)
  ),
  bel3 = rowmean2(
    fcase(b17_3 == 1, 1, b17_3 == 2, 0.25, b17_3 == 4, 0, default = NA_real_),
    fcase(b18_2 == 1, 1, b18_2 == 2, 0.25, b18_2 == 4, 0, default = NA_real_)
  ),
  bel4 = fcase(b17_4 == 1, 1, b17_4 == 2, 0.25, b17_4 == 4, 0, default = NA_real_),
  bel5 = fcase(b17_5 == 1, 1, b17_5 == 2, 0.25, b17_5 == 4, 0, default = NA_real_),
  bel6 = rowmean2(
    fcase(b17_6 == 1, 1, b17_6 == 2, 0.25, b17_6 == 4, 0, default = NA_real_),
    fcase(b18_3 == 1, 1, b18_3 == 2, 0.25, b18_3 == 4, 0, default = NA_real_)
  ),
  bel7 = fcase(b17_7 == 1, 1, b17_7 == 2, 0.25, b17_7 == 4, 0, default = NA_real_),
  bel8 = fcase(b17_8 == 1, 1, b17_8 == 2, 0.25, b17_8 == 4, 0, default = NA_real_),
  bel9 = rowmean2(
    fcase(b17_14 == 1, 1, b17_14 == 2, 0.25, b17_14 == 4, 0, default = NA_real_),
    fcase(b18_8 == 1, 1, b18_8 == 2, 0.25, b18_8 == 4, 0, default = NA_real_)
  ),
  bel10 = rowmean2(
    fcase(b17_13 == 1, 1, b17_13 == 2, 0.25, b17_13 == 4, 0, default = NA_real_),
    fcase(b18_7 == 1, 1, b18_7 == 2, 0.25, b18_7 == 4, 0, default = NA_real_)
  ),
  bel11 = rowmean2(
    fcase(b17_12 == 1, 1, b17_12 == 2, 0.25, b17_12 == 4, 0, default = NA_real_),
    fcase(b18_6 == 1, 1, b18_6 == 2, 0.25, b18_6 == 4, 0, default = NA_real_)
  ),
  bel12 = fcase(b17_9 == 1, 1, b17_9 == 2, 0.25, b17_9 == 4, 0, default = NA_real_),
  bel13 = rowmean2(
    fcase(b17_10 == 1, 1, b17_10 == 2, 0.25, b17_10 == 4, 0, default = NA_real_),
    fcase(b18_4 == 1, 1, b18_4 == 2, 0.25, b18_4 == 4, 0, default = NA_real_)
  ),
  shm1 = scale_transformation(b27_1, minimum = 1, maximum = 2),
  shm2 = scale_transformation(b21_3, minimum = 1, maximum = 2),
  shm3 = scale_transformation(b27_2, minimum = 1, maximum = 2),
  shm4 = scale_transformation(b27_3, minimum = 2, maximum = 1),
  shm5 = scale_transformation(b27_4, minimum = 1, maximum = 2),
  shm6 = scale_transformation(b27_5, minimum = 1, maximum = 2),
  shm7 = scale_transformation(b27_8, minimum = 1, maximum = 2),
  shm8 = scale_transformation(b27_6, minimum = 2, maximum = 1),
  shm9 = scale_transformation(b27_7, minimum = 1, maximum = 2),
  ah = scale_transformation(b33_3, minimum = 3, maximum = 1),
  guv1 = scale_transformation(b37),
  guv2 = scale_transformation(b38),
  ud = scale_transformation(fifelse(b39 >= 3, b39 + 1, b39)),
  sw = scale_transformation(b40, minimum = 1, maximum = 11),
  ssb1 = scale_transformation(b43_1, minimum = 3, maximum = 1),
  ssb2 = scale_transformation(b43_3, minimum = 3, maximum = 1),
  ssb3 = scale_transformation(b43_2, minimum = 3, maximum = 1),
  ssb4 = scale_transformation(b43_5, minimum = 3, maximum = 1),
  ssb5 = scale_transformation(b43_4, minimum = 3, maximum = 1),
  spi = {
    t <- list(
      scale_transformation(b52_1, minimum = 4, maximum = 1),
      scale_transformation(b52_2, minimum = 4, maximum = 1),
      scale_transformation(b52_3, minimum = 4, maximum = 1),
      scale_transformation(b52_4, minimum = 4, maximum = 1),
      scale_transformation(b52_5, minimum = 4, maximum = 1),
      scale_transformation(b52_6, minimum = 4, maximum = 1),
      scale_transformation(b52_7, minimum = 4, maximum = 1),
      scale_transformation(b52_8, minimum = 4, maximum = 1),
      scale_transformation(b52_9, minimum = 4, maximum = 1),
      fifelse(b52_11 == 5, NA_real_, (4 - b52_10) / 3), # ported bug: guards on b52_11, not b52_10
      scale_transformation(b52_11, minimum = 4, maximum = 1)
    )
    spi1 <- rowMeans(do.call(cbind, t), na.rm = TRUE)
    fifelse(!is.na(spi1) & spi1 > 10 / 11, 1, 0)
  },
  nei1 = scale_transformation(b46_1, minimum = 3, maximum = 1),
  nei2 = scale_transformation(b46_2, minimum = 3, maximum = 1),
  nei3 = scale_transformation(b46_3, minimum = 3, maximum = 1),
  nei4 = scale_transformation(b46_4, minimum = 3, maximum = 1)
)]

## 2013 ----
# The municipal-services (bel) scale changes to 1-5 + "Has No Idea" (6) + "Not served" (7); tul/tuc get real formulas for the first time.
swbi_2013 <- data_2013[, .(
  unique_id = paste(birimno, rowid(birimno), sep = "_"),
  weight = ff,
  od1 = rowmean2(scale_transformation(fifelse(h16_1 == 3, NA_real_, h16_1), minimum = 1, maximum = 2), scale_transformation(fifelse(h17_1 == 3, NA_real_, h17_1), minimum = 1, maximum = 2)),
  od2 = rowmean2(scale_transformation(h16_2, minimum = 2, maximum = 1), scale_transformation(h17_2, minimum = 2, maximum = 1)),
  od3 = rowmean2(scale_transformation(fifelse(h16_3 == 3, NA_real_, h16_3), minimum = 2, maximum = 1), scale_transformation(fifelse(h17_3 == 3, NA_real_, h17_3), minimum = 2, maximum = 1)),
  od4 = rowmean2(scale_transformation(h16_4, minimum = 2, maximum = 1), scale_transformation(h17_4, minimum = 2, maximum = 1)),
  od5 = rowmean2(scale_transformation(h16_5, minimum = 2, maximum = 1), scale_transformation(h17_5, minimum = 2, maximum = 1)),
  od6 = rowmean2(scale_transformation(fifelse(h16_6 == 3, NA_real_, h16_6), minimum = 1, maximum = 2), scale_transformation(fifelse(h17_6 == 3, NA_real_, h17_6), minimum = 1, maximum = 2)),
  od7 = rowmean2(scale_transformation(h16_7, minimum = 3, maximum = 1), scale_transformation(h17_7, minimum = 3, maximum = 1)),
  od8 = rowmean2(scale_transformation(h16_8, minimum = 3, maximum = 1), scale_transformation(h17_8, minimum = 3, maximum = 1)),
  od9 = rowmean2(scale_transformation(h16_9, minimum = 3, maximum = 1), scale_transformation(h17_9, minimum = 3, maximum = 1)),
  hhg = scale_transformation(h19, minimum = 1, maximum = 6),
  hhgy = scale_transformation(h20),
  ym = scale_transformation(b9),
  sm = scale_transformation(b12_1),
  evs = scale_transformation(b12_2),
  egs = scale_transformation(b12_3),
  okm = scale_transformation(b12_4),
  semt = scale_transformation(b12_5),
  ism = scale_transformation(b12_6),
  isgm = scale_transformation(b12_7),
  hhg2 = scale_transformation(b12_8),
  tul = scale_transformation(b12_10),
  tuc = scale_transformation(b12_11),
  ark = scale_transformation(b13_2),
  kom = scale_transformation(b13_3),
  isam = scale_transformation(b13_4),
  issm = scale_transformation(b8_3, minimum = 1, maximum = 2),
  isck = scale_transformation(b8_4, minimum = 1, maximum = 2),
  sshm = scale_transformation(b14_1),
  sghm = scale_transformation(b14_2),
  sahm = scale_transformation(b14_3),
  sehm = scale_transformation(b14_4),
  sihm = scale_transformation(b14_6),
  bel1 = scale_transformation(fifelse(b18_1 == 7, 5, b18_1)),
  bel2 = rowmean2(scale_transformation(fifelse(b18_2 == 7, 5, b18_2)), scale_transformation(fifelse(b19_1 == 7, 5, b19_1))),
  bel3 = rowmean2(scale_transformation(fifelse(b18_3 == 7, 5, b18_3)), scale_transformation(fifelse(b19_2 == 7, 5, b19_2))),
  bel4 = scale_transformation(fifelse(b18_4 == 7, 5, b18_4)),
  bel5 = scale_transformation(fifelse(b18_5 == 7, 5, b18_5)),
  bel6 = rowmean2(scale_transformation(fifelse(b18_6 == 7, 5, b18_6)), scale_transformation(fifelse(b19_3 == 7, 5, b19_3))),
  bel7 = scale_transformation(fifelse(b18_7 == 7, 5, b18_7)),
  bel8 = scale_transformation(fifelse(b18_8 == 7, 5, b18_8)),
  bel9 = rowmean2(scale_transformation(fifelse(b18_14 == 7, 5, b18_14)), scale_transformation(fifelse(b19_8 == 7, 5, b19_8))),
  bel10 = rowmean2(scale_transformation(fifelse(b18_13 == 7, 5, b18_13)), scale_transformation(fifelse(b19_7 == 7, 5, b19_7))),
  bel11 = rowmean2(scale_transformation(fifelse(b18_12 == 7, 5, b18_12)), scale_transformation(fifelse(b19_6 == 7, 5, b19_6))),
  bel12 = scale_transformation(fifelse(b18_9 == 7, 5, b18_9)),
  bel13 = rowmean2(scale_transformation(fifelse(b18_10 == 7, 5, b18_10)), scale_transformation(fifelse(b19_4 == 7, 5, b19_4))),
  shm1 = scale_transformation(b30_1, minimum = 1, maximum = 2),
  shm2 = scale_transformation(b22_3, minimum = 1, maximum = 2),
  shm3 = scale_transformation(b30_2, minimum = 1, maximum = 2),
  shm4 = scale_transformation(b30_3, minimum = 2, maximum = 1),
  shm5 = scale_transformation(b30_4, minimum = 1, maximum = 2),
  shm6 = scale_transformation(b30_5, minimum = 1, maximum = 2),
  shm7 = scale_transformation(b30_8, minimum = 1, maximum = 2),
  shm8 = scale_transformation(b30_6, minimum = 2, maximum = 1),
  shm9 = scale_transformation(b30_7, minimum = 1, maximum = 2),
  ah = scale_transformation(b34_3, minimum = 3, maximum = 1),
  guv1 = scale_transformation(b38),
  guv2 = scale_transformation(b39),
  ud = scale_transformation(fifelse(b40 >= 3, b40 + 1, b40)),
  sw = scale_transformation(b41, minimum = 1, maximum = 11),
  ssb1 = scale_transformation(b44_1, minimum = 3, maximum = 1),
  ssb2 = scale_transformation(b44_3, minimum = 3, maximum = 1),
  ssb3 = scale_transformation(b44_2, minimum = 3, maximum = 1),
  ssb4 = scale_transformation(b44_5, minimum = 3, maximum = 1),
  ssb5 = scale_transformation(b44_4, minimum = 3, maximum = 1),
  spi = {
    t <- list(
      scale_transformation(b53_1, minimum = 4, maximum = 1),
      scale_transformation(b53_2, minimum = 4, maximum = 1),
      scale_transformation(b53_3, minimum = 4, maximum = 1),
      scale_transformation(b53_4, minimum = 4, maximum = 1),
      scale_transformation(b53_5, minimum = 4, maximum = 1),
      scale_transformation(b53_6, minimum = 4, maximum = 1),
      scale_transformation(b53_7, minimum = 4, maximum = 1),
      scale_transformation(b53_8, minimum = 4, maximum = 1),
      scale_transformation(b53_9, minimum = 4, maximum = 1),
      fifelse(b53_11 == 5, NA_real_, (4 - b53_10) / 3), # ported bug: guards on b53_11, not b53_10
      scale_transformation(b53_11, minimum = 4, maximum = 1)
    )
    spi1 <- rowMeans(do.call(cbind, t), na.rm = TRUE)
    fifelse(!is.na(spi1) & spi1 > 10 / 11, 1, 0)
  },
  nei1 = scale_transformation(b47_1, minimum = 3, maximum = 1),
  nei2 = scale_transformation(b47_2, minimum = 3, maximum = 1),
  nei3 = scale_transformation(b47_3, minimum = 3, maximum = 1),
  nei4 = scale_transformation(b47_4, minimum = 3, maximum = 1)
)]

## 2014 ----
# Same layout as 2013 (h16_/h17_ renamed h16a_/h17a_); bel7 gains a second ("private
# provider") component this year (b19_12), unlike every other year where bel7 is a
# single item.
swbi_2014 <- data_2014[, .(
  unique_id = paste(birimno, rowid(birimno), sep = "_"),
  weight = ff,
  od1 = rowmean2(scale_transformation(fifelse(h16a_1 == 3, NA_real_, h16a_1), minimum = 1, maximum = 2), scale_transformation(fifelse(h17a_1 == 3, NA_real_, h17a_1), minimum = 1, maximum = 2)),
  od2 = rowmean2(scale_transformation(h16a_2, minimum = 2, maximum = 1), scale_transformation(h17a_2, minimum = 2, maximum = 1)),
  od3 = rowmean2(scale_transformation(fifelse(h16a_3 == 3, NA_real_, h16a_3), minimum = 2, maximum = 1), scale_transformation(fifelse(h17a_3 == 3, NA_real_, h17a_3), minimum = 2, maximum = 1)),
  od4 = rowmean2(scale_transformation(h16a_4, minimum = 2, maximum = 1), scale_transformation(h17a_4, minimum = 2, maximum = 1)),
  od5 = rowmean2(scale_transformation(h16a_5, minimum = 2, maximum = 1), scale_transformation(h17a_5, minimum = 2, maximum = 1)),
  od6 = rowmean2(scale_transformation(fifelse(h16a_6 == 3, NA_real_, h16a_6), minimum = 1, maximum = 2), scale_transformation(fifelse(h17a_6 == 3, NA_real_, h17a_6), minimum = 1, maximum = 2)),
  od7 = rowmean2(scale_transformation(h16a_7, minimum = 3, maximum = 1), scale_transformation(h17a_7, minimum = 3, maximum = 1)),
  od8 = rowmean2(scale_transformation(h16a_8, minimum = 3, maximum = 1), scale_transformation(h17a_8, minimum = 3, maximum = 1)),
  od9 = rowmean2(scale_transformation(h16a_9, minimum = 3, maximum = 1), scale_transformation(h17a_9, minimum = 3, maximum = 1)),
  hhg = scale_transformation(h19, minimum = 1, maximum = 6),
  hhgy = scale_transformation(h20),
  ym = scale_transformation(b9),
  sm = scale_transformation(b12_1),
  evs = scale_transformation(b12_2),
  egs = scale_transformation(b12_3),
  okm = scale_transformation(b12_4),
  semt = scale_transformation(b12_5),
  ism = scale_transformation(b12_6),
  isgm = scale_transformation(b12_7),
  hhg2 = scale_transformation(b12_8),
  tul = scale_transformation(b12_10),
  tuc = scale_transformation(b12_11),
  ark = scale_transformation(b13_2),
  kom = scale_transformation(b13_3),
  isam = scale_transformation(b13_4),
  issm = scale_transformation(b8_3, minimum = 1, maximum = 2),
  isck = scale_transformation(b8_4, minimum = 1, maximum = 2),
  sshm = scale_transformation(b14_1),
  sghm = scale_transformation(b14_2),
  sahm = scale_transformation(b14_3),
  sehm = scale_transformation(b14_4),
  sihm = scale_transformation(b14_6),
  bel1 = scale_transformation(fifelse(b18_1 == 7, 5, b18_1)),
  bel2 = rowmean2(scale_transformation(fifelse(b18_2 == 7, 5, b18_2)), scale_transformation(fifelse(b19_1 == 7, 5, b19_1))),
  bel3 = rowmean2(scale_transformation(fifelse(b18_3 == 7, 5, b18_3)), scale_transformation(fifelse(b19_2 == 7, 5, b19_2))),
  bel4 = scale_transformation(fifelse(b18_4 == 7, 5, b18_4)),
  bel5 = scale_transformation(fifelse(b18_5 == 7, 5, b18_5)),
  bel6 = rowmean2(scale_transformation(fifelse(b18_6 == 7, 5, b18_6)), scale_transformation(fifelse(b19_3 == 7, 5, b19_3))),
  bel7 = rowmean2(scale_transformation(fifelse(b18_7 == 7, 5, b18_7)), scale_transformation(fifelse(b19_12 == 7, 5, b19_12))),
  bel8 = scale_transformation(fifelse(b18_8 == 7, 5, b18_8)),
  bel9 = rowmean2(scale_transformation(fifelse(b18_14 == 7, 5, b18_14)), scale_transformation(fifelse(b19_8 == 7, 5, b19_8))),
  bel10 = rowmean2(scale_transformation(fifelse(b18_13 == 7, 5, b18_13)), scale_transformation(fifelse(b19_7 == 7, 5, b19_7))),
  bel11 = rowmean2(scale_transformation(fifelse(b18_12 == 7, 5, b18_12)), scale_transformation(fifelse(b19_6 == 7, 5, b19_6))),
  bel12 = scale_transformation(fifelse(b18_9 == 7, 5, b18_9)),
  bel13 = rowmean2(scale_transformation(fifelse(b18_10 == 7, 5, b18_10)), scale_transformation(fifelse(b19_4 == 7, 5, b19_4))),
  shm1 = scale_transformation(b30_1, minimum = 1, maximum = 2),
  shm2 = scale_transformation(b22_3, minimum = 1, maximum = 2),
  shm3 = scale_transformation(b30_2, minimum = 1, maximum = 2),
  shm4 = scale_transformation(b30_3, minimum = 2, maximum = 1),
  shm5 = scale_transformation(b30_4, minimum = 1, maximum = 2),
  shm6 = scale_transformation(b30_5, minimum = 1, maximum = 2),
  shm7 = scale_transformation(b30_8, minimum = 1, maximum = 2),
  shm8 = scale_transformation(b30_6, minimum = 2, maximum = 1),
  shm9 = scale_transformation(b30_7, minimum = 1, maximum = 2),
  ah = scale_transformation(b34_3, minimum = 3, maximum = 1),
  guv1 = scale_transformation(b38),
  guv2 = scale_transformation(b39),
  ud = scale_transformation(fifelse(b40 >= 3, b40 + 1, b40)),
  sw = scale_transformation(b41, minimum = 1, maximum = 11),
  ssb1 = scale_transformation(b44_1, minimum = 3, maximum = 1),
  ssb2 = scale_transformation(b44_3, minimum = 3, maximum = 1),
  ssb3 = scale_transformation(b44_2, minimum = 3, maximum = 1),
  ssb4 = scale_transformation(b44_5, minimum = 3, maximum = 1),
  ssb5 = scale_transformation(b44_4, minimum = 3, maximum = 1),
  spi = {
    t <- list(
      scale_transformation(b53_1, minimum = 4, maximum = 1),
      scale_transformation(b53_2, minimum = 4, maximum = 1),
      scale_transformation(b53_3, minimum = 4, maximum = 1),
      scale_transformation(b53_4, minimum = 4, maximum = 1),
      scale_transformation(b53_5, minimum = 4, maximum = 1),
      scale_transformation(b53_6, minimum = 4, maximum = 1),
      scale_transformation(b53_7, minimum = 4, maximum = 1),
      scale_transformation(b53_8, minimum = 4, maximum = 1),
      scale_transformation(b53_9, minimum = 4, maximum = 1),
      fifelse(b53_11 == 5, NA_real_, (4 - b53_10) / 3), # ported bug: guards on b53_11, not b53_10
      scale_transformation(b53_11, minimum = 4, maximum = 1)
    )
    spi1 <- rowMeans(do.call(cbind, t), na.rm = TRUE)
    fifelse(!is.na(spi1) & spi1 > 10 / 11, 1, 0)
  },
  nei1 = scale_transformation(b47_1, minimum = 3, maximum = 1),
  nei2 = scale_transformation(b47_2, minimum = 3, maximum = 1),
  nei3 = scale_transformation(b47_3, minimum = 3, maximum = 1),
  nei4 = scale_transformation(b47_4, minimum = 3, maximum = 1)
)]

## 2015-2017 ----
# GELIR_GRUP (hhg) is now a 1-5 scale (was 1-6) and UMUT (ud) is now a plain linear 4-point scale (was the shifted
# "skip 0.5" trick used 2004-2014); (c) direct inspection of data_2015/2016/2017.csv.
#
# od4, od5 gained a 3rd "irrelevant" category (excluded to NA), same as od1/od3/od6
# (which also carry a previously-unseen raw 3, confirmed to mean the same "irrelevant,
# exclude" thing, even though 2004-2014's .do files never guarded for it on those three items).
# od7, od8, od9, od10 (od10 is a brand-new item, "OKUL_GUVENLIK"/school safety, not
# present in 2004-2014) are a 1-3 scale, 1 = best, with any 4th "irrelevant" value
# excluded (only actually observed for od7/od9/od10 in the data; od8 never takes
# value 4 in 2015-2017, but the formula handles it either way).
# bel/io is now a 1-5 scale (1 = best), 6 = "Has No Idea" (excluded), 7 = "Not
# served" (recoded to 5, the worst score) -- identical to the scale already used for 2013/2014,
# so the same fifelse(x==7,5,x) + scale_transformation() pattern applies unchanged.
# All of the above fall out of scale_transformation()'s existing range-check behaviour
# (anything outside minimum:maximum becomes NA) without needing extra guards -- e.g.
# scale_transformation(DEV_OKUL_KAYIT, minimum=1, maximum=2) already excludes a raw 3
# automatically, since 3 %in% c(1,2) is FALSE.
#
# od10 is captured as its own field below but deliberately NOT folded into the "od"/
# "odb" composite formula in the Step 2 section -- those PCA weights were estimated
# without it and have no coefficient for a 10th item. Flag before relying on "od" for
# these years if it should be incorporated (and how).
#
# unique_id = BIRIMNO + FERT_NO (BIRIMNO alone repeats per household member, exactly
# like formno in every earlier year; the pair is confirmed unique with zero duplicates
# in all three years). weight = FAKTOR_FERT, the person-level weight, matching every
# earlier year's convention -- HANE_FAKTOR (household-level weight) also exists in
# these files but differs from FAKTOR_FERT in ~16-23% of rows, so they are genuinely
# different variables, not two names for the same thing.

swbi_2015 <- data_2015[, .(
  unique_id = paste(BIRIMNO, FERT_NO, sep = "_"),
  weight = FAKTOR_FERT,
  od1 = rowmean2(scale_transformation(DEV_OKUL_KAYIT, minimum = 1, maximum = 2), scale_transformation(OZEL_OKUL_KAYIT, minimum = 1, maximum = 2)),
  od2 = rowmean2(scale_transformation(DEV_EGT_KALIT, minimum = 2, maximum = 1), scale_transformation(OZEL_EGT_KALIT, minimum = 2, maximum = 1)),
  od3 = rowmean2(scale_transformation(DEV_EGT_ARAC, minimum = 2, maximum = 1), scale_transformation(OZEL_EGT_ARAC, minimum = 2, maximum = 1)),
  od4 = rowmean2(scale_transformation(DEV_OKUL_IDR, minimum = 2, maximum = 1), scale_transformation(OZEL_OKUL_IDR, minimum = 2, maximum = 1)),
  od5 = rowmean2(scale_transformation(DEV_OGRETMEN_YAKLASIM, minimum = 2, maximum = 1), scale_transformation(OZEL_OGR_YAK, minimum = 2, maximum = 1)),
  od6 = rowmean2(scale_transformation(DEV_SERVIS, minimum = 1, maximum = 2), scale_transformation(OZEL_SERVIS, minimum = 1, maximum = 2)),
  od7 = rowmean2(scale_transformation(DEV_OGRENCI_SAYISI, minimum = 3, maximum = 1), scale_transformation(OZEL_OGR_SAYI, minimum = 3, maximum = 1)),
  od8 = rowmean2(scale_transformation(DEV_EGT_MASRAF, minimum = 3, maximum = 1), scale_transformation(OZEL_EGT_MAS, minimum = 3, maximum = 1)),
  od9 = rowmean2(scale_transformation(DEV_OKUL_ISINMA, minimum = 3, maximum = 1), scale_transformation(OZEL_OKUL_ISIN, minimum = 3, maximum = 1)),
  od10 = rowmean2(scale_transformation(DEV_OKUL_GUVENLIK, minimum = 3, maximum = 1), scale_transformation(OZEL_OKUL_GUVENLIK, minimum = 3, maximum = 1)),
  hhg = scale_transformation(GELIR_GRUP, minimum = 1, maximum = 5),
  hhgy = scale_transformation(OLCEK_GELIR_KARSILAMA),
  ym = scale_transformation(MUTLULUK),
  sm = scale_transformation(OLCEK_MEMNUNIYET_SAGLIK),
  evs = scale_transformation(OLCEK_MEMNUNIYET_EVLILIK),
  egs = scale_transformation(OLCEK_MEMNUNIYET_EGITIM),
  okm = scale_transformation(OLCEK_MEMNUNIYET_KONUT),
  semt = scale_transformation(OLCEK_MEMNUNIYET_SEMT),
  ism = scale_transformation(OLCEK_MEMNUNIYET_IS),
  isgm = scale_transformation(OLCEK_MEMNUNIYET_KAZANC),
  hhg2 = scale_transformation(OLCEK_MEMNUNIYET_GELIR),
  tul = scale_transformation(OLCEK_MEMNUNIYET_KISISEL_BKM),
  tuc = scale_transformation(OLCEK_MEMNUNIYET_IS_TRFK_ZMN),
  ark = scale_transformation(OLCEK_MEMNUNIYET_ARKADAS),
  kom = scale_transformation(OLCEK_MEMNUNIYET_KOMSU),
  isam = scale_transformation(OLCEK_MEMNUNIYET_ISILISKI),
  issm = scale_transformation(ISTEKI_SORUN3, minimum = 1, maximum = 2),
  isck = scale_transformation(ISTEKI_SORUN4, minimum = 1, maximum = 2),
  sshm = scale_transformation(OLCEK_MEMNUNIYET_GN_SAG),
  sghm = scale_transformation(OLCEK_MEMNUNIYET_ASAYIS),
  sahm = scale_transformation(OLCEK_MEMNUNIYET_ADLI),
  sehm = scale_transformation(OLCEK_MEMNUNIYET_EGITI),
  sihm = scale_transformation(OLCEK_MEMNUNIYET_SGK),
  bel1 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_COP == 7, 5, MEMNUNIYET_COP)), scale_transformation(fifelse(MEMNUNIYET_IL_TEMIZLIK == 7, 5, MEMNUNIYET_IL_TEMIZLIK))),
  bel2 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_KAN == 7, 5, MEMNUNIYET_KAN)), scale_transformation(fifelse(MEMNUNIYET_IL_KANAL == 7, 5, MEMNUNIYET_IL_KANAL))),
  bel3 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_SU == 7, 5, MEMNUNIYET_SU)), scale_transformation(fifelse(MEMNUNIYET_IL_SU == 7, 5, MEMNUNIYET_IL_SU))),
  bel4 = scale_transformation(fifelse(MEMNUNIYET_TASIMA == 7, 5, MEMNUNIYET_TASIMA)),
  bel5 = scale_transformation(fifelse(MEMNUNIYET_ZABITA == 7, 5, MEMNUNIYET_ZABITA)),
  bel6 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_YOL == 7, 5, MEMNUNIYET_YOL)), scale_transformation(fifelse(MEMNUNIYET_IL_YOL == 7, 5, MEMNUNIYET_IL_YOL))),
  bel7 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_YESIL == 7, 5, MEMNUNIYET_YESIL)), scale_transformation(fifelse(MEMNUNIYET_IL_YESIL == 7, 5, MEMNUNIYET_IL_YESIL))),
  bel8 = scale_transformation(fifelse(MEMNUNIYET_KIRLILIK == 7, 5, MEMNUNIYET_KIRLILIK)),
  bel9 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_KURS == 7, 5, MEMNUNIYET_KURS)), scale_transformation(fifelse(MEMNUNIYET_IL_KURS == 7, 5, MEMNUNIYET_IL_KURS))),
  bel10 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_SERGI_FEST == 7, 5, MEMNUNIYET_SERGI_FEST)), scale_transformation(fifelse(MEMNUNIYET_IL_SERGI == 7, 5, MEMNUNIYET_IL_SERGI))),
  bel11 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_YARDIM == 7, 5, MEMNUNIYET_YARDIM)), scale_transformation(fifelse(MEMNUNIYET_IL_HASTA == 7, 5, MEMNUNIYET_IL_HASTA))),
  bel12 = scale_transformation(fifelse(MEMNUNIYET_SPOR_MRK == 7, 5, MEMNUNIYET_SPOR_MRK)),
  bel13 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_IMAR == 7, 5, MEMNUNIYET_IMAR)), scale_transformation(fifelse(MEMNUNIYET_IL_ISKAN == 7, 5, MEMNUNIYET_IL_ISKAN))),
  shm1 = scale_transformation(SORUN_MUAYNE, minimum = 1, maximum = 2),
  shm2 = scale_transformation(SORUN_SGK_PERSONEL, minimum = 1, maximum = 2),
  shm3 = scale_transformation(SORUN_HIJYEN, minimum = 1, maximum = 2),
  shm4 = scale_transformation(MEMNUNIYET_MUAYENE, minimum = 2, maximum = 1),
  shm5 = scale_transformation(SORUN_DOKTOR, minimum = 1, maximum = 2),
  shm6 = scale_transformation(SORUN_HEMSIRE, minimum = 1, maximum = 2),
  shm7 = scale_transformation(SORUN_ILAC_FIYAT, minimum = 1, maximum = 2),
  shm8 = scale_transformation(SORUN_YETERLI_SGLKPERSONEL, minimum = 2, maximum = 1),
  shm9 = scale_transformation(SORUN_UCRET_MUAYENE, minimum = 1, maximum = 2),
  ah = scale_transformation(SORUN_YASA, minimum = 3, maximum = 1),
  guv1 = scale_transformation(GUVEN_EV),
  guv2 = scale_transformation(GUVEN_CEVRE),
  ud = scale_transformation(UMUT, minimum = 4, maximum = 1),
  sw = scale_transformation(UMUT_BASAMAK, minimum = 1, maximum = 11),
  ssb1 = scale_transformation(GELECEK_HAYAT, minimum = 3, maximum = 1),
  ssb2 = scale_transformation(GELECEK_MALI, minimum = 3, maximum = 1),
  ssb3 = scale_transformation(GELECEK_KISISEL, minimum = 3, maximum = 1),
  ssb4 = scale_transformation(GELECEK_EKONOMI, minimum = 3, maximum = 1),
  ssb5 = scale_transformation(GELECEK_IS, minimum = 3, maximum = 1),
  spi = {
    t <- list(
      scale_transformation(CINSIYET_BASKI, minimum = 4, maximum = 1),
      scale_transformation(MEDENI_DURUM_BASKI, minimum = 4, maximum = 1),
      scale_transformation(YAS_BASKI, minimum = 4, maximum = 1),
      scale_transformation(GELENEK_GORENEK, minimum = 4, maximum = 1),
      scale_transformation(DINI_INANCDAN, minimum = 4, maximum = 1),
      scale_transformation(SIYASI_GORUSTEN, minimum = 4, maximum = 1),
      scale_transformation(MEMLEKETINDEN, minimum = 4, maximum = 1),
      scale_transformation(ISINDEN_DOLAYI, minimum = 4, maximum = 1),
      scale_transformation(KILIK_KIYAFET, minimum = 4, maximum = 1),
      scale_transformation(ISSIZ_OLMA, minimum = 4, maximum = 1),
      scale_transformation(GELIR_DUZEY_DURUM, minimum = 4, maximum = 1)
    )
    spi1 <- rowMeans(do.call(cbind, t), na.rm = TRUE)
    fifelse(!is.na(spi1) & spi1 > 10 / 11, 1, 0)
  },
  nei1 = scale_transformation(EKONOMIK_ACIDAN, minimum = 3, maximum = 1),
  nei2 = scale_transformation(SOSYAL_HAKLAR, minimum = 3, maximum = 1),
  nei3 = scale_transformation(KAMU_HIZMET_SUNUM, minimum = 3, maximum = 1),
  nei4 = scale_transformation(DEVLET_SEFFAF, minimum = 3, maximum = 1)
)]

# 2016: identical column names to 2015 (below), 2017: same names too (further below) --
# the household-level merge added a couple of extra individual-level questions in 2017
# (YASAM_MEMNUNIYET, OLCEK_MEMNUNIYET_SU_KALITE/YESIL_ALAN/TEMIZLIK, ISTEKI_SORUN
# renumbered from 5/6 to 5/6/7) that are not part of the SWBI variable set used here, so
# they don't affect this mapping.
swbi_2016 <- data_2016[, .(
  unique_id = paste(BIRIMNO, FERT_NO, sep = "_"),
  weight = FAKTOR_FERT,
  od1 = rowmean2(scale_transformation(DEV_OKUL_KAYIT, minimum = 1, maximum = 2), scale_transformation(OZEL_OKUL_KAYIT, minimum = 1, maximum = 2)),
  od2 = rowmean2(scale_transformation(DEV_EGT_KALIT, minimum = 2, maximum = 1), scale_transformation(OZEL_EGT_KALIT, minimum = 2, maximum = 1)),
  od3 = rowmean2(scale_transformation(DEV_EGT_ARAC, minimum = 2, maximum = 1), scale_transformation(OZEL_EGT_ARAC, minimum = 2, maximum = 1)),
  od4 = rowmean2(scale_transformation(DEV_OKUL_IDR, minimum = 2, maximum = 1), scale_transformation(OZEL_OKUL_IDR, minimum = 2, maximum = 1)),
  od5 = rowmean2(scale_transformation(DEV_OGRETMEN_YAKLASIM, minimum = 2, maximum = 1), scale_transformation(OZEL_OGR_YAK, minimum = 2, maximum = 1)),
  od6 = rowmean2(scale_transformation(DEV_SERVIS, minimum = 1, maximum = 2), scale_transformation(OZEL_SERVIS, minimum = 1, maximum = 2)),
  od7 = rowmean2(scale_transformation(DEV_OGRENCI_SAYISI, minimum = 3, maximum = 1), scale_transformation(OZEL_OGR_SAYI, minimum = 3, maximum = 1)),
  od8 = rowmean2(scale_transformation(DEV_EGT_MASRAF, minimum = 3, maximum = 1), scale_transformation(OZEL_EGT_MAS, minimum = 3, maximum = 1)),
  od9 = rowmean2(scale_transformation(DEV_OKUL_ISINMA, minimum = 3, maximum = 1), scale_transformation(OZEL_OKUL_ISIN, minimum = 3, maximum = 1)),
  od10 = rowmean2(scale_transformation(DEV_OKUL_GUVENLIK, minimum = 3, maximum = 1), scale_transformation(OZEL_OKUL_GUVENLIK, minimum = 3, maximum = 1)),
  hhg = scale_transformation(GELIR_GRUP, minimum = 1, maximum = 5),
  hhgy = scale_transformation(OLCEK_GELIR_KARSILAMA),
  ym = scale_transformation(MUTLULUK),
  sm = scale_transformation(OLCEK_MEMNUNIYET_SAGLIK),
  evs = scale_transformation(OLCEK_MEMNUNIYET_EVLILIK),
  egs = scale_transformation(OLCEK_MEMNUNIYET_EGITIM),
  okm = scale_transformation(OLCEK_MEMNUNIYET_KONUT),
  semt = scale_transformation(OLCEK_MEMNUNIYET_SEMT),
  ism = scale_transformation(OLCEK_MEMNUNIYET_IS),
  isgm = scale_transformation(OLCEK_MEMNUNIYET_KAZANC),
  hhg2 = scale_transformation(OLCEK_MEMNUNIYET_GELIR),
  tul = scale_transformation(OLCEK_MEMNUNIYET_KISISEL_BKM),
  tuc = scale_transformation(OLCEK_MEMNUNIYET_IS_TRFK_ZMN),
  ark = scale_transformation(OLCEK_MEMNUNIYET_ARKADAS),
  kom = scale_transformation(OLCEK_MEMNUNIYET_KOMSU),
  isam = scale_transformation(OLCEK_MEMNUNIYET_ISILISKI),
  issm = scale_transformation(ISTEKI_SORUN3, minimum = 1, maximum = 2),
  isck = scale_transformation(ISTEKI_SORUN4, minimum = 1, maximum = 2),
  sshm = scale_transformation(OLCEK_MEMNUNIYET_GN_SAG),
  sghm = scale_transformation(OLCEK_MEMNUNIYET_ASAYIS),
  sahm = scale_transformation(OLCEK_MEMNUNIYET_ADLI),
  sehm = scale_transformation(OLCEK_MEMNUNIYET_EGITI),
  sihm = scale_transformation(OLCEK_MEMNUNIYET_SGK),
  bel1 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_COP == 7, 5, MEMNUNIYET_COP)), scale_transformation(fifelse(MEMNUNIYET_IL_TEMIZLIK == 7, 5, MEMNUNIYET_IL_TEMIZLIK))),
  bel2 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_KAN == 7, 5, MEMNUNIYET_KAN)), scale_transformation(fifelse(MEMNUNIYET_IL_KANAL == 7, 5, MEMNUNIYET_IL_KANAL))),
  bel3 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_SU == 7, 5, MEMNUNIYET_SU)), scale_transformation(fifelse(MEMNUNIYET_IL_SU == 7, 5, MEMNUNIYET_IL_SU))),
  bel4 = scale_transformation(fifelse(MEMNUNIYET_TASIMA == 7, 5, MEMNUNIYET_TASIMA)),
  bel5 = scale_transformation(fifelse(MEMNUNIYET_ZABITA == 7, 5, MEMNUNIYET_ZABITA)),
  bel6 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_YOL == 7, 5, MEMNUNIYET_YOL)), scale_transformation(fifelse(MEMNUNIYET_IL_YOL == 7, 5, MEMNUNIYET_IL_YOL))),
  bel7 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_YESIL == 7, 5, MEMNUNIYET_YESIL)), scale_transformation(fifelse(MEMNUNIYET_IL_YESIL == 7, 5, MEMNUNIYET_IL_YESIL))),
  bel8 = scale_transformation(fifelse(MEMNUNIYET_KIRLILIK == 7, 5, MEMNUNIYET_KIRLILIK)),
  bel9 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_KURS == 7, 5, MEMNUNIYET_KURS)), scale_transformation(fifelse(MEMNUNIYET_IL_KURS == 7, 5, MEMNUNIYET_IL_KURS))),
  bel10 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_SERGI_FEST == 7, 5, MEMNUNIYET_SERGI_FEST)), scale_transformation(fifelse(MEMNUNIYET_IL_SERGI == 7, 5, MEMNUNIYET_IL_SERGI))),
  bel11 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_YARDIM == 7, 5, MEMNUNIYET_YARDIM)), scale_transformation(fifelse(MEMNUNIYET_IL_HASTA == 7, 5, MEMNUNIYET_IL_HASTA))),
  bel12 = scale_transformation(fifelse(MEMNUNIYET_SPOR_MRK == 7, 5, MEMNUNIYET_SPOR_MRK)),
  bel13 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_IMAR == 7, 5, MEMNUNIYET_IMAR)), scale_transformation(fifelse(MEMNUNIYET_IL_ISKAN == 7, 5, MEMNUNIYET_IL_ISKAN))),
  shm1 = scale_transformation(SORUN_MUAYNE, minimum = 1, maximum = 2),
  shm2 = scale_transformation(SORUN_SGK_PERSONEL, minimum = 1, maximum = 2),
  shm3 = scale_transformation(SORUN_HIJYEN, minimum = 1, maximum = 2),
  shm4 = scale_transformation(MEMNUNIYET_MUAYENE, minimum = 2, maximum = 1),
  shm5 = scale_transformation(SORUN_DOKTOR, minimum = 1, maximum = 2),
  shm6 = scale_transformation(SORUN_HEMSIRE, minimum = 1, maximum = 2),
  shm7 = scale_transformation(SORUN_ILAC_FIYAT, minimum = 1, maximum = 2),
  shm8 = scale_transformation(SORUN_YETERLI_SGLKPERSONEL, minimum = 2, maximum = 1),
  shm9 = scale_transformation(SORUN_UCRET_MUAYENE, minimum = 1, maximum = 2),
  ah = scale_transformation(SORUN_YASA, minimum = 3, maximum = 1),
  guv1 = scale_transformation(GUVEN_EV),
  guv2 = scale_transformation(GUVEN_CEVRE),
  ud = scale_transformation(UMUT, minimum = 4, maximum = 1),
  sw = scale_transformation(UMUT_BASAMAK, minimum = 1, maximum = 11),
  ssb1 = scale_transformation(GELECEK_HAYAT, minimum = 3, maximum = 1),
  ssb2 = scale_transformation(GELECEK_MALI, minimum = 3, maximum = 1),
  ssb3 = scale_transformation(GELECEK_KISISEL, minimum = 3, maximum = 1),
  ssb4 = scale_transformation(GELECEK_EKONOMI, minimum = 3, maximum = 1),
  ssb5 = scale_transformation(GELECEK_IS, minimum = 3, maximum = 1),
  spi = {
    t <- list(
      scale_transformation(CINSIYET_BASKI, minimum = 4, maximum = 1),
      scale_transformation(MEDENI_DURUM_BASKI, minimum = 4, maximum = 1),
      scale_transformation(YAS_BASKI, minimum = 4, maximum = 1),
      scale_transformation(GELENEK_GORENEK, minimum = 4, maximum = 1),
      scale_transformation(DINI_INANCDAN, minimum = 4, maximum = 1),
      scale_transformation(SIYASI_GORUSTEN, minimum = 4, maximum = 1),
      scale_transformation(MEMLEKETINDEN, minimum = 4, maximum = 1),
      scale_transformation(ISINDEN_DOLAYI, minimum = 4, maximum = 1),
      scale_transformation(KILIK_KIYAFET, minimum = 4, maximum = 1),
      scale_transformation(ISSIZ_OLMA, minimum = 4, maximum = 1),
      scale_transformation(GELIR_DUZEY_DURUM, minimum = 4, maximum = 1)
    )
    spi1 <- rowMeans(do.call(cbind, t), na.rm = TRUE)
    fifelse(!is.na(spi1) & spi1 > 10 / 11, 1, 0)
  },
  nei1 = scale_transformation(EKONOMIK_ACIDAN, minimum = 3, maximum = 1),
  nei2 = scale_transformation(SOSYAL_HAKLAR, minimum = 3, maximum = 1),
  nei3 = scale_transformation(KAMU_HIZMET_SUNUM, minimum = 3, maximum = 1),
  nei4 = scale_transformation(DEVLET_SEFFAF, minimum = 3, maximum = 1)
)]


# 2017: same column names as 2015/2016 (the SWBI Excel tab's U/2017 column was left
# blank, but data_2017.csv itself has an identical household-merge structure, verified
# directly -- no duplicate columns, and the individual/household tables' shared
# REFERANS_YIL.x/.y both read 2017 on every row).
swbi_2017 <- data_2017[, .(
  unique_id = paste(BIRIMNO, FERT_NO, sep = "_"),
  weight = FAKTOR_FERT,
  od1 = rowmean2(scale_transformation(DEV_OKUL_KAYIT, minimum = 1, maximum = 2), scale_transformation(OZEL_OKUL_KAYIT, minimum = 1, maximum = 2)),
  od2 = rowmean2(scale_transformation(DEV_EGT_KALIT, minimum = 2, maximum = 1), scale_transformation(OZEL_EGT_KALIT, minimum = 2, maximum = 1)),
  od3 = rowmean2(scale_transformation(DEV_EGT_ARAC, minimum = 2, maximum = 1), scale_transformation(OZEL_EGT_ARAC, minimum = 2, maximum = 1)),
  od4 = rowmean2(scale_transformation(DEV_OKUL_IDR, minimum = 2, maximum = 1), scale_transformation(OZEL_OKUL_IDR, minimum = 2, maximum = 1)),
  od5 = rowmean2(scale_transformation(DEV_OGRETMEN_YAKLASIM, minimum = 2, maximum = 1), scale_transformation(OZEL_OGR_YAK, minimum = 2, maximum = 1)),
  od6 = rowmean2(scale_transformation(DEV_SERVIS, minimum = 1, maximum = 2), scale_transformation(OZEL_SERVIS, minimum = 1, maximum = 2)),
  od7 = rowmean2(scale_transformation(DEV_OGRENCI_SAYISI, minimum = 3, maximum = 1), scale_transformation(OZEL_OGR_SAYI, minimum = 3, maximum = 1)),
  od8 = rowmean2(scale_transformation(DEV_EGT_MASRAF, minimum = 3, maximum = 1), scale_transformation(OZEL_EGT_MAS, minimum = 3, maximum = 1)),
  od9 = rowmean2(scale_transformation(DEV_OKUL_ISINMA, minimum = 3, maximum = 1), scale_transformation(OZEL_OKUL_ISIN, minimum = 3, maximum = 1)),
  od10 = rowmean2(scale_transformation(DEV_OKUL_GUVENLIK, minimum = 3, maximum = 1), scale_transformation(OZEL_OKUL_GUVENLIK, minimum = 3, maximum = 1)),
  hhg = scale_transformation(GELIR_GRUP, minimum = 1, maximum = 5),
  hhgy = scale_transformation(OLCEK_GELIR_KARSILAMA),
  ym = scale_transformation(MUTLULUK),
  sm = scale_transformation(OLCEK_MEMNUNIYET_SAGLIK),
  evs = scale_transformation(OLCEK_MEMNUNIYET_EVLILIK),
  egs = scale_transformation(OLCEK_MEMNUNIYET_EGITIM),
  okm = scale_transformation(OLCEK_MEMNUNIYET_KONUT),
  semt = scale_transformation(OLCEK_MEMNUNIYET_SEMT),
  ism = scale_transformation(OLCEK_MEMNUNIYET_IS),
  isgm = scale_transformation(OLCEK_MEMNUNIYET_KAZANC),
  hhg2 = scale_transformation(OLCEK_MEMNUNIYET_GELIR),
  tul = scale_transformation(OLCEK_MEMNUNIYET_KISISEL_BKM),
  tuc = scale_transformation(OLCEK_MEMNUNIYET_IS_TRFK_ZMN),
  ark = scale_transformation(OLCEK_MEMNUNIYET_ARKADAS),
  kom = scale_transformation(OLCEK_MEMNUNIYET_KOMSU),
  isam = scale_transformation(OLCEK_MEMNUNIYET_ISILISKI),
  issm = scale_transformation(ISTEKI_SORUN3, minimum = 1, maximum = 2),
  isck = scale_transformation(ISTEKI_SORUN4, minimum = 1, maximum = 2),
  sshm = scale_transformation(OLCEK_MEMNUNIYET_GN_SAG),
  sghm = scale_transformation(OLCEK_MEMNUNIYET_ASAYIS),
  sahm = scale_transformation(OLCEK_MEMNUNIYET_ADLI),
  sehm = scale_transformation(OLCEK_MEMNUNIYET_EGITI),
  sihm = scale_transformation(OLCEK_MEMNUNIYET_SGK),
  bel1 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_COP == 7, 5, MEMNUNIYET_COP)), scale_transformation(fifelse(MEMNUNIYET_IL_TEMIZLIK == 7, 5, MEMNUNIYET_IL_TEMIZLIK))),
  bel2 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_KAN == 7, 5, MEMNUNIYET_KAN)), scale_transformation(fifelse(MEMNUNIYET_IL_KANAL == 7, 5, MEMNUNIYET_IL_KANAL))),
  bel3 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_SU == 7, 5, MEMNUNIYET_SU)), scale_transformation(fifelse(MEMNUNIYET_IL_SU == 7, 5, MEMNUNIYET_IL_SU))),
  bel4 = scale_transformation(fifelse(MEMNUNIYET_TASIMA == 7, 5, MEMNUNIYET_TASIMA)),
  bel5 = scale_transformation(fifelse(MEMNUNIYET_ZABITA == 7, 5, MEMNUNIYET_ZABITA)),
  bel6 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_YOL == 7, 5, MEMNUNIYET_YOL)), scale_transformation(fifelse(MEMNUNIYET_IL_YOL == 7, 5, MEMNUNIYET_IL_YOL))),
  bel7 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_YESIL == 7, 5, MEMNUNIYET_YESIL)), scale_transformation(fifelse(MEMNUNIYET_IL_YESIL == 7, 5, MEMNUNIYET_IL_YESIL))),
  bel8 = scale_transformation(fifelse(MEMNUNIYET_KIRLILIK == 7, 5, MEMNUNIYET_KIRLILIK)),
  bel9 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_KURS == 7, 5, MEMNUNIYET_KURS)), scale_transformation(fifelse(MEMNUNIYET_IL_KURS == 7, 5, MEMNUNIYET_IL_KURS))),
  bel10 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_SERGI_FEST == 7, 5, MEMNUNIYET_SERGI_FEST)), scale_transformation(fifelse(MEMNUNIYET_IL_SERGI == 7, 5, MEMNUNIYET_IL_SERGI))),
  bel11 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_YARDIM == 7, 5, MEMNUNIYET_YARDIM)), scale_transformation(fifelse(MEMNUNIYET_IL_HASTA == 7, 5, MEMNUNIYET_IL_HASTA))),
  bel12 = scale_transformation(fifelse(MEMNUNIYET_SPOR_MRK == 7, 5, MEMNUNIYET_SPOR_MRK)),
  bel13 = rowmean2(scale_transformation(fifelse(MEMNUNIYET_IMAR == 7, 5, MEMNUNIYET_IMAR)), scale_transformation(fifelse(MEMNUNIYET_IL_ISKAN == 7, 5, MEMNUNIYET_IL_ISKAN))),
  shm1 = scale_transformation(SORUN_MUAYNE, minimum = 1, maximum = 2),
  shm2 = scale_transformation(SORUN_SGK_PERSONEL, minimum = 1, maximum = 2),
  shm3 = scale_transformation(SORUN_HIJYEN, minimum = 1, maximum = 2),
  shm4 = scale_transformation(MEMNUNIYET_MUAYENE, minimum = 2, maximum = 1),
  shm5 = scale_transformation(SORUN_DOKTOR, minimum = 1, maximum = 2),
  shm6 = scale_transformation(SORUN_HEMSIRE, minimum = 1, maximum = 2),
  shm7 = scale_transformation(SORUN_ILAC_FIYAT, minimum = 1, maximum = 2),
  shm8 = scale_transformation(SORUN_YETERLI_SGLKPERSONEL, minimum = 2, maximum = 1),
  shm9 = scale_transformation(SORUN_UCRET_MUAYENE, minimum = 1, maximum = 2),
  ah = scale_transformation(SORUN_YASA, minimum = 3, maximum = 1),
  guv1 = scale_transformation(GUVEN_EV),
  guv2 = scale_transformation(GUVEN_CEVRE),
  ud = scale_transformation(UMUT, minimum = 4, maximum = 1),
  sw = scale_transformation(UMUT_BASAMAK, minimum = 1, maximum = 11),
  ssb1 = scale_transformation(GELECEK_HAYAT, minimum = 3, maximum = 1),
  ssb2 = scale_transformation(GELECEK_MALI, minimum = 3, maximum = 1),
  ssb3 = scale_transformation(GELECEK_KISISEL, minimum = 3, maximum = 1),
  ssb4 = scale_transformation(GELECEK_EKONOMI, minimum = 3, maximum = 1),
  ssb5 = scale_transformation(GELECEK_IS, minimum = 3, maximum = 1),
  spi = {
    t <- list(
      scale_transformation(CINSIYET_BASKI, minimum = 4, maximum = 1),
      scale_transformation(MEDENI_DURUM_BASKI, minimum = 4, maximum = 1),
      scale_transformation(YAS_BASKI, minimum = 4, maximum = 1),
      scale_transformation(GELENEK_GORENEK, minimum = 4, maximum = 1),
      scale_transformation(DINI_INANCDAN, minimum = 4, maximum = 1),
      scale_transformation(SIYASI_GORUSTEN, minimum = 4, maximum = 1),
      scale_transformation(MEMLEKETINDEN, minimum = 4, maximum = 1),
      scale_transformation(ISINDEN_DOLAYI, minimum = 4, maximum = 1),
      scale_transformation(KILIK_KIYAFET, minimum = 4, maximum = 1),
      scale_transformation(ISSIZ_OLMA, minimum = 4, maximum = 1),
      scale_transformation(GELIR_DUZEY_DURUM, minimum = 4, maximum = 1)
    )
    spi1 <- rowMeans(do.call(cbind, t), na.rm = TRUE)
    fifelse(!is.na(spi1) & spi1 > 10 / 11, 1, 0)
  },
  nei1 = scale_transformation(EKONOMIK_ACIDAN, minimum = 3, maximum = 1),
  nei2 = scale_transformation(SOSYAL_HAKLAR, minimum = 3, maximum = 1),
  nei3 = scale_transformation(KAMU_HIZMET_SUNUM, minimum = 3, maximum = 1),
  nei4 = scale_transformation(DEVLET_SEFFAF, minimum = 3, maximum = 1)
)]


## Step 2: append years, compute composite well-being scores ----
# Ported from Stata `SWBI2/Do Files/{std2.do, std2 - auwbi.do, std2 - gnh.do,
# std2 - faktör.do}`. append.do there only appends 2004-2013 (2014 is not part of this
# stage -- extending the pipeline to newer years is a separate later step).
#
# rowmeann() replicates `egen ... = rowmean(a b c ...)` for any number of columns
# (rowmean2() above stays untouched since step-1 code already depends on it).
#
# wmean_row() replicates Stata's `cond(x,x,x,0)` weighted-average idiom used throughout
# these do-files: cond(x,a,b,c) returns a (here, x itself) whenever x is non-missing
# (whether zero or not) and c (here, 0) when x is missing, so
#   gen y = (w1*cond(x1,x1,x1,0) + w2*cond(x2,x2,x2,0) + ...) /
#           (1 - (cond(x1,0,0,w1) + cond(x2,0,0,w2) + ...))
# is a weighted mean of the non-missing x_i with weights renormalised to sum to 1 among
# only the non-missing items -- exactly what wmean_row() computes below. Verified
# against the Stata formula by hand for oda (see session notes).
rowmeann <- function(...) {
  m <- rowMeans(cbind(...), na.rm = TRUE)
  m[is.nan(m)] <- NA_real_
  m
}

wmean_row <- function(..., w) {
  X <- cbind(...)
  W <- matrix(w, nrow = nrow(X), ncol = length(w), byrow = TRUE)
  W[is.na(X)] <- 0
  X[is.na(X)] <- 0
  num <- rowSums(X * W)
  den <- rowSums(W)
  fifelse(den == 0, NA_real_, num / den)
}

swbi_panel <- rbindlist(list(
  swbi_2004, swbi_2005, swbi_2006, swbi_2007, swbi_2008,
  swbi_2009, swbi_2010, swbi_2011, swbi_2012, swbi_2013,
  swbi_2015, swbi_2016, swbi_2017
), idcol = "year", fill = TRUE)
swbi_panel[, year := c(2004:2013, 2015:2017)[year]]
# -- std2.do: domain-level composites --
swbi_panel[, oda := wmean_row(od2, od4, od5, w = c(0.2893, 0.3554, 0.3553))]
swbi_panel[, odb := wmean_row(od3, od7, od8, od9, w = c(0.2090, 0.2770, 0.2654, 0.2486))]
swbi_panel[, od := wmean_row(oda, odb, w = c(0.5572, 0.4428))]
# NOTE: od1 and od6 are deliberately excluded from this composite (not in either weight
# set) -- presumably dropped for low loadings in whatever factor analysis produced
# these weights, not an omission on my part.

swbi_panel[, comm1 := rowmeann(okm, semt)]
swbi_panel[, comm2 := rowmeann(ark, kom)]
swbi_panel[, comm := rowmeann(comm1, comm2)]

swbi_panel[, is := rowmeann(ism, isgm, issm, isck)] # isam (colleague satisfaction) is deliberately excluded here
swbi_panel[, state := rowmeann(sshm, sghm, sahm, sehm, sihm)]
swbi_panel[, bel := rowmeann(bel1, bel2, bel3, bel4, bel5, bel6, bel7, bel8, bel9, bel10, bel11, bel12, bel13)]

swbi_panel[, shma := wmean_row(shm3, shm5, shm6, w = c(0.2298, 0.3847, 0.3855))]
swbi_panel[, shmb := wmean_row(shm2, shm7, shm9, w = c(0.2078, 0.4006, 0.3916))]
swbi_panel[, shmc := wmean_row(shm4, shm8, w = c(0.4657, 0.5343))]
swbi_panel[, shm := wmean_row(shma, shmb, shmc, w = c(0.39446, 0.34918, 0.25636))]
# NOTE: shm1 is deliberately excluded from this composite (not in any of the three
# weight sets above).

swbi_panel[, guv := rowmeann(guv1, guv2)]
swbi_panel[, ssbi := rowmeann(ssb1, ssb2, ssb3)]
swbi_panel[, ssbn := rowmeann(ssb4, ssb5)]
swbi_panel[, ssb := rowmeann(ssb1, ssb2, ssb3, ssb4, ssb5)]
swbi_panel[, nei := rowmeann(nei1, nei2, nei3, nei4)]
swbi_panel[, income := rowmeann(hhg, hhg2, hhgy, sw)]

# -- std2 - auwbi.do: individual/national well-being aggregates --
# NOTE -- likely typo in the source do-file: its `iwb` line reads
# `egen iwb = rowmean ( comm is inc ym ud ssbi sm )`, referencing "inc", which is never
# defined anywhere in any of these do-files -- only "income" (created just above)
# exists. Treated as a typo for "income" below; flag to Kâzım before trusting iwb.
swbi_panel[, nwb := rowmeann(ssbn, od, state, bel, shm, guv)]
swbi_panel[, iwb := rowmeann(comm, is, income, ym, ud, ssbi, sm)]

# -- std2 - gnh.do: simple-average Bhutan-style GNH index (Alkire-Foster, 2/3 domain
# sufficiency threshold, adjusted headcount formula) --
swbi_panel[, gnh1 := rowmeann(is, income)]
swbi_panel[, gnh2 := rowmeann(ym, ud, ssb)]
swbi_panel[, gnh3 := sm]
swbi_panel[, gnh4 := rowmeann(od, state, bel, shm)]
swbi_panel[, gnh5 := guv]
swbi_panel[, gnh6 := comm]

# NOTE: ported as written -- Stata's `gen X1=1 if gnhX>=2/3` + `replace X1=0 if X1==.`
# means a domain with no usable inputs (gnhX missing) is scored "insufficient" (0)
# rather than left NA, so gnh11..gnh61 (and gnh9/happy/gnh below) are never NA.
swbi_panel[, gnh11 := fifelse(gnh1 >= 2 / 3, 1, 0)]
swbi_panel[, gnh21 := fifelse(gnh2 >= 2 / 3, 1, 0)]
swbi_panel[, gnh31 := fifelse(gnh3 >= 2 / 3, 1, 0)]
swbi_panel[, gnh41 := fifelse(gnh4 >= 2 / 3, 1, 0)]
swbi_panel[, gnh51 := fifelse(gnh5 >= 2 / 3, 1, 0)]
swbi_panel[, gnh61 := fifelse(gnh6 >= 2 / 3, 1, 0)]

swbi_panel[, gnh9 := (gnh11 + gnh21 + gnh31 + gnh41 + gnh51 + gnh61) / 6]
swbi_panel[, happy := fifelse(gnh9 >= 2 / 3, 1, 0)]
swbi_panel[, gnh := 1 + ((happy - 1) * (1 - gnh9))]

# -- std2 - faktör.do: factor(PCA)-weighted alternative scoring for the same GNH,
# national- and individual-wellbeing indices --
swbi_panel[, gnh8 := wmean_row(
  gnh11, gnh21, gnh31, gnh41, gnh51, gnh61,
  w = c(0.1765, 0.1628, 0.1760, 0.1476, 0.1749, 0.1622)
)]
swbi_panel[, happy3 := fifelse(gnh8 >= 2 / 3, 1, 0)]
swbi_panel[, unhappy3 := fifelse(happy3 == 0, 1, 0)]
swbi_panel[, gnhf := 1 - (unhappy3 * (1 - gnh8))]
# algebraically identical construction to `gnh` above, just built from gnh8 (weighted)
# instead of gnh9 (simple average): happy3==1 -> gnhf=1; happy3==0 -> gnhf=gnh8.

swbi_panel[, nwbf := wmean_row(
  od, state, shm, guv, bel, ssbn,
  w = c(0.1111, 0.0962, 0.0992, 0.1131, 0.2950, 0.2854)
)]
swbi_panel[, iwbf := wmean_row(
  income, ssbi, is, ud, ym, comm, sm,
  w = c(0.1093, 0.1059, 0.0942, 0.1037, 0.0841, 0.2604, 0.2424)
)]

# -- weighted summary statistics, mirroring every `sum X [w=ff]` / `tab X [w=ff]` line
# across all four do-files, by year (matching how std2.do runs once per year, before
# append.do stacks the years) so each row is directly comparable against the original
# per-year Stata output --
swbi_summary_by_year <- swbi_panel[, .(
  ym           = weighted.mean(ym, weight, na.rm = TRUE),
  ud           = weighted.mean(ud, weight, na.rm = TRUE),
  ssb          = weighted.mean(ssb, weight, na.rm = TRUE),
  ssbi         = weighted.mean(ssbi, weight, na.rm = TRUE),
  ssbn         = weighted.mean(ssbn, weight, na.rm = TRUE),
  sm           = weighted.mean(sm, weight, na.rm = TRUE),
  is           = weighted.mean(is, weight, na.rm = TRUE),
  income       = weighted.mean(income, weight, na.rm = TRUE),
  od           = weighted.mean(od, weight, na.rm = TRUE),
  state        = weighted.mean(state, weight, na.rm = TRUE),
  bel          = weighted.mean(bel, weight, na.rm = TRUE),
  shm          = weighted.mean(shm, weight, na.rm = TRUE),
  guv          = weighted.mean(guv, weight, na.rm = TRUE),
  comm         = weighted.mean(comm, weight, na.rm = TRUE),
  nwb          = weighted.mean(nwb, weight, na.rm = TRUE),
  iwb          = weighted.mean(iwb, weight, na.rm = TRUE),
  happy_share  = weighted.mean(happy, weight, na.rm = TRUE),
  gnh          = weighted.mean(gnh, weight, na.rm = TRUE),
  happy3_share = weighted.mean(happy3, weight, na.rm = TRUE),
  gnhf         = weighted.mean(gnhf, weight, na.rm = TRUE),
  nwbf         = weighted.mean(nwbf, weight, na.rm = TRUE),
  iwbf         = weighted.mean(iwbf, weight, na.rm = TRUE)
), by = year][order(year)]
fwrite(swbi_summary_by_year, "agg_data/swbi_turkey_2003_2017.csv", sep = "|", dec = ".")

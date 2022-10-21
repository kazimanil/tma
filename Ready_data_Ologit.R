# Source Files Import ----
rm(list = ls()); gc();
source("https://raw.githubusercontent.com/kazimanil/tma/master/functions.R") # Base functions for data manipulation
source("https://raw.githubusercontent.com/kazimanil/project_kaf/master/R_functions/gg_theme.R") # My Theme for GGPlot2

# Libraries ----
library('data.table');
library('ggplot2');
library('plotly');
library('readxl');

# Raw Data Import ----
# The Data is shared by TURKSTAT on the promise that it will not be shared publicly.
# Thus, I will only be able share aggregated data after the manipulation & aggregation steps.

## 2016
fert_2016 = fread("data/2016/yma2016_fert_mikroveri.csv");
hane_2016 = fread("data/2016/yma2016_hane_mikroveri.csv");
data_2016 = merge(
  fert_2016,
  hane_2016,
  by = 'BIRIMNO'
);
rm(fert_2016, hane_2016);

ologit_2016 = data_2016[, .(
  weight = FAKTOR_FERT,
  hap = scale_transformation(MUTLULUK),
  hap_binary = happiness_transformation(MUTLULUK),
  sex = relevel(
    as.factor(gender(CINSIYET)),
    'Female'
  ),
  age = as.numeric(BITIRILEN_YAS),
  age2 = as.numeric(BITIRILEN_YAS)^2,
  mars = relevel(
    as.factor(
      marriage_satisfaction(
        marital_status(MEDENI_DURUM), 
        scale_transformation(OLCEK_MEMNUNIYET_EVLILIK)
      )),
    "Single"
  ),
  edl = relevel(
    as.factor(education_level(OKUL_BITEN)),
    "No Schooling"),
  jobs = relevel(
    as.factor(
      job_satisfaction(
        employment(CALISMA_DURUM, CALISMAMA_NEDEN, TRUE),
        scale_transformation(OLCEK_MEMNUNIYET_IS)
    )),
    "Out of Labour Force"),
  mat = relevel(
    as.factor(materialism(NE_MUTLU)),
    "Not Materialistic"
  ),
  sw = relevel(
    as.factor(paste0("Subjective Welfare - Cantril Ladder: ", as.character(as.numeric(UMUT_BASAMAK) - 1))),
    "Subjective Welfare - Cantril Ladder: 0"
  ),
  hhi_lvl = relevel(
    as.factor(household_income_transformation(GELIR_GRUP)),
    "Household Income Tier 1"
  ),
  hhi_suf = relevel(
    as.factor(likert_categoric(OLCEK_GELIR_KARSILAMA, "Income Sufficiency")),
    "Neutral"
  ),
  sh = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_SAGLIK, "Subjective Health")),
    "Neutral"
  ),
  eds = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_EGITIM, "Education")),
    "Neutral"
  ),
  hos = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_KONUT, "Housing")),
    "Neutral"
  ),
  dis = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_SEMT, "Neighbourhood")),
    "Neutral"
  ),
  hhi_sat = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_GELIR, "Income")),
    "Neutral"
  ),
  frs = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_ARKADAS, "Friends")),
    "Neutral"
  ),
  nes = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_KOMSU, "Neighbours")),
    "Neutral"
  ),
  sls = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_SOS_HAYAT, "Social Life")),
    "Neutral"
  ),
  tul = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_KISISEL_BKM, "Leisure")),
    "Neutral"
  ),
  psh = relevel(
    as.factor(likert_categoric(GUVEN_EV, "Safety at Home")),
    "Neutral"
  ),
  pse = relevel(
    as.factor(likert_categoric(GUVEN_CEVRE, "Safety Around Home")),
    "Neutral"
  ),
  doh = relevel(
    as.factor(hope_transformation(UMUT)),
    "Not Hopeful At All"
  ),
  fya = relevel(
    as.factor(comparison_5y_ago(GECMIS_KARSILASTIRMA)),
    "No Idea"
  ),
  fyl = relevel(
    as.factor(expectations_5y_later(GELECEK_KARSILASTIRMA)),
    "No Idea"
  ),
  lyp = relevel(
    as.factor(
      ifelse(pmin(ISE_GIRDI, ISYERI_ACTI, GELIRI_ARTTI, TASARRUF_YAPTI, ARABA_ALDI, EV_ALDI, BORC_ODEDI, na.rm = TRUE) == 1,
             'Positive Memory',
             'No Positive Memory')
    ),
    'No Positive Memory'
  ),
  lyn = relevel(
    as.factor(
      ifelse(pmin(ISINI_KAYBETTI, IFLAS_ETTI, GELIRI_AZALDI, TASARRUFLARI_AZALDI, UCUZ_URUN, TATIL_KISTI, ARABA_SATTI, EV_SATTI, BORCLANDI, na.rm = TRUE) == 1, 
             'Negative Memory', 
             'No Negative Memory')
    ),
    'No Negative Memory'
  ),
  lym = relevel(
    as.factor(
      ifelse(GOC_ETTI == 1, 
             'Migrated', 
             'Not Migrated')
    ),
    'Not Migrated'
  ),
  fsp = relevel(
    as.factor(
      ifelse(pmax(CINSIYET_BASKI, MEDENI_DURUM_BASKI, YAS_BASKI, GELENEK_GORENEK, DINI_INANCDAN, SIYASI_GORUSTEN, MEMLEKETINDEN, ISINDEN_DOLAYI, KILIK_KIYAFET, ISSIZ_OLMA, GELIR_DUZEY_DURUM, na.rm = TRUE) > 3,
             "Had Social Pressure",
             'Base'
      )),
    "Had Social Pressure"
  )
)]

## 2015
fert_2015 = fread("data/2015/yma2015_fert_mikroveri.csv");
hane_2015 = fread("data/2015/yma2015_hane_mikroveri.csv");
data_2015 = merge(
  fert_2015,
  hane_2015,
  by = 'BIRIMNO'
);
rm(fert_2015, hane_2015);

ologit_2015 = data_2015[, .(
  weight = FAKTOR_FERT,
  hap = scale_transformation(MUTLULUK),
  hap_binary = happiness_transformation(MUTLULUK),
  sex = relevel(
    as.factor(gender(CINSIYET)),
    'Female'
  ),
  age = as.numeric(BITIRILEN_YAS),
  age2 = as.numeric(BITIRILEN_YAS)^2,
  mars = relevel(
    as.factor(
      marriage_satisfaction(
        marital_status(MEDENI_DURUM), 
        scale_transformation(OLCEK_MEMNUNIYET_EVLILIK)
      )),
    "Single"
  ),
  edl = relevel(
    as.factor(education_level(OKUL_BITEN)),
    "No Schooling"),
  jobs = relevel(
    as.factor(
      job_satisfaction(
        employment(CALISMA_DURUM, CALISMAMA_NEDEN, TRUE),
        scale_transformation(OLCEK_MEMNUNIYET_IS)
      )),
    "Out of Labour Force"),
  mat = relevel(
    as.factor(materialism(NE_MUTLU)),
    "Not Materialistic"
  ),
  sw = relevel(
    as.factor(paste0("Subjective Welfare - Cantril Ladder: ", as.character(as.numeric(UMUT_BASAMAK) - 1))),
    "Subjective Welfare - Cantril Ladder: 0"
  ),
  hhi_lvl = relevel(
    as.factor(household_income_transformation(GELIR_GRUP)),
    "Household Income Tier 1"
  ),
  hhi_suf = relevel(
    as.factor(likert_categoric(OLCEK_GELIR_KARSILAMA, "Income Sufficiency")),
    "Neutral"
  ),
  sh = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_SAGLIK, "Subjective Health")),
    "Neutral"
  ),
  eds = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_EGITIM, "Education")),
    "Neutral"
  ),
  hos = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_KONUT, "Housing")),
    "Neutral"
  ),
  dis = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_SEMT, "Neighbourhood")),
    "Neutral"
  ),
  hhi_sat = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_GELIR, "Income")),
    "Neutral"
  ),
  frs = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_ARKADAS, "Friends")),
    "Neutral"
  ),
  nes = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_KOMSU, "Neighbours")),
    "Neutral"
  ),
  sls = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_SOS_HAYAT, "Social Life")),
    "Neutral"
  ),
  tul = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_KISISEL_BKM, "Leisure")),
    "Neutral"
  ),
  psh = relevel(
    as.factor(likert_categoric(GUVEN_EV, "Safety at Home")),
    "Neutral"
  ),
  pse = relevel(
    as.factor(likert_categoric(GUVEN_CEVRE, "Safety Around Home")),
    "Neutral"
  ),
  doh = relevel(
    as.factor(hope_transformation(UMUT)),
    "Not Hopeful At All"
  ),
  fya = relevel(
    as.factor(comparison_5y_ago(GECMIS_KARSILASTIRMA)),
    "No Idea"
  ),
  fyl = relevel(
    as.factor(expectations_5y_later(GELECEK_KARSILASTIRMA)),
    "No Idea"
  ),
  lyp = relevel(
    as.factor(
      ifelse(pmin(ISE_GIRDI, ISYERI_ACTI, GELIRI_ARTTI, TASARRUF_YAPTI, ARABA_ALDI, EV_ALDI, BORC_ODEDI, na.rm = TRUE) == 1,
             'Positive Memory',
             'No Positive Memory')
    ),
    'No Positive Memory'
  ),
  lyn = relevel(
    as.factor(
      ifelse(pmin(ISINI_KAYBETTI, IFLAS_ETTI, GELIRI_AZALDI, TASARRUFLARI_AZALDI, UCUZ_URUN, TATIL_KISTI, ARABA_SATTI, EV_SATTI, BORCLANDI, na.rm = TRUE) == 1, 
             'Negative Memory', 
             'No Negative Memory')
    ),
    'No Negative Memory'
  ),
  lym = relevel(
    as.factor(
      ifelse(GOC_ETTI == 1, 
             'Migrated', 
             'Not Migrated')
    ),
    'Not Migrated'
  ),
  fsp = relevel(
    as.factor(
      ifelse(pmax(CINSIYET_BASKI, MEDENI_DURUM_BASKI, YAS_BASKI, GELENEK_GORENEK, DINI_INANCDAN, SIYASI_GORUSTEN, MEMLEKETINDEN, ISINDEN_DOLAYI, KILIK_KIYAFET, ISSIZ_OLMA, GELIR_DUZEY_DURUM, na.rm = TRUE) > 3,
             "Had Social Pressure",
             'Base'
      )),
    "Had Social Pressure"
  )
)]

## 2014
fert_2014 = fread("data/2014/yma2014_fert_mikroveri.csv");
hane_2014 = fread("data/2014/yma2014_hane_mikroveri.csv");
data_2014 = merge(
  fert_2014,
  hane_2014,
  by = 'BIRIMNO'
);
rm(fert_2014, hane_2014);

ologit_2014 = data_2014[, .(
  weight = FAKTOR_FERT,
  hap = scale_transformation(MUTLULUK),
  hap_binary = happiness_transformation(MUTLULUK),
  sex = relevel(
    as.factor(gender(CINSIYET)),
    'Female'
  ),
  age = as.numeric(BITIRILEN_YAS),
  age2 = as.numeric(BITIRILEN_YAS)^2,
  mars = relevel(
    as.factor(
      marriage_satisfaction(
        marital_status(MEDENI_DURUM), 
        scale_transformation(OLCEK_MEMNUNIYET_EVLILIK)
      )),
    "Single"
  ),
  edl = relevel(
    as.factor(education_level(OKUL_BITEN)),
    "No Schooling"),
  jobs = relevel(
    as.factor(
      job_satisfaction(
        employment(CALISMA_DURUM, CALISMAMA_NEDEN, TRUE),
        scale_transformation(OLCEK_MEMNUNIYET_IS)
      )),
    "Out of Labour Force"),
  mat = relevel(
    as.factor(materialism(NE_MUTLU)),
    "Not Materialistic"
  ),
  sw = relevel(
    as.factor(paste0("Subjective Welfare - Cantril Ladder: ", as.character(as.numeric(UMUT_BASAMAK) - 1))),
    "Subjective Welfare - Cantril Ladder: 0"
  ),
  hhi_lvl = relevel(
    as.factor(household_income_transformation(GELIR_GRUP)),
    "Household Income Tier 1"
  ),
  hhi_suf = relevel(
    as.factor(likert_categoric(OLCEK_GELIR_KARSILAMA, "Income Sufficiency")),
    "Neutral"
  ),
  sh = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_SAGLIK, "Subjective Health")),
    "Neutral"
  ),
  eds = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_EGITIM, "Education")),
    "Neutral"
  ),
  hos = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_KONUT, "Housing")),
    "Neutral"
  ),
  dis = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_SEMT, "Neighbourhood")),
    "Neutral"
  ),
  hhi_sat = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_GELIR, "Income")),
    "Neutral"
  ),
  frs = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_ARKADAS, "Friends")),
    "Neutral"
  ),
  nes = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_KOMSU, "Neighbours")),
    "Neutral"
  ),
  sls = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_SOS_HAYAT, "Social Life")),
    "Neutral"
  ),
  tul = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_KISISEL_BKM, "Leisure")),
    "Neutral"
  ),
  psh = relevel(
    as.factor(likert_categoric(GUVEN_EV, "Safety at Home")),
    "Neutral"
  ),
  pse = relevel(
    as.factor(likert_categoric(GUVEN_CEVRE, "Safety Around Home")),
    "Neutral"
  ),
  doh = relevel(
    as.factor(hope_transformation(UMUT)),
    "Not Hopeful At All"
  ),
  fya = relevel(
    as.factor(comparison_5y_ago(GECMIS_KARSILASTIRMA)),
    "No Idea"
  ),
  fyl = relevel(
    as.factor(expectations_5y_later(GELECEK_KARSILASTIRMA)),
    "No Idea"
  ),
  lyp = relevel(
    as.factor(
      ifelse(pmin(ISE_GIRDI, ISYERI_ACTI, GELIRI_ARTTI, TASARRUF_YAPTI, ARABA_ALDI, EV_ALDI, BORC_ODEDI, na.rm = TRUE) == 1,
             'Positive Memory',
             'No Positive Memory')
    ),
    'No Positive Memory'
  ),
  lyn = relevel(
    as.factor(
      ifelse(pmin(ISINI_KAYBETTI, IFLAS_ETTI, GELIRI_AZALDI, TASARRUFLARI_AZALDI, UCUZ_URUN, TATIL_KISTI, ARABA_SATTI, EV_SATTI, BORCLANDI, na.rm = TRUE) == 1, 
             'Negative Memory', 
             'No Negative Memory')
    ),
    'No Negative Memory'
  ),
  lym = relevel(
    as.factor(
      ifelse(GOC_ETTI == 1, 
             'Migrated', 
             'Not Migrated')
    ),
    'Not Migrated'
  ),
  fsp = relevel(
    as.factor(
      ifelse(pmax(CINSIYET_BASKI, MEDENI_DURUM_BASKI, YAS_BASKI, GELENEK_GORENEK, DINI_INANCDAN, SIYASI_GORUSTEN, MEMLEKETINDEN, ISINDEN_DOLAYI, KILIK_KIYAFET, ISSIZ_OLMA, GELIR_DUZEY_DURUM, na.rm = TRUE) > 3,
             "Had Social Pressure",
             'Base'
      )),
    "Had Social Pressure"
  )
)]


## 2013
fert_2013 = fread("data/2013/yma2013_fert_mikroveri.csv");
hane_2013 = fread("data/2013/yma2013_hane_mikroveri.csv");
data_2013 = merge(
  fert_2013,
  hane_2013,
  by = 'BIRIMNO'
);
rm(fert_2013, hane_2013);

ologit_2013 = data_2013[, .(
  weight = FAKTOR_FERT,
  hap = scale_transformation(MUTLULUK),
  hap_binary = happiness_transformation(MUTLULUK),
  sex = relevel(
    as.factor(gender(CINSIYET)),
    'Female'
  ),
  age = as.numeric(BITIRILEN_YAS),
  age2 = as.numeric(BITIRILEN_YAS)^2,
  mars = relevel(
    as.factor(
      marriage_satisfaction(
        marital_status(MEDENI_DURUM), 
        scale_transformation(OLCEK_MEMNUNIYET_EVLILIK)
      )),
    "Single"
  ),
  edl = relevel(
    as.factor(education_level(OKUL_BITEN)),
    "No Schooling"),
  jobs = relevel(
    as.factor(
      job_satisfaction(
        employment(CALISMA_DURUM, CALISMAMA_NEDEN, TRUE),
        scale_transformation(OLCEK_MEMNUNIYET_IS)
      )),
    "Out of Labour Force"),
  mat = relevel(
    as.factor(materialism(NE_MUTLU)),
    "Not Materialistic"
  ),
  sw = relevel(
    as.factor(paste0("Subjective Welfare - Cantril Ladder: ", as.character(as.numeric(UMUT_BASAMAK) - 1))),
    "Subjective Welfare - Cantril Ladder: 0"
  ),
  hhi_lvl = relevel(
    as.factor(household_income_transformation(GELIR_GRUP)),
    "Household Income Tier 1"
  ),
  hhi_suf = relevel(
    as.factor(likert_categoric(OLCEK_GELIR_KARSILAMA, "Income Sufficiency")),
    "Neutral"
  ),
  sh = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_SAGLIK, "Subjective Health")),
    "Neutral"
  ),
  eds = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_EGITIM, "Education")),
    "Neutral"
  ),
  hos = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_KONUT, "Housing")),
    "Neutral"
  ),
  dis = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_SEMT, "Neighbourhood")),
    "Neutral"
  ),
  hhi_sat = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_GELIR, "Income")),
    "Neutral"
  ),
  frs = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_ARKADAS, "Friends")),
    "Neutral"
  ),
  nes = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_KOMSU, "Neighbours")),
    "Neutral"
  ),
  sls = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_SOS_HAYAT, "Social Life")),
    "Neutral"
  ),
  tul = relevel(
    as.factor(likert_categoric(OLCEK_MEMNUNIYET_KISISEL_BKM, "Leisure")),
    "Neutral"
  ),
  psh = relevel(
    as.factor(likert_categoric(GUVEN_EV, "Safety at Home")),
    "Neutral"
  ),
  pse = relevel(
    as.factor(likert_categoric(GUVEN_CEVRE, "Safety Around Home")),
    "Neutral"
  ),
  doh = relevel(
    as.factor(hope_transformation(UMUT)),
    "Not Hopeful At All"
  ),
  fya = relevel(
    as.factor(comparison_5y_ago(GECMIS_KARSILASTIRMA)),
    "No Idea"
  ),
  fyl = relevel(
    as.factor(expectations_5y_later(GELECEK_KARSILASTIRMA)),
    "No Idea"
  ),
  lyp = relevel(
    as.factor(
      ifelse(pmin(ISE_GIRDI, ISYERI_ACTI, GELIRI_ARTTI, TASARRUF_YAPTI, ARABA_ALDI, EV_ALDI, BORC_ODEDI, na.rm = TRUE) == 1,
             'Positive Memory',
             'No Positive Memory')
    ),
    'No Positive Memory'
  ),
  lyn = relevel(
    as.factor(
      ifelse(pmin(ISINI_KAYBETTI, IFLAS_ETTI, GELIRI_AZALDI, TASARRUFLARI_AZALDI, UCUZ_URUN, TATIL_KISTI, ARABA_SATTI, EV_SATTI, BORCLANDI, na.rm = TRUE) == 1, 
             'Negative Memory', 
             'No Negative Memory')
    ),
    'No Negative Memory'
  ),
  lym = relevel(
    as.factor(
      ifelse(GOC_ETTI == 1, 
             'Migrated', 
             'Not Migrated')
    ),
    'Not Migrated'
  ),
  fsp = relevel(
    as.factor(
      ifelse(pmax(CINSIYET_BASKI, MEDENI_DURUM_BASKI, YAS_BASKI, GELENEK_GORENEK, DINI_INANCDAN, SIYASI_GORUSTEN, MEMLEKETINDEN, ISINDEN_DOLAYI, KILIK_KIYAFET, ISSIZ_OLMA, GELIR_DUZEY_DURUM, na.rm = TRUE) > 3,
             "Had Social Pressure",
             'Base'
      )),
    "Had Social Pressure"
  )
)]

# Only works for 0/1 y variables. Not Ologit.
summary(glm(hap_binary ~ sex + age + age2 + mars  + jobs + mat + lyp + lyn + lym + fsp, data = ologit_2016, family = "binomial"))

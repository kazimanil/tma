# Source Files Import ----
rm(list = ls())
gc()
setwd("C:/Users/kazim/Documents/tma/")
library("data.table")
source("https://raw.githubusercontent.com/kazimanil/tma/master/functions.R") # Base functions for data manipulation

# GNH mapping ----
# This script turns questionnaire_mapping/swbi_gnh_domain_mapping.csv (129 items,
# 9 domains, 2013-2017 only) into an executable scoring pipeline. See
# "GNH Terminology.md" for what each domain/sub-domain means and how the scoring
# choices below (Alkire-Foster-style "any issue" sub-domains, PCA-weighted
# averages, the education/asset special cases) were arrived at. This file only
# covers 2013-2017 -- the mapping CSV was built for that range on purpose, since
# the GNH item set (crime, pressure, housing-issue batteries etc.) only exists
# in the questionnaire from 2013 onward.

# Raw Data Import ----
# The Data is shared by TURKSTAT on the promise that it will not be shared publicly.
# Thus, I will only be able share aggregated data after the manipulation & aggregation steps.
data_2013 <- fread("data/data_2013.csv")
data_2014 <- fread("data/data_2014.csv")
data_2015 <- fread("data/data_2015.csv")
data_2016 <- fread("data/data_2016.csv")
data_2017 <- fread("data/data_2017.csv")

## Helper functions ----
# Every harmonised item column below is 0-1, oriented so 1 = best/no issue and
# 0 = worst/issue present (scale_transformation()'s own convention).

# MEMNUNIYET_* municipal/national satisfaction items share one quirk: code 7
# means "no such service here" and is recoded to 5 (the worst substantive code)
# before the usual 1-5 scale_transformation() -- same rule Data Preparation_SWBI.R
# already applies to the same variables.
memnuniyet_scale <- function(x) scale_transformation(fifelse(x == 7, 5, x))

# "Sorun var mi?/yasadiniz mi?" (problem/event yes-no) items: 1 = Evet (bad), 2 = Hayir (good).
sorun_scale <- function(x) scale_transformation(x, minimum = 1, maximum = 2)

# Yes/no items phrased the other way round ("memnun musunuz?", "yeterli buluyor
# musunuz?", "begeniyor musunuz?"): 1 = Evet (good), 2 = Hayir (bad).
evet_good_scale <- function(x) scale_transformation(x, minimum = 2, maximum = 1)

# 3-point "Sorun yok / Az sorun var / Cok sorun var" (+ 4 = Fikri yok, excluded)
# scale used by the judicial-system battery and by 3 of the education items.
sorun3_scale <- function(x) scale_transformation(x, minimum = 3, maximum = 1)

# education_tier(): educational_attainment recoded onto a fixed 0/0.33/0.66/1
# scale (no schooling / primary / secondary / tertiary), reusing the existing
# education_level() categoriser with the coding scheme each year actually used.
education_tier <- function(value, scheme) {
  cat <- education_level(value, scheme = scheme)
  fcase(
    cat == "No Schooling", 0,
    cat == "Primary Education", 0.33,
    cat == "Secondary Education", 0.66,
    cat == "Tertiary Education", 1,
    default = NA_real_
  )
}

# any_issue_score(): Alkire-Foster-style sub-domain score for the *_issues /
# *_problems / crime_victimization / social_pressure batteries. Each item is
# first binarised at its own perfect score (only a harmonised value of exactly
# 1 -- no issue at all -- counts as issue-free; any lesser value, including a
# neutral/partial response, counts as an issue). The sub-domain score is 1 only
# if every item available for that respondent is issue-free, 0 if at least one
# indicates an issue, NA if every item is NA for that respondent.
any_issue_score <- function(...) {
  X <- cbind(...)
  flags <- (X == 1) * 1
  apply(flags, 1, function(r) if (all(is.na(r))) NA_real_ else min(r, na.rm = TRUE))
}

# owns_any_score(): OR across binary 0/1 ownership flags (Living Standards/asset_ownership).
owns_any_score <- function(...) {
  X <- cbind(...)
  apply(X, 1, function(r) if (all(is.na(r))) NA_real_ else max(r, na.rm = TRUE))
}

# pca_weights(): first-principal-component loadings for a set of harmonised 0-1
# columns, sign-corrected so a higher score always means "better" and rescaled
# to sum to 1 -- the same weight-vector shape as the wmean_row() calls in
# Data Preparation_SWBI.R, just estimated here at runtime (on the pooled
# 2013-2017 panel) instead of ported from a prior Stata factor analysis.
pca_weights <- function(X) {
  cc <- X[stats::complete.cases(X), , drop = FALSE]
  sds <- apply(cc, 2, stats::sd)
  if (nrow(cc) < 10 || ncol(cc) < 2 || any(sds == 0)) {
    return(rep(1 / ncol(X), ncol(X))) # fallback: equal weights
  }
  pr <- stats::prcomp(cc, center = TRUE, scale. = TRUE)
  w <- pr$rotation[, 1]
  if (sum(w) < 0) w <- -w # orient so a higher score means "better"
  w <- pmax(w, 0) # a negative loading would fight that convention -- floor at 0
  if (sum(w) == 0) return(rep(1 / ncol(X), ncol(X)))
  w / sum(w)
}

# pca_row_score(): weighted mean of the supplied harmonised columns. A single
# column is returned as-is (nothing to combine). With 2+ columns, weights come
# from pca_weights() and are renormalised per row across whichever items are
# non-missing for that respondent -- same renormalise-on-missing idiom as
# wmean_row() in Data Preparation_SWBI.R.
pca_row_score <- function(...) {
  X <- cbind(...)
  if (ncol(X) == 1) return(as.numeric(X))
  w <- pca_weights(X)
  W <- matrix(w, nrow = nrow(X), ncol = ncol(X), byrow = TRUE)
  W[is.na(X)] <- 0
  Xz <- X
  Xz[is.na(X)] <- 0
  num <- rowSums(Xz * W)
  den <- rowSums(W)
  fifelse(den == 0, NA_real_, num / den)
}

## Step 1: per-year item harmonisation ----
# unique_id/weight follow the exact same per-year conventions already used in
# Data Preparation_SWBI.R (birimno+rowid / ff for 2013-2014, BIRIMNO+FERT_NO /
# FAKTOR_FERT for 2015-2017).
#
# local_governance items: B18 (belediye) and B19 (il ozel idare) are mutually
# exclusive per respondent, so each of the 11 matched pairs is coalesced with
# rowmean2() (only one side is ever non-NA) into one lg_* column; the 7
# belediye-only items (no il ozel idare equivalent) are single columns.
#
# sgk_health_service_quality_problem (SORUN_SAG_HIZMET) is sourced from 2013's
# b22_1 only. A b22_1 column also exists in 2014-2017, but the questionnaire
# documentation confirms it holds a *different* item there (b22_1 in 2014+ is
# health_expense_reimbursement_problem, already captured separately below) --
# using it here too would double-count that item under the wrong label, so
# this concept is left NA for 2014-2017 rather than guessed.
#
# school_safety (DEV_OKUL_GUVENLIK/OZEL_OKUL_GUVENLIK) is a 2015+ addition to
# the school-quality battery with no 2013/2014 equivalent (confirmed in
# Data Preparation_SWBI.R's own 2015-2017 notes) -- left NA for those two years.

gnh_2013 <- data_2013[, .(
  unique_id = paste(birimno, rowid(birimno), sep = "_"),
  weight = ff,
  # Psychological Wellbeing / life_evaluation
  happiness_overall = scale_transformation(b9),
  hope_for_own_future = scale_transformation(b40, minimum = 4, maximum = 1),
  # Health / own_health_satisfaction
  health_satisfaction = scale_transformation(b12_1),
  # Health / healthcare_service_facing_issues
  appointment_booking_problem = sorun_scale(b30_1),
  copayment_cost_problem = sorun_scale(b30_10),
  doctor_attitude_problem = sorun_scale(b30_4),
  examination_fee_problem = sorun_scale(b30_7),
  health_expense_reimbursement_problem = sorun_scale(b22_2),
  health_facility_hygiene_problem = sorun_scale(b30_2),
  health_staff_adequacy_problem = evet_good_scale(b30_6),
  medicine_price_problem = sorun_scale(b30_8),
  nurse_attitude_problem = sorun_scale(b30_5),
  problem_at_last_health_visit = sorun_scale(b29),
  sgk_health_service_quality_problem = sorun_scale(b22_1),
  sgk_medicine_purchase_problem = sorun_scale(b22_3),
  waiting_time_problem = sorun_scale(b30_9),
  # Education / access, attainment
  educational_attainment = education_tier(b2, scheme = "2013"),
  own_education_satisfaction = scale_transformation(b12_3),
  # Education / educational_issues (od1-od9; od10/school_safety not asked yet)
  school_registration_problem = rowmean2(sorun_scale(h16_1), sorun_scale(h17_1)),
  school_education_quality = rowmean2(evet_good_scale(h16_2), evet_good_scale(h17_2)),
  school_materials_adequacy = rowmean2(evet_good_scale(h16_3), evet_good_scale(h17_3)),
  school_administration_satisfaction = rowmean2(evet_good_scale(h16_4), evet_good_scale(h17_4)),
  teacher_attitude_satisfaction = rowmean2(evet_good_scale(h16_5), evet_good_scale(h17_5)),
  school_transport_problem = rowmean2(sorun_scale(h16_6), sorun_scale(h17_6)),
  classroom_overcrowding = rowmean2(sorun3_scale(h16_7), sorun3_scale(h17_7)),
  school_costs_perception = rowmean2(sorun3_scale(h16_8), sorun3_scale(h17_8)),
  school_facility_conditions = rowmean2(sorun3_scale(h16_9), sorun3_scale(h17_9)),
  school_safety = NA_real_,
  # Time Use / time_allocation_satisfaction
  personal_leisure_time_satisfaction = scale_transformation(b12_10),
  commute_time_satisfaction = scale_transformation(b12_11),
  # Good Governance / central_government
  egovernment_services_satisfaction = scale_transformation(b16),
  judicial_services_satisfaction = scale_transformation(b14_3),
  medical_examination_satisfaction = evet_good_scale(b30_3),
  public_education_services_satisfaction = scale_transformation(b14_4),
  public_health_services_satisfaction = scale_transformation(b14_1),
  public_security_services_satisfaction = scale_transformation(b14_2),
  social_security_services_satisfaction = scale_transformation(b14_5),
  # Good Governance / judicial_system_problems
  court_procedure_problem = sorun3_scale(b34_1),
  case_resolution_time_problem = sorun3_scale(b34_2),
  judicial_fairness_problem = sorun3_scale(b34_3),
  lawyer_service_quality_problem = sorun3_scale(b34_4),
  trial_process_problem = sorun_scale(b36),
  # Good Governance / local_governance -- 11 coalesced belediye/il ozel idare pairs
  lg_sewage = rowmean2(memnuniyet_scale(b18_2), memnuniyet_scale(b19_1)),
  lg_water = rowmean2(memnuniyet_scale(b18_3), memnuniyet_scale(b19_2)),
  lg_road = rowmean2(memnuniyet_scale(b18_6), memnuniyet_scale(b19_3)),
  lg_zoning = rowmean2(memnuniyet_scale(b18_10), memnuniyet_scale(b19_4)),
  lg_disability = rowmean2(memnuniyet_scale(b18_11), memnuniyet_scale(b19_5)),
  lg_social_assistance = rowmean2(memnuniyet_scale(b18_12), memnuniyet_scale(b19_6)),
  lg_cultural_events = rowmean2(memnuniyet_scale(b18_13), memnuniyet_scale(b19_7)),
  lg_vocational_courses = rowmean2(memnuniyet_scale(b18_14), memnuniyet_scale(b19_8)),
  lg_street_lighting = rowmean2(memnuniyet_scale(b18_15), memnuniyet_scale(b19_9)),
  lg_cleaning = rowmean2(memnuniyet_scale(b18_16), memnuniyet_scale(b19_10)),
  lg_street_signage = rowmean2(memnuniyet_scale(b18_19), memnuniyet_scale(b19_11)),
  # Good Governance / local_governance -- 7 belediye-only items
  lg_waste_collection = memnuniyet_scale(b18_1),
  lg_public_transport = memnuniyet_scale(b18_4),
  lg_police = memnuniyet_scale(b18_5),
  lg_health_sports = memnuniyet_scale(b18_9),
  lg_fire = memnuniyet_scale(b18_17),
  lg_funeral = memnuniyet_scale(b18_18),
  lg_food_inspection = memnuniyet_scale(b18_20),
  # Community Vitality / crime_victimization
  crime_bag_snatching = sorun_scale(b33_1),
  crime_robbery = sorun_scale(b33_2),
  crime_physical_assault = sorun_scale(b33_3),
  crime_mistreatment_by_family_member = sorun_scale(b33_4),
  crime_threat = sorun_scale(b33_5),
  crime_sexual_offense = sorun_scale(b33_6),
  crime_fraud = sorun_scale(b33_7),
  crime_other_victimization = sorun_scale(b33_8),
  crime_household_theft_gateway = sorun_scale(h21),
  crime_home_burglary = sorun_scale(h21_1),
  crime_workplace_burglary = sorun_scale(h21_21),
  crime_farmland_theft = sorun_scale(h21_3),
  crime_car_theft = sorun_scale(h21_41),
  crime_motorcycle_theft = sorun_scale(h21_5),
  crime_other_household_victimization = sorun_scale(h21_6),
  # Community Vitality / police_conduct
  police_response_timeliness = evet_good_scale(b32_1),
  police_behaviour_satisfaction = evet_good_scale(b32_2),
  police_traffic_service_satisfaction = evet_good_scale(b32_3),
  # Community Vitality / relationship_satisfaction
  marriage_satisfaction = scale_transformation(b12_2),
  neighbourhood_satisfaction = scale_transformation(b12_5),
  social_life_satisfaction = scale_transformation(b12_9),
  extended_family_satisfaction = scale_transformation(b13_1),
  friends_satisfaction = scale_transformation(b13_2),
  neighbours_relationship_satisfaction = scale_transformation(b13_3),
  workplace_relationships_satisfaction = scale_transformation(b13_4),
  # Community Vitality / safety_perception
  safety_at_home = scale_transformation(b38),
  safety_around_home = scale_transformation(b39),
  # Community Vitality / social_pressure
  pressure_gender = scale_transformation(b53_1, minimum = 4, maximum = 1),
  pressure_marital_status = scale_transformation(b53_2, minimum = 4, maximum = 1),
  pressure_age = scale_transformation(b53_3, minimum = 4, maximum = 1),
  pressure_tradition = scale_transformation(b53_4, minimum = 4, maximum = 1),
  pressure_religious_belief = scale_transformation(b53_5, minimum = 4, maximum = 1),
  pressure_political_view = scale_transformation(b53_6, minimum = 4, maximum = 1),
  pressure_hometown = scale_transformation(b53_7, minimum = 4, maximum = 1),
  pressure_job = scale_transformation(b53_8, minimum = 4, maximum = 1),
  pressure_clothing = scale_transformation(b53_9, minimum = 4, maximum = 1),
  pressure_unemployment = scale_transformation(b53_10, minimum = 4, maximum = 1),
  pressure_income_level = scale_transformation(b53_11, minimum = 4, maximum = 1),
  # Ecological Diversity and Resilience / environmental_satisfaction
  municipal_green_space_satisfaction = memnuniyet_scale(b18_7), # no il ozel idare item this year
  municipal_pollution_control_satisfaction = memnuniyet_scale(b18_8),
  # Ecological Diversity and Resilience / environmental_issues
  indoor_noise_problem = sorun_scale(h12_4),
  outdoor_noise_problem = sorun_scale(h12_5),
  # Living Standards / housing_satisfaction
  housing_satisfaction = scale_transformation(b12_4),
  # Living Standards / housing_issues
  dwelling_has_bathroom = scale_transformation(h11_3, minimum = 3, maximum = 1),
  dwelling_has_toilet = scale_transformation(h11_2, minimum = 3, maximum = 1),
  dwelling_has_piped_water_system = scale_transformation(h11_1, minimum = 3, maximum = 1),
  dwelling_has_mains_water = scale_transformation(h10, minimum = 2, maximum = 1),
  leaky_roof_damp_walls_problem = sorun_scale(h12_1),
  poor_lighting_problem = sorun_scale(h12_2),
  flooding_problem = sorun_scale(h12_3),
  power_outage_problem = sorun_scale(h12_6),
  water_supply_outage_problem = sorun_scale(h12_7),
  heating_problem = sorun_scale(h12_8),
  # Living Standards / income_and_finances
  income_sufficiency = scale_transformation(h20),
  household_income_satisfaction = scale_transformation(b12_8),
  welfare_ladder_self_placement = scale_transformation(b41, minimum = 1, maximum = 11),
  # Living Standards / asset_ownership
  has_workplace = evet_good_scale(h21_2),
  has_vehicle = evet_good_scale(h21_4),
  owns_dwelling = as.numeric(h7 == 1),
  # Job Satisfaction / job_satisfaction
  job_satisfaction = scale_transformation(b12_6)
)]

gnh_2014 <- data_2014[, .(
  unique_id = paste(birimno, rowid(birimno), sep = "_"),
  weight = ff,
  happiness_overall = scale_transformation(b9),
  hope_for_own_future = scale_transformation(b40, minimum = 4, maximum = 1),
  health_satisfaction = scale_transformation(b12_1),
  appointment_booking_problem = sorun_scale(b30_1),
  copayment_cost_problem = sorun_scale(b30_10),
  doctor_attitude_problem = sorun_scale(b30_4),
  examination_fee_problem = sorun_scale(b30_7),
  health_expense_reimbursement_problem = sorun_scale(b22_1),
  health_facility_hygiene_problem = sorun_scale(b30_2),
  health_staff_adequacy_problem = evet_good_scale(b30_6),
  medicine_price_problem = sorun_scale(b30_8),
  nurse_attitude_problem = sorun_scale(b30_5),
  problem_at_last_health_visit = sorun_scale(b29),
  sgk_health_service_quality_problem = NA_real_, # see Step 1 note above
  sgk_medicine_purchase_problem = sorun_scale(b22_2),
  waiting_time_problem = sorun_scale(b30_9),
  educational_attainment = education_tier(b2, scheme = "2014_2016"),
  own_education_satisfaction = scale_transformation(b12_3),
  school_registration_problem = rowmean2(sorun_scale(h16a_1), sorun_scale(h17a_1)),
  school_education_quality = rowmean2(evet_good_scale(h16a_2), evet_good_scale(h17a_2)),
  school_materials_adequacy = rowmean2(evet_good_scale(h16a_3), evet_good_scale(h17a_3)),
  school_administration_satisfaction = rowmean2(evet_good_scale(h16a_4), evet_good_scale(h17a_4)),
  teacher_attitude_satisfaction = rowmean2(evet_good_scale(h16a_5), evet_good_scale(h17a_5)),
  school_transport_problem = rowmean2(sorun_scale(h16a_6), sorun_scale(h17a_6)),
  classroom_overcrowding = rowmean2(sorun3_scale(h16a_7), sorun3_scale(h17a_7)),
  school_costs_perception = rowmean2(sorun3_scale(h16a_8), sorun3_scale(h17a_8)),
  school_facility_conditions = rowmean2(sorun3_scale(h16a_9), sorun3_scale(h17a_9)),
  school_safety = NA_real_,
  personal_leisure_time_satisfaction = scale_transformation(b12_10),
  commute_time_satisfaction = scale_transformation(b12_11),
  egovernment_services_satisfaction = scale_transformation(b16),
  judicial_services_satisfaction = scale_transformation(b14_3),
  medical_examination_satisfaction = evet_good_scale(b30_3),
  public_education_services_satisfaction = scale_transformation(b14_4),
  public_health_services_satisfaction = scale_transformation(b14_1),
  public_security_services_satisfaction = scale_transformation(b14_2),
  social_security_services_satisfaction = scale_transformation(b14_5),
  court_procedure_problem = sorun3_scale(b34_1),
  case_resolution_time_problem = sorun3_scale(b34_2),
  judicial_fairness_problem = sorun3_scale(b34_3),
  lawyer_service_quality_problem = sorun3_scale(b34_4),
  trial_process_problem = sorun_scale(b36),
  lg_sewage = rowmean2(memnuniyet_scale(b18_2), memnuniyet_scale(b19_1)),
  lg_water = rowmean2(memnuniyet_scale(b18_3), memnuniyet_scale(b19_2)),
  lg_road = rowmean2(memnuniyet_scale(b18_6), memnuniyet_scale(b19_3)),
  lg_zoning = rowmean2(memnuniyet_scale(b18_10), memnuniyet_scale(b19_4)),
  lg_disability = rowmean2(memnuniyet_scale(b18_11), memnuniyet_scale(b19_5)),
  lg_social_assistance = rowmean2(memnuniyet_scale(b18_12), memnuniyet_scale(b19_6)),
  lg_cultural_events = rowmean2(memnuniyet_scale(b18_13), memnuniyet_scale(b19_7)),
  lg_vocational_courses = rowmean2(memnuniyet_scale(b18_14), memnuniyet_scale(b19_8)),
  lg_street_lighting = rowmean2(memnuniyet_scale(b18_15), memnuniyet_scale(b19_9)),
  lg_cleaning = rowmean2(memnuniyet_scale(b18_16), memnuniyet_scale(b19_10)),
  lg_street_signage = rowmean2(memnuniyet_scale(b18_19), memnuniyet_scale(b19_11)),
  lg_waste_collection = memnuniyet_scale(b18_1),
  lg_public_transport = memnuniyet_scale(b18_4),
  lg_police = memnuniyet_scale(b18_5),
  lg_health_sports = memnuniyet_scale(b18_9),
  lg_fire = memnuniyet_scale(b18_17),
  lg_funeral = memnuniyet_scale(b18_18),
  lg_food_inspection = memnuniyet_scale(b18_20),
  crime_bag_snatching = sorun_scale(b33_1),
  crime_robbery = sorun_scale(b33_2),
  crime_physical_assault = sorun_scale(b33_3),
  crime_mistreatment_by_family_member = sorun_scale(b33_4),
  crime_threat = sorun_scale(b33_5),
  crime_sexual_offense = sorun_scale(b33_6),
  crime_fraud = sorun_scale(b33_7),
  crime_other_victimization = sorun_scale(b33_8),
  crime_household_theft_gateway = sorun_scale(h21),
  crime_home_burglary = sorun_scale(h21_1),
  crime_workplace_burglary = sorun_scale(h21_2_1),
  crime_farmland_theft = sorun_scale(h21_3),
  crime_car_theft = sorun_scale(h21_4_1),
  crime_motorcycle_theft = sorun_scale(h21_5),
  crime_other_household_victimization = sorun_scale(h21_6),
  police_response_timeliness = evet_good_scale(b32_1),
  police_behaviour_satisfaction = evet_good_scale(b32_2),
  police_traffic_service_satisfaction = evet_good_scale(b32_3),
  marriage_satisfaction = scale_transformation(b12_2),
  neighbourhood_satisfaction = scale_transformation(b12_5),
  social_life_satisfaction = scale_transformation(b12_9),
  extended_family_satisfaction = scale_transformation(b13_1),
  friends_satisfaction = scale_transformation(b13_2),
  neighbours_relationship_satisfaction = scale_transformation(b13_3),
  workplace_relationships_satisfaction = scale_transformation(b13_4),
  safety_at_home = scale_transformation(b38),
  safety_around_home = scale_transformation(b39),
  pressure_gender = scale_transformation(b53_1, minimum = 4, maximum = 1),
  pressure_marital_status = scale_transformation(b53_2, minimum = 4, maximum = 1),
  pressure_age = scale_transformation(b53_3, minimum = 4, maximum = 1),
  pressure_tradition = scale_transformation(b53_4, minimum = 4, maximum = 1),
  pressure_religious_belief = scale_transformation(b53_5, minimum = 4, maximum = 1),
  pressure_political_view = scale_transformation(b53_6, minimum = 4, maximum = 1),
  pressure_hometown = scale_transformation(b53_7, minimum = 4, maximum = 1),
  pressure_job = scale_transformation(b53_8, minimum = 4, maximum = 1),
  pressure_clothing = scale_transformation(b53_9, minimum = 4, maximum = 1),
  pressure_unemployment = scale_transformation(b53_10, minimum = 4, maximum = 1),
  pressure_income_level = scale_transformation(b53_11, minimum = 4, maximum = 1),
  municipal_green_space_satisfaction = rowmean2(memnuniyet_scale(b18_7), memnuniyet_scale(b19_12)),
  municipal_pollution_control_satisfaction = memnuniyet_scale(b18_8),
  indoor_noise_problem = sorun_scale(h12_4),
  outdoor_noise_problem = sorun_scale(h12_5),
  housing_satisfaction = scale_transformation(b12_4),
  dwelling_has_bathroom = scale_transformation(h11_3, minimum = 3, maximum = 1),
  dwelling_has_toilet = scale_transformation(h11_2, minimum = 3, maximum = 1),
  dwelling_has_piped_water_system = scale_transformation(h11_1, minimum = 3, maximum = 1),
  dwelling_has_mains_water = scale_transformation(h10, minimum = 2, maximum = 1),
  leaky_roof_damp_walls_problem = sorun_scale(h12_1),
  poor_lighting_problem = sorun_scale(h12_2),
  flooding_problem = sorun_scale(h12_3),
  power_outage_problem = sorun_scale(h12_6),
  water_supply_outage_problem = sorun_scale(h12_7),
  heating_problem = sorun_scale(h12_8),
  income_sufficiency = scale_transformation(h20),
  household_income_satisfaction = scale_transformation(b12_8),
  welfare_ladder_self_placement = scale_transformation(b41, minimum = 1, maximum = 11),
  has_workplace = evet_good_scale(h21_2),
  has_vehicle = evet_good_scale(h21_4),
  owns_dwelling = as.numeric(h7 == 1),
  job_satisfaction = scale_transformation(b12_6)
)]

## 2015-2017 ----
# Same semantic-name column layout across all three years (confirmed in
# Data Preparation_SWBI.R's own 2015-2017 notes), so one block is reused for each.
build_gnh_20xx <- function(dt, okul_biten_scheme) {
  dt[, .(
    unique_id = paste(BIRIMNO, FERT_NO, sep = "_"),
    weight = FAKTOR_FERT,
    happiness_overall = scale_transformation(MUTLULUK),
    hope_for_own_future = scale_transformation(UMUT, minimum = 4, maximum = 1),
    health_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_SAGLIK),
    appointment_booking_problem = sorun_scale(SORUN_MUAYNE),
    copayment_cost_problem = sorun_scale(SORUN_KATKIPAY),
    doctor_attitude_problem = sorun_scale(SORUN_DOKTOR),
    examination_fee_problem = sorun_scale(SORUN_UCRET_MUAYENE),
    health_expense_reimbursement_problem = sorun_scale(SORUN_SAGLIK_HARCAMA),
    health_facility_hygiene_problem = sorun_scale(SORUN_HIJYEN),
    health_staff_adequacy_problem = evet_good_scale(SORUN_YETERLI_SGLKPERSONEL),
    medicine_price_problem = sorun_scale(SORUN_ILAC_FIYAT),
    nurse_attitude_problem = sorun_scale(SORUN_HEMSIRE),
    problem_at_last_health_visit = sorun_scale(SORUN_SAGLIK_HIZALIM),
    sgk_health_service_quality_problem = NA_real_, # see Step 1 note above
    sgk_medicine_purchase_problem = sorun_scale(SORUN_ILAC),
    waiting_time_problem = sorun_scale(SORUN_SIRABEKLE),
    educational_attainment = education_tier(OKUL_BITEN, scheme = okul_biten_scheme),
    own_education_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_EGITIM),
    school_registration_problem = rowmean2(sorun_scale(DEV_OKUL_KAYIT), sorun_scale(OZEL_OKUL_KAYIT)),
    school_education_quality = rowmean2(evet_good_scale(DEV_EGT_KALIT), evet_good_scale(OZEL_EGT_KALIT)),
    school_materials_adequacy = rowmean2(evet_good_scale(DEV_EGT_ARAC), evet_good_scale(OZEL_EGT_ARAC)),
    school_administration_satisfaction = rowmean2(evet_good_scale(DEV_OKUL_IDR), evet_good_scale(OZEL_OKUL_IDR)),
    teacher_attitude_satisfaction = rowmean2(evet_good_scale(DEV_OGRETMEN_YAKLASIM), evet_good_scale(OZEL_OGR_YAK)),
    school_transport_problem = rowmean2(sorun_scale(DEV_SERVIS), sorun_scale(OZEL_SERVIS)),
    classroom_overcrowding = rowmean2(sorun3_scale(DEV_OGRENCI_SAYISI), sorun3_scale(OZEL_OGR_SAYI)),
    school_costs_perception = rowmean2(sorun3_scale(DEV_EGT_MASRAF), sorun3_scale(OZEL_EGT_MAS)),
    school_facility_conditions = rowmean2(sorun3_scale(DEV_OKUL_ISINMA), sorun3_scale(OZEL_OKUL_ISIN)),
    school_safety = rowmean2(sorun3_scale(DEV_OKUL_GUVENLIK), sorun3_scale(OZEL_OKUL_GUVENLIK)),
    personal_leisure_time_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_KISISEL_BKM),
    commute_time_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_IS_TRFK_ZMN),
    egovernment_services_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_ELKNIK_KAM),
    judicial_services_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_ADLI),
    medical_examination_satisfaction = evet_good_scale(MEMNUNIYET_MUAYENE),
    public_education_services_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_EGITI),
    public_health_services_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_GN_SAG),
    public_security_services_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_ASAYIS),
    social_security_services_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_SGK),
    court_procedure_problem = sorun3_scale(SORUN_MAH),
    case_resolution_time_problem = sorun3_scale(SORUN_DAVA),
    judicial_fairness_problem = sorun3_scale(SORUN_YASA),
    lawyer_service_quality_problem = sorun3_scale(SORUN_AVUKAT),
    trial_process_problem = sorun_scale(SORUN_MAHSUREC),
    lg_sewage = rowmean2(memnuniyet_scale(MEMNUNIYET_KAN), memnuniyet_scale(MEMNUNIYET_IL_KANAL)),
    lg_water = rowmean2(memnuniyet_scale(MEMNUNIYET_SU), memnuniyet_scale(MEMNUNIYET_IL_SU)),
    lg_road = rowmean2(memnuniyet_scale(MEMNUNIYET_YOL), memnuniyet_scale(MEMNUNIYET_IL_YOL)),
    lg_zoning = rowmean2(memnuniyet_scale(MEMNUNIYET_IMAR), memnuniyet_scale(MEMNUNIYET_IL_ISKAN)),
    lg_disability = rowmean2(memnuniyet_scale(MEMNUNIYET_ENGELLI), memnuniyet_scale(MEMNUNIYET_IL_ENGEL)),
    lg_social_assistance = rowmean2(memnuniyet_scale(MEMNUNIYET_YARDIM), memnuniyet_scale(MEMNUNIYET_IL_HASTA)),
    lg_cultural_events = rowmean2(memnuniyet_scale(MEMNUNIYET_SERGI_FEST), memnuniyet_scale(MEMNUNIYET_IL_SERGI)),
    lg_vocational_courses = rowmean2(memnuniyet_scale(MEMNUNIYET_KURS), memnuniyet_scale(MEMNUNIYET_IL_KURS)),
    lg_street_lighting = rowmean2(memnuniyet_scale(MEMNUNIYET_ISIK), memnuniyet_scale(MEMNUNIYET_IL_ISIK)),
    lg_cleaning = rowmean2(memnuniyet_scale(MEMNUNIYET_TEMIZLIK), memnuniyet_scale(MEMNUNIYET_IL_TEMIZLIK)),
    lg_street_signage = rowmean2(memnuniyet_scale(MEMNUNIYET_LEVHA), memnuniyet_scale(MEMNUNIYET_IL_LEVHA)),
    lg_waste_collection = memnuniyet_scale(MEMNUNIYET_COP),
    lg_public_transport = memnuniyet_scale(MEMNUNIYET_TASIMA),
    lg_police = memnuniyet_scale(MEMNUNIYET_ZABITA),
    lg_health_sports = memnuniyet_scale(MEMNUNIYET_SPOR_MRK),
    lg_fire = memnuniyet_scale(MEMNUNIYET_ITFAIYE),
    lg_funeral = memnuniyet_scale(MEMNUNIYET_CENAZE),
    lg_food_inspection = memnuniyet_scale(MEMNUNIYET_GIDA_TESIS),
    crime_bag_snatching = sorun_scale(KAPKAC),
    crime_robbery = sorun_scale(GASP),
    crime_physical_assault = sorun_scale(DARP),
    crime_mistreatment_by_family_member = sorun_scale(FERT_KOTU),
    crime_threat = sorun_scale(TEHDIT),
    crime_sexual_offense = sorun_scale(MAGDURIYET),
    crime_fraud = sorun_scale(DOLANDIR),
    crime_other_victimization = sorun_scale(BASKASUC),
    crime_household_theft_gateway = sorun_scale(HIRSIZLIK),
    crime_home_burglary = sorun_scale(EV_HIRS),
    crime_workplace_burglary = sorun_scale(IS_HIRS),
    crime_farmland_theft = sorun_scale(TARLA_HIRS),
    crime_car_theft = sorun_scale(OTO_HIRS),
    crime_motorcycle_theft = sorun_scale(MOTOR_HIRS),
    crime_other_household_victimization = sorun_scale(SUC),
    police_response_timeliness = evet_good_scale(POL_JAN__MUDAHALE),
    police_behaviour_satisfaction = evet_good_scale(MEMNUNIYET_POLIS),
    police_traffic_service_satisfaction = evet_good_scale(MEMNUNIYET_TRAFIK),
    marriage_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_EVLILIK),
    neighbourhood_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_SEMT),
    social_life_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_SOS_HAYAT),
    extended_family_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_AKRABA),
    friends_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_ARKADAS),
    neighbours_relationship_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_KOMSU),
    workplace_relationships_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_ISILISKI),
    safety_at_home = scale_transformation(GUVEN_EV),
    safety_around_home = scale_transformation(GUVEN_CEVRE),
    pressure_gender = scale_transformation(CINSIYET_BASKI, minimum = 4, maximum = 1),
    pressure_marital_status = scale_transformation(MEDENI_DURUM_BASKI, minimum = 4, maximum = 1),
    pressure_age = scale_transformation(YAS_BASKI, minimum = 4, maximum = 1),
    pressure_tradition = scale_transformation(GELENEK_GORENEK, minimum = 4, maximum = 1),
    pressure_religious_belief = scale_transformation(DINI_INANCDAN, minimum = 4, maximum = 1),
    pressure_political_view = scale_transformation(SIYASI_GORUSTEN, minimum = 4, maximum = 1),
    pressure_hometown = scale_transformation(MEMLEKETINDEN, minimum = 4, maximum = 1),
    pressure_job = scale_transformation(ISINDEN_DOLAYI, minimum = 4, maximum = 1),
    pressure_clothing = scale_transformation(KILIK_KIYAFET, minimum = 4, maximum = 1),
    pressure_unemployment = scale_transformation(ISSIZ_OLMA, minimum = 4, maximum = 1),
    pressure_income_level = scale_transformation(GELIR_DUZEY_DURUM, minimum = 4, maximum = 1),
    municipal_green_space_satisfaction = rowmean2(memnuniyet_scale(MEMNUNIYET_YESIL), memnuniyet_scale(MEMNUNIYET_IL_YESIL)),
    municipal_pollution_control_satisfaction = memnuniyet_scale(MEMNUNIYET_KIRLILIK),
    indoor_noise_problem = sorun_scale(K_GURULTU),
    outdoor_noise_problem = sorun_scale(S_GURULTU),
    housing_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_KONUT),
    dwelling_has_bathroom = scale_transformation(KONUT_BANYO, minimum = 3, maximum = 1),
    dwelling_has_toilet = scale_transformation(KONUT_TUVALET, minimum = 3, maximum = 1),
    dwelling_has_piped_water_system = scale_transformation(SU_SISTEMI, minimum = 3, maximum = 1),
    dwelling_has_mains_water = scale_transformation(KONUT_SEBEKE_SUYU, minimum = 2, maximum = 1),
    leaky_roof_damp_walls_problem = sorun_scale(CATI),
    poor_lighting_problem = sorun_scale(KARANLIK),
    flooding_problem = sorun_scale(SU_BASKINI),
    power_outage_problem = sorun_scale(ELEK_KESINTI),
    water_supply_outage_problem = sorun_scale(SEBEKE_SUYU_KESINTI),
    heating_problem = sorun_scale(ISINMA_PROBLEM),
    income_sufficiency = scale_transformation(OLCEK_GELIR_KARSILAMA),
    household_income_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_GELIR),
    welfare_ladder_self_placement = scale_transformation(UMUT_BASAMAK, minimum = 1, maximum = 11),
    has_workplace = evet_good_scale(ISYERI_VAR),
    has_vehicle = evet_good_scale(OTO_VAR),
    owns_dwelling = as.numeric(MULKIYET == 1),
    job_satisfaction = scale_transformation(OLCEK_MEMNUNIYET_IS)
  )]
}
gnh_2015 <- build_gnh_20xx(data_2015, okul_biten_scheme = "2014_2016")
gnh_2016 <- build_gnh_20xx(data_2016, okul_biten_scheme = "2014_2016")
gnh_2017 <- build_gnh_20xx(data_2017, okul_biten_scheme = "2017")

## Step 2: append years, compute domain scores ----
gnh_panel <- rbindlist(list(gnh_2013, gnh_2014, gnh_2015, gnh_2016, gnh_2017), idcol = "year", fill = TRUE)
gnh_panel[, year := c(2013:2017)[year]]

# Sub-domain scores -- combination method follows the mapping CSV / GNH Terminology.md:
# a single item is used as-is; a *_issues/*_problems/crime/pressure battery uses
# any_issue_score(); asset_ownership uses owns_any_score(); everything else with
# 2+ Likert-type items uses the PCA-weighted average pca_row_score().
gnh_panel[, life_evaluation := pca_row_score(happiness_overall, hope_for_own_future)]

gnh_panel[, own_health_satisfaction := health_satisfaction]
gnh_panel[, healthcare_service_facing_issues := any_issue_score(
  appointment_booking_problem, copayment_cost_problem, doctor_attitude_problem,
  examination_fee_problem, health_expense_reimbursement_problem, health_facility_hygiene_problem,
  health_staff_adequacy_problem, medicine_price_problem, nurse_attitude_problem,
  problem_at_last_health_visit, sgk_health_service_quality_problem, sgk_medicine_purchase_problem,
  waiting_time_problem
)]

gnh_panel[, access := educational_attainment]
gnh_panel[, attainment := own_education_satisfaction]
gnh_panel[, educational_issues := any_issue_score(
  school_registration_problem, school_education_quality, school_materials_adequacy,
  school_administration_satisfaction, teacher_attitude_satisfaction, school_transport_problem,
  classroom_overcrowding, school_costs_perception, school_facility_conditions, school_safety
)]

gnh_panel[, time_allocation_satisfaction := pca_row_score(personal_leisure_time_satisfaction, commute_time_satisfaction)]

gnh_panel[, central_government := pca_row_score(
  egovernment_services_satisfaction, judicial_services_satisfaction, medical_examination_satisfaction,
  public_education_services_satisfaction, public_health_services_satisfaction,
  public_security_services_satisfaction, social_security_services_satisfaction
)]
gnh_panel[, judicial_system_problems := any_issue_score(
  court_procedure_problem, case_resolution_time_problem, judicial_fairness_problem,
  lawyer_service_quality_problem, trial_process_problem
)]
gnh_panel[, local_governance := pca_row_score(
  lg_sewage, lg_water, lg_road, lg_zoning, lg_disability, lg_social_assistance,
  lg_cultural_events, lg_vocational_courses, lg_street_lighting, lg_cleaning, lg_street_signage,
  lg_waste_collection, lg_public_transport, lg_police, lg_health_sports, lg_fire, lg_funeral,
  lg_food_inspection
)]

gnh_panel[, crime_victimization := any_issue_score(
  crime_bag_snatching, crime_robbery, crime_physical_assault, crime_mistreatment_by_family_member,
  crime_threat, crime_sexual_offense, crime_fraud, crime_other_victimization,
  crime_household_theft_gateway, crime_home_burglary, crime_workplace_burglary, crime_farmland_theft,
  crime_car_theft, crime_motorcycle_theft, crime_other_household_victimization
)]
gnh_panel[, social_pressure := any_issue_score(
  pressure_gender, pressure_marital_status, pressure_age, pressure_tradition, pressure_religious_belief,
  pressure_political_view, pressure_hometown, pressure_job, pressure_clothing, pressure_unemployment,
  pressure_income_level
)]
gnh_panel[, relationship_satisfaction := pca_row_score(
  marriage_satisfaction, neighbourhood_satisfaction, social_life_satisfaction, extended_family_satisfaction,
  friends_satisfaction, neighbours_relationship_satisfaction, workplace_relationships_satisfaction
)]
gnh_panel[, police_conduct := pca_row_score(police_response_timeliness, police_behaviour_satisfaction, police_traffic_service_satisfaction)]
gnh_panel[, safety_perception := pca_row_score(safety_at_home, safety_around_home)]

gnh_panel[, environmental_satisfaction := pca_row_score(municipal_green_space_satisfaction, municipal_pollution_control_satisfaction)]
gnh_panel[, environmental_issues := any_issue_score(indoor_noise_problem, outdoor_noise_problem)]

gnh_panel[, housing_satisfaction_sub := housing_satisfaction]
gnh_panel[, housing_issues := any_issue_score(
  dwelling_has_bathroom, dwelling_has_toilet, dwelling_has_piped_water_system, dwelling_has_mains_water,
  leaky_roof_damp_walls_problem, poor_lighting_problem, flooding_problem, power_outage_problem,
  water_supply_outage_problem, heating_problem
)]
gnh_panel[, income_and_finances := pca_row_score(income_sufficiency, household_income_satisfaction, welfare_ladder_self_placement)]
gnh_panel[, asset_ownership := owns_any_score(has_workplace, has_vehicle, owns_dwelling)]

gnh_panel[, job_satisfaction_sub := job_satisfaction]

# Domain scores -- a domain with a single sub-domain takes that score directly;
# a domain with 2+ sub-domains combines them with pca_row_score().
gnh_panel[, dom_psychological_wellbeing := life_evaluation]
gnh_panel[, dom_health := pca_row_score(own_health_satisfaction, healthcare_service_facing_issues)]
gnh_panel[, dom_education := pca_row_score(access, attainment, educational_issues)]
gnh_panel[, dom_time_use := time_allocation_satisfaction]
gnh_panel[, dom_good_governance := pca_row_score(local_governance, central_government, judicial_system_problems)]
gnh_panel[, dom_community_vitality := pca_row_score(
  crime_victimization, social_pressure, relationship_satisfaction, police_conduct, safety_perception
)]
gnh_panel[, dom_ecological_diversity := pca_row_score(environmental_satisfaction, environmental_issues)]
gnh_panel[, dom_living_standards := pca_row_score(housing_satisfaction_sub, housing_issues, income_and_finances, asset_ownership)]
gnh_panel[, dom_job_satisfaction := job_satisfaction_sub]

# Overall GNH composite -- PCA-weighted average across the 9 domain scores.
gnh_panel[, gnh := pca_row_score(
  dom_psychological_wellbeing, dom_health, dom_education, dom_time_use, dom_good_governance,
  dom_community_vitality, dom_ecological_diversity, dom_living_standards, dom_job_satisfaction
)]

## Step 3: weighted summary by year ----
gnh_summary_by_year <- gnh_panel[, .(
  psychological_wellbeing = weighted.mean(dom_psychological_wellbeing, weight, na.rm = TRUE),
  health                  = weighted.mean(dom_health, weight, na.rm = TRUE),
  education               = weighted.mean(dom_education, weight, na.rm = TRUE),
  time_use                = weighted.mean(dom_time_use, weight, na.rm = TRUE),
  good_governance         = weighted.mean(dom_good_governance, weight, na.rm = TRUE),
  community_vitality      = weighted.mean(dom_community_vitality, weight, na.rm = TRUE),
  ecological_diversity    = weighted.mean(dom_ecological_diversity, weight, na.rm = TRUE),
  living_standards        = weighted.mean(dom_living_standards, weight, na.rm = TRUE),
  job_satisfaction        = weighted.mean(dom_job_satisfaction, weight, na.rm = TRUE),
  gnh                     = weighted.mean(gnh, weight, na.rm = TRUE)
), by = year][order(year)]
fwrite(gnh_summary_by_year, "agg_data/gnh_turkey_2013_2017.csv", sep = "|", dec = ",")

# Source Files Import ----
rm(list = ls())
gc()
source("https://raw.githubusercontent.com/kazimanil/tma/master/functions.R") # Base functions for data manipulation
source("https://raw.githubusercontent.com/kazimanil/project_kaf/master/R_functions/gg_theme.R") # My Theme for GGPlot2

# Libraries ----
library("data.table")
library("ggplot2")
library("plotly")

# Raw Data Import ----
# The Data is shared by TURKSTAT on the promise that it will not be shared publicly.
# Thus, I will only be able share aggregated data after the manipulation & aggregation steps.
#
# One combined table per year (panel_YYYY) holding both the perception/
# satisfaction predictors and the objective occurrence/battery predictors
# (life events, crime, housing issues, social pressure, comparison &
# reputation concern, future outlook -- each item coded 0/1 or its own small
# ordinal factor rather than fused into one lossy summary variable), plus 9
# group-level flags (1 = at least one item in the corresponding battery
# fired). Three logistic regressions run against that same table:
#   main_YYYY_results     -- perception / satisfaction based predictors
#   events_YYYY_results   -- objective occurrence / battery based predictors
#   combined_YYYY_results -- Main plus the Events terms forward-selected on
#                            top of it (see COMBINED_FORMULA_VARS below)

## 2013 ----
data_2013 <- fread("data/data_2013.csv")
panel_2013 <- data_2013[, .(
    survey_weight = as.numeric(ff),
    happiness_ordered = scale_transformation(b9),
    happiness_binary = happiness_transformation(b9),
    gender = relevel(as.factor(gender(cins)), "Female"),
    age = as.numeric(yas),
    age_squared = as.numeric(yas)^2,
    household_size = as.numeric(hhb),
    marital_status_satisfaction = relevel(as.factor(marriage_satisfaction(marital_status(b1), scale_transformation(b12_2))), "Single"),
    education_level = relevel(as.factor(education_level(b2, scheme = "2013")), "No Schooling"),
    education_satisfaction = relevel(as.factor(likert_categoric(b12_3, "Education")), "Neutral"),
    employment_job_satisfaction = relevel(as.factor(job_satisfaction(employment(b4, b5, TRUE), scale_transformation(b12_6))), "Out of Labour Force"),
    materialism = relevel(as.factor(materialism(b11)), "Not Materialistic"),
    wellbeing_ladder = relevel(as.factor(paste0("Wellbeing Ladder: ", wellbeing_ladder_transformation(b41, legacy_coding = TRUE))), "Wellbeing Ladder: 0"),
    hope_level = relevel(as.factor(hope_transformation(b40)), "Not Hopeful At All"),
    household_income_tier = relevel(as.factor(household_income_transformation(h19)), "Household Income Tier 1"),
    income_sufficiency = relevel(as.factor(likert_categoric(h20, "Income Sufficiency")), "Neutral"),
    income_satisfaction = relevel(as.factor(likert_categoric(b12_8, "Income")), "Neutral"),
    health_satisfaction = relevel(as.factor(likert_categoric(b12_1, "Subjective Health")), "Neutral"),
    housing_satisfaction = relevel(as.factor(likert_categoric(b12_4, "Housing")), "Neutral"),
    housing_tenure = relevel(as.factor(housing_tenure_transformation(h7)), "Owner"),
    neighbourhood_satisfaction = relevel(as.factor(likert_categoric(b12_5, "Neighbourhood")), "Neutral"),
    friends_satisfaction = relevel(as.factor(likert_categoric(b13_2, "Friends")), "Neutral"),
    extended_family_satisfaction = relevel(as.factor(likert_categoric(b13_1, "Extended Family")), "Neutral"),
    neighbours_satisfaction = relevel(as.factor(likert_categoric(b13_3, "Neighbours")), "Neutral"),
    social_life_satisfaction = relevel(as.factor(likert_categoric(b12_9, "Social Life")), "Neutral"),
    leisure_satisfaction = relevel(as.factor(likert_categoric(b12_10, "Leisure")), "Neutral"),
    safety_at_home = relevel(as.factor(likert_categoric(b38, "Safety at Home")), "Neutral"),
    safety_around_home = relevel(as.factor(likert_categoric(b39, "Safety Around Home")), "Neutral"),
    comparison_to_5y_ago = relevel(as.factor(comparison_5y_ago(b42)), "No Idea"),
    expectation_5y_later = relevel(as.factor(expectations_5y_later(b43)), "No Idea"),
    migration_experience = relevel(as.factor(ifelse(b48_11 == 1, "Migrated", "Not Migrated")), "Not Migrated"),
    religiosity = relevel(as.factor(religiosity_transformation(b54_9)), "Not Religious"),
    got_married = relevel(as.factor(yes_no_transformation(b49_1, na_as_no = TRUE)), "No"),
    got_a_job = relevel(as.factor(yes_no_transformation(b48_1, na_as_no = TRUE)), "No"),
    opened_a_business = relevel(as.factor(yes_no_transformation(b48_2, na_as_no = TRUE)), "No"),
    income_increased = relevel(as.factor(yes_no_transformation(b48_6, na_as_no = TRUE)), "No"),
    started_saving = relevel(as.factor(yes_no_transformation(b48_8)), "No"),
    bought_a_car = relevel(as.factor(yes_no_transformation(b48_12)), "No"),
    bought_a_house = relevel(as.factor(yes_no_transformation(b48_13)), "No"),
    paid_off_debt = relevel(as.factor(yes_no_transformation(b48_17, na_as_no = TRUE)), "No"),
    had_a_child = relevel(as.factor(yes_no_transformation(b49_3)), "No"),
    lost_job = relevel(as.factor(yes_no_transformation(b48_3, na_as_no = TRUE)), "No"),
    went_bankrupt = relevel(as.factor(yes_no_transformation(b48_4, na_as_no = TRUE)), "No"),
    income_decreased = relevel(as.factor(yes_no_transformation(b48_5, na_as_no = TRUE)), "No"),
    savings_decreased = relevel(as.factor(yes_no_transformation(b48_7, na_as_no = TRUE)), "No"),
    bought_cheaper_products = relevel(as.factor(yes_no_transformation(b48_9)), "No"),
    cut_vacation_spending = relevel(as.factor(yes_no_transformation(b48_10, na_as_no = TRUE)), "No"),
    sold_car = relevel(as.factor(yes_no_transformation(b48_14, na_as_no = TRUE)), "No"),
    sold_house = relevel(as.factor(yes_no_transformation(b48_15, na_as_no = TRUE)), "No"),
    went_into_debt = relevel(as.factor(yes_no_transformation(b48_16)), "No"),
    got_divorced = relevel(as.factor(yes_no_transformation(b49_2, na_as_no = TRUE)), "No"),
    had_a_bereavement = relevel(as.factor(yes_no_transformation(b49_4)), "No"),
    had_a_health_problem = relevel(as.factor(yes_no_transformation(b49_8)), "No"),
    pressure_gender = relevel(as.factor(social_pressure_transformation(b53_1)), "No Pressure"),
    pressure_marital_status = relevel(as.factor(social_pressure_transformation(b53_2)), "No Pressure"),
    pressure_age = relevel(as.factor(social_pressure_transformation(b53_3)), "No Pressure"),
    pressure_tradition = relevel(as.factor(social_pressure_transformation(b53_4)), "No Pressure"),
    pressure_religious_belief = relevel(as.factor(social_pressure_transformation(b53_5)), "No Pressure"),
    pressure_political_view = relevel(as.factor(social_pressure_transformation(b53_6)), "No Pressure"),
    pressure_hometown = relevel(as.factor(social_pressure_transformation(b53_7)), "No Pressure"),
    pressure_clothing = relevel(as.factor(social_pressure_transformation(b53_9)), "No Pressure"),
    pressure_income_level = relevel(as.factor(social_pressure_transformation(b53_11)), "No Pressure"),
    crime_bag_snatching = relevel(as.factor(yes_no_transformation(b33_1)), "No"),
    crime_robbery = relevel(as.factor(yes_no_transformation(b33_2)), "No"),
    crime_physical_assault = relevel(as.factor(yes_no_transformation(b33_3)), "No"),
    crime_mistreatment = relevel(as.factor(yes_no_transformation(b33_4)), "No"),
    crime_threat = relevel(as.factor(yes_no_transformation(b33_5)), "No"),
    crime_victimization_other = relevel(as.factor(yes_no_transformation(b33_6)), "No"),
    crime_fraud = relevel(as.factor(yes_no_transformation(b33_7)), "No"),
    crime_witnessed_someone_elses_crime = relevel(as.factor(yes_no_transformation(b33_8)), "No"),
    crime_burglary_general = relevel(as.factor(yes_no_transformation(h21)), "No"),
    crime_home_burglary = relevel(as.factor(yes_no_transformation(h21_1, na_as_no = TRUE)), "No"),
    crime_workplace_burglary = relevel(as.factor(yes_no_transformation(h21_21, na_as_no = TRUE)), "No"),
    crime_farmland_theft = relevel(as.factor(yes_no_transformation(h21_3, na_as_no = TRUE)), "No"),
    crime_car_theft = relevel(as.factor(yes_no_transformation(h21_41, na_as_no = TRUE)), "No"),
    crime_motorcycle_theft = relevel(as.factor(yes_no_transformation(h21_5, na_as_no = TRUE)), "No"),
    crime_other = relevel(as.factor(yes_no_transformation(h21_6, na_as_no = TRUE)), "No"),
    leaky_roof = relevel(as.factor(yes_no_transformation(h12_1)), "No"),
    poor_lighting = relevel(as.factor(yes_no_transformation(h12_2)), "No"),
    flooding = relevel(as.factor(yes_no_transformation(h12_3)), "No"),
    indoor_noise = relevel(as.factor(yes_no_transformation(h12_4)), "No"),
    outdoor_noise = relevel(as.factor(yes_no_transformation(h12_5)), "No"),
    power_outages = relevel(as.factor(yes_no_transformation(h12_6)), "No"),
    heating_problem = relevel(as.factor(yes_no_transformation(h12_8)), "No"),
    concern_clothing = relevel(as.factor(comparison_concern_transformation(b51_1)), "Not Important"),
    concern_family_lifestyle = relevel(as.factor(comparison_concern_transformation(b51_2)), "Not Important"),
    concern_personal_belongings = relevel(as.factor(comparison_concern_transformation(b51_3)), "Not Important"),
    concern_friend_circle = relevel(as.factor(comparison_concern_transformation(b51_4)), "Not Important"),
    concern_childrens_success = relevel(as.factor(comparison_concern_transformation(b51_5)), "Not Important"),
    concern_curfew_hours = relevel(as.factor(comparison_concern_transformation(b51_6)), "Not Important"),
    concern_occupation = relevel(as.factor(comparison_concern_transformation(b51_7)), "Not Important"),
    concern_income_level = relevel(as.factor(comparison_concern_transformation(b51_8)), "Not Important"),
    concern_religious_belief = relevel(as.factor(comparison_concern_transformation(b51_9)), "Not Important"),
    concern_political_view = relevel(as.factor(comparison_concern_transformation(b51_10)), "Not Important"),
    concern_education_level = relevel(as.factor(comparison_concern_transformation(b51_11)), "Not Important"),
    reputation_clothing = relevel(as.factor(comparison_concern_transformation(b52_1)), "Not Important"),
    reputation_family_lifestyle = relevel(as.factor(comparison_concern_transformation(b52_2)), "Not Important"),
    reputation_personal_belongings = relevel(as.factor(comparison_concern_transformation(b52_3)), "Not Important"),
    reputation_friend_circle = relevel(as.factor(comparison_concern_transformation(b52_4)), "Not Important"),
    reputation_childrens_success = relevel(as.factor(comparison_concern_transformation(b52_5)), "Not Important"),
    reputation_curfew_hours = relevel(as.factor(comparison_concern_transformation(b52_6)), "Not Important"),
    reputation_income_level = relevel(as.factor(comparison_concern_transformation(b52_8)), "Not Important"),
    reputation_religious_belief = relevel(as.factor(comparison_concern_transformation(b52_9)), "Not Important"),
    reputation_political_view = relevel(as.factor(comparison_concern_transformation(b52_10)), "Not Important"),
    reputation_education_level = relevel(as.factor(comparison_concern_transformation(b52_11)), "Not Important"),
    outlook_life_in_general = relevel(as.factor(future_outlook_transformation(b44_1)), "Same / No Opinion"),
    outlook_personal_job = relevel(as.factor(future_outlook_transformation(b44_2)), "Same / No Opinion"),
    outlook_household_finances = relevel(as.factor(future_outlook_transformation(b44_3)), "Same / No Opinion"),
    outlook_national_employment = relevel(as.factor(future_outlook_transformation(b44_4)), "Same / No Opinion"),
    outlook_national_economy = relevel(as.factor(future_outlook_transformation(b44_5)), "Same / No Opinion")
)]
panel_2013[, `:=`(
    has_positive_life_event = relevel(as.factor(ifelse(pmax(ifelse(got_married == "Yes", 1, 0), ifelse(got_a_job == "Yes", 1, 0), ifelse(opened_a_business == "Yes", 1, 0), ifelse(income_increased == "Yes", 1, 0), ifelse(started_saving == "Yes", 1, 0), ifelse(bought_a_car == "Yes", 1, 0), ifelse(bought_a_house == "Yes", 1, 0), ifelse(paid_off_debt == "Yes", 1, 0), ifelse(had_a_child == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    has_negative_life_event = relevel(as.factor(ifelse(pmax(ifelse(lost_job == "Yes", 1, 0), ifelse(went_bankrupt == "Yes", 1, 0), ifelse(income_decreased == "Yes", 1, 0), ifelse(savings_decreased == "Yes", 1, 0), ifelse(bought_cheaper_products == "Yes", 1, 0), ifelse(cut_vacation_spending == "Yes", 1, 0), ifelse(sold_car == "Yes", 1, 0), ifelse(sold_house == "Yes", 1, 0), ifelse(went_into_debt == "Yes", 1, 0), ifelse(got_divorced == "Yes", 1, 0), ifelse(had_a_bereavement == "Yes", 1, 0), ifelse(had_a_health_problem == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    faced_social_pressure = relevel(as.factor(ifelse(pmax(ifelse(pressure_gender == "Felt Pressure", 1, 0), ifelse(pressure_marital_status == "Felt Pressure", 1, 0), ifelse(pressure_age == "Felt Pressure", 1, 0), ifelse(pressure_tradition == "Felt Pressure", 1, 0), ifelse(pressure_religious_belief == "Felt Pressure", 1, 0), ifelse(pressure_political_view == "Felt Pressure", 1, 0), ifelse(pressure_hometown == "Felt Pressure", 1, 0), ifelse(pressure_clothing == "Felt Pressure", 1, 0), ifelse(pressure_income_level == "Felt Pressure", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    experienced_crime = relevel(as.factor(ifelse(pmax(ifelse(crime_bag_snatching == "Yes", 1, 0), ifelse(crime_robbery == "Yes", 1, 0), ifelse(crime_physical_assault == "Yes", 1, 0), ifelse(crime_mistreatment == "Yes", 1, 0), ifelse(crime_threat == "Yes", 1, 0), ifelse(crime_victimization_other == "Yes", 1, 0), ifelse(crime_fraud == "Yes", 1, 0), ifelse(crime_witnessed_someone_elses_crime == "Yes", 1, 0), ifelse(crime_burglary_general == "Yes", 1, 0), ifelse(crime_home_burglary == "Yes", 1, 0), ifelse(crime_workplace_burglary == "Yes", 1, 0), ifelse(crime_farmland_theft == "Yes", 1, 0), ifelse(crime_car_theft == "Yes", 1, 0), ifelse(crime_motorcycle_theft == "Yes", 1, 0), ifelse(crime_other == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    is_faced_with_a_housing_issue = relevel(as.factor(ifelse(pmax(ifelse(leaky_roof == "Yes", 1, 0), ifelse(poor_lighting == "Yes", 1, 0), ifelse(flooding == "Yes", 1, 0), ifelse(indoor_noise == "Yes", 1, 0), ifelse(outdoor_noise == "Yes", 1, 0), ifelse(power_outages == "Yes", 1, 0), ifelse(heating_problem == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    has_comparison_concern = relevel(as.factor(ifelse(pmax(ifelse(concern_clothing == "Important", 1, 0), ifelse(concern_family_lifestyle == "Important", 1, 0), ifelse(concern_personal_belongings == "Important", 1, 0), ifelse(concern_friend_circle == "Important", 1, 0), ifelse(concern_childrens_success == "Important", 1, 0), ifelse(concern_curfew_hours == "Important", 1, 0), ifelse(concern_occupation == "Important", 1, 0), ifelse(concern_income_level == "Important", 1, 0), ifelse(concern_religious_belief == "Important", 1, 0), ifelse(concern_political_view == "Important", 1, 0), ifelse(concern_education_level == "Important", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    has_reputation_concern = relevel(as.factor(ifelse(pmax(ifelse(reputation_clothing == "Important", 1, 0), ifelse(reputation_family_lifestyle == "Important", 1, 0), ifelse(reputation_personal_belongings == "Important", 1, 0), ifelse(reputation_friend_circle == "Important", 1, 0), ifelse(reputation_childrens_success == "Important", 1, 0), ifelse(reputation_curfew_hours == "Important", 1, 0), ifelse(reputation_income_level == "Important", 1, 0), ifelse(reputation_religious_belief == "Important", 1, 0), ifelse(reputation_political_view == "Important", 1, 0), ifelse(reputation_education_level == "Important", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    expects_personal_outlook_to_worsen = relevel(as.factor(ifelse(pmax(ifelse(outlook_life_in_general == "Expects Worse", 1, 0), ifelse(outlook_personal_job == "Expects Worse", 1, 0), ifelse(outlook_household_finances == "Expects Worse", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    expects_national_outlook_to_worsen = relevel(as.factor(ifelse(pmax(ifelse(outlook_national_employment == "Expects Worse", 1, 0), ifelse(outlook_national_economy == "Expects Worse", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No")
)]

## 2014 ----
data_2014 <- fread("data/data_2014.csv")
panel_2014 <- data_2014[, .(
    survey_weight = as.numeric(ff),
    happiness_ordered = scale_transformation(b9),
    happiness_binary = happiness_transformation(b9),
    gender = relevel(as.factor(gender(cins)), "Female"),
    age = as.numeric(yas),
    age_squared = as.numeric(yas)^2,
    household_size = as.numeric(hhb),
    marital_status_satisfaction = relevel(as.factor(marriage_satisfaction(marital_status(b1), scale_transformation(b12_2))), "Single"),
    education_level = relevel(as.factor(education_level(b2, scheme = "2014_2016")), "No Schooling"),
    education_satisfaction = relevel(as.factor(likert_categoric(b12_3, "Education")), "Neutral"),
    employment_job_satisfaction = relevel(as.factor(job_satisfaction(employment(b4, b5, TRUE), scale_transformation(b12_6))), "Out of Labour Force"),
    materialism = relevel(as.factor(materialism(b11)), "Not Materialistic"),
    wellbeing_ladder = relevel(as.factor(paste0("Wellbeing Ladder: ", wellbeing_ladder_transformation(b41, legacy_coding = TRUE))), "Wellbeing Ladder: 0"),
    hope_level = relevel(as.factor(hope_transformation(b40)), "Not Hopeful At All"),
    household_income_tier = relevel(as.factor(household_income_transformation(h19)), "Household Income Tier 1"),
    income_sufficiency = relevel(as.factor(likert_categoric(h20, "Income Sufficiency")), "Neutral"),
    income_satisfaction = relevel(as.factor(likert_categoric(b12_8, "Income")), "Neutral"),
    health_satisfaction = relevel(as.factor(likert_categoric(b12_1, "Subjective Health")), "Neutral"),
    housing_satisfaction = relevel(as.factor(likert_categoric(b12_4, "Housing")), "Neutral"),
    housing_tenure = relevel(as.factor(housing_tenure_transformation(h7)), "Owner"),
    neighbourhood_satisfaction = relevel(as.factor(likert_categoric(b12_5, "Neighbourhood")), "Neutral"),
    friends_satisfaction = relevel(as.factor(likert_categoric(b13_2, "Friends")), "Neutral"),
    extended_family_satisfaction = relevel(as.factor(likert_categoric(b13_1, "Extended Family")), "Neutral"),
    neighbours_satisfaction = relevel(as.factor(likert_categoric(b13_3, "Neighbours")), "Neutral"),
    social_life_satisfaction = relevel(as.factor(likert_categoric(b12_9, "Social Life")), "Neutral"),
    leisure_satisfaction = relevel(as.factor(likert_categoric(b12_10, "Leisure")), "Neutral"),
    safety_at_home = relevel(as.factor(likert_categoric(b38, "Safety at Home")), "Neutral"),
    safety_around_home = relevel(as.factor(likert_categoric(b39, "Safety Around Home")), "Neutral"),
    comparison_to_5y_ago = relevel(as.factor(comparison_5y_ago(b42)), "No Idea"),
    expectation_5y_later = relevel(as.factor(expectations_5y_later(b43)), "No Idea"),
    migration_experience = relevel(as.factor(ifelse(b48_11 == 1, "Migrated", "Not Migrated")), "Not Migrated"),
    religiosity = relevel(as.factor(religiosity_transformation(b54_9)), "Not Religious"),
    got_married = relevel(as.factor(yes_no_transformation(b49_1, na_as_no = TRUE)), "No"),
    got_a_job = relevel(as.factor(yes_no_transformation(b48_1, na_as_no = TRUE)), "No"),
    opened_a_business = relevel(as.factor(yes_no_transformation(b48_2, na_as_no = TRUE)), "No"),
    income_increased = relevel(as.factor(yes_no_transformation(b48_6, na_as_no = TRUE)), "No"),
    started_saving = relevel(as.factor(yes_no_transformation(b48_8)), "No"),
    bought_a_car = relevel(as.factor(yes_no_transformation(b48_12)), "No"),
    bought_a_house = relevel(as.factor(yes_no_transformation(b48_13)), "No"),
    paid_off_debt = relevel(as.factor(yes_no_transformation(b48_17, na_as_no = TRUE)), "No"),
    had_a_child = relevel(as.factor(yes_no_transformation(b49_3)), "No"),
    lost_job = relevel(as.factor(yes_no_transformation(b48_3, na_as_no = TRUE)), "No"),
    went_bankrupt = relevel(as.factor(yes_no_transformation(b48_4, na_as_no = TRUE)), "No"),
    income_decreased = relevel(as.factor(yes_no_transformation(b48_5, na_as_no = TRUE)), "No"),
    savings_decreased = relevel(as.factor(yes_no_transformation(b48_7, na_as_no = TRUE)), "No"),
    bought_cheaper_products = relevel(as.factor(yes_no_transformation(b48_9)), "No"),
    cut_vacation_spending = relevel(as.factor(yes_no_transformation(b48_10, na_as_no = TRUE)), "No"),
    sold_car = relevel(as.factor(yes_no_transformation(b48_14, na_as_no = TRUE)), "No"),
    sold_house = relevel(as.factor(yes_no_transformation(b48_15, na_as_no = TRUE)), "No"),
    went_into_debt = relevel(as.factor(yes_no_transformation(b48_16)), "No"),
    got_divorced = relevel(as.factor(yes_no_transformation(b49_2, na_as_no = TRUE)), "No"),
    had_a_bereavement = relevel(as.factor(yes_no_transformation(b49_4)), "No"),
    had_a_health_problem = relevel(as.factor(yes_no_transformation(b49_8)), "No"),
    pressure_gender = relevel(as.factor(social_pressure_transformation(b53_1)), "No Pressure"),
    pressure_marital_status = relevel(as.factor(social_pressure_transformation(b53_2)), "No Pressure"),
    pressure_age = relevel(as.factor(social_pressure_transformation(b53_3)), "No Pressure"),
    pressure_tradition = relevel(as.factor(social_pressure_transformation(b53_4)), "No Pressure"),
    pressure_religious_belief = relevel(as.factor(social_pressure_transformation(b53_5)), "No Pressure"),
    pressure_political_view = relevel(as.factor(social_pressure_transformation(b53_6)), "No Pressure"),
    pressure_hometown = relevel(as.factor(social_pressure_transformation(b53_7)), "No Pressure"),
    pressure_clothing = relevel(as.factor(social_pressure_transformation(b53_9)), "No Pressure"),
    pressure_income_level = relevel(as.factor(social_pressure_transformation(b53_11)), "No Pressure"),
    crime_bag_snatching = relevel(as.factor(yes_no_transformation(b33_1)), "No"),
    crime_robbery = relevel(as.factor(yes_no_transformation(b33_2)), "No"),
    crime_physical_assault = relevel(as.factor(yes_no_transformation(b33_3)), "No"),
    crime_mistreatment = relevel(as.factor(yes_no_transformation(b33_4)), "No"),
    crime_threat = relevel(as.factor(yes_no_transformation(b33_5)), "No"),
    crime_victimization_other = relevel(as.factor(yes_no_transformation(b33_6)), "No"),
    crime_fraud = relevel(as.factor(yes_no_transformation(b33_7)), "No"),
    crime_witnessed_someone_elses_crime = relevel(as.factor(yes_no_transformation(b33_8)), "No"),
    crime_burglary_general = relevel(as.factor(yes_no_transformation(h21)), "No"),
    crime_home_burglary = relevel(as.factor(yes_no_transformation(h21_1, na_as_no = TRUE)), "No"),
    crime_workplace_burglary = relevel(as.factor(yes_no_transformation(h21_2_1, na_as_no = TRUE)), "No"),
    crime_farmland_theft = relevel(as.factor(yes_no_transformation(h21_3, na_as_no = TRUE)), "No"),
    crime_car_theft = relevel(as.factor(yes_no_transformation(h21_4_1, na_as_no = TRUE)), "No"),
    crime_motorcycle_theft = relevel(as.factor(yes_no_transformation(h21_5, na_as_no = TRUE)), "No"),
    crime_other = relevel(as.factor(yes_no_transformation(h21_6, na_as_no = TRUE)), "No"),
    leaky_roof = relevel(as.factor(yes_no_transformation(h12_1)), "No"),
    poor_lighting = relevel(as.factor(yes_no_transformation(h12_2)), "No"),
    flooding = relevel(as.factor(yes_no_transformation(h12_3)), "No"),
    indoor_noise = relevel(as.factor(yes_no_transformation(h12_4)), "No"),
    outdoor_noise = relevel(as.factor(yes_no_transformation(h12_5)), "No"),
    power_outages = relevel(as.factor(yes_no_transformation(h12_6)), "No"),
    heating_problem = relevel(as.factor(yes_no_transformation(h12_8)), "No"),
    concern_clothing = relevel(as.factor(comparison_concern_transformation(b51_1)), "Not Important"),
    concern_family_lifestyle = relevel(as.factor(comparison_concern_transformation(b51_2)), "Not Important"),
    concern_personal_belongings = relevel(as.factor(comparison_concern_transformation(b51_3)), "Not Important"),
    concern_friend_circle = relevel(as.factor(comparison_concern_transformation(b51_4)), "Not Important"),
    concern_childrens_success = relevel(as.factor(comparison_concern_transformation(b51_5)), "Not Important"),
    concern_curfew_hours = relevel(as.factor(comparison_concern_transformation(b51_6)), "Not Important"),
    concern_occupation = relevel(as.factor(comparison_concern_transformation(b51_7)), "Not Important"),
    concern_income_level = relevel(as.factor(comparison_concern_transformation(b51_8)), "Not Important"),
    concern_religious_belief = relevel(as.factor(comparison_concern_transformation(b51_9)), "Not Important"),
    concern_political_view = relevel(as.factor(comparison_concern_transformation(b51_10)), "Not Important"),
    concern_education_level = relevel(as.factor(comparison_concern_transformation(b51_11)), "Not Important"),
    reputation_clothing = relevel(as.factor(comparison_concern_transformation(b52_1)), "Not Important"),
    reputation_family_lifestyle = relevel(as.factor(comparison_concern_transformation(b52_2)), "Not Important"),
    reputation_personal_belongings = relevel(as.factor(comparison_concern_transformation(b52_3)), "Not Important"),
    reputation_friend_circle = relevel(as.factor(comparison_concern_transformation(b52_4)), "Not Important"),
    reputation_childrens_success = relevel(as.factor(comparison_concern_transformation(b52_5)), "Not Important"),
    reputation_curfew_hours = relevel(as.factor(comparison_concern_transformation(b52_6)), "Not Important"),
    reputation_income_level = relevel(as.factor(comparison_concern_transformation(b52_8)), "Not Important"),
    reputation_religious_belief = relevel(as.factor(comparison_concern_transformation(b52_9)), "Not Important"),
    reputation_political_view = relevel(as.factor(comparison_concern_transformation(b52_10)), "Not Important"),
    reputation_education_level = relevel(as.factor(comparison_concern_transformation(b52_11)), "Not Important"),
    outlook_life_in_general = relevel(as.factor(future_outlook_transformation(b44_1)), "Same / No Opinion"),
    outlook_personal_job = relevel(as.factor(future_outlook_transformation(b44_2)), "Same / No Opinion"),
    outlook_household_finances = relevel(as.factor(future_outlook_transformation(b44_3)), "Same / No Opinion"),
    outlook_national_employment = relevel(as.factor(future_outlook_transformation(b44_4)), "Same / No Opinion"),
    outlook_national_economy = relevel(as.factor(future_outlook_transformation(b44_5)), "Same / No Opinion")
)]
panel_2014[, `:=`(
    has_positive_life_event = relevel(as.factor(ifelse(pmax(ifelse(got_married == "Yes", 1, 0), ifelse(got_a_job == "Yes", 1, 0), ifelse(opened_a_business == "Yes", 1, 0), ifelse(income_increased == "Yes", 1, 0), ifelse(started_saving == "Yes", 1, 0), ifelse(bought_a_car == "Yes", 1, 0), ifelse(bought_a_house == "Yes", 1, 0), ifelse(paid_off_debt == "Yes", 1, 0), ifelse(had_a_child == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    has_negative_life_event = relevel(as.factor(ifelse(pmax(ifelse(lost_job == "Yes", 1, 0), ifelse(went_bankrupt == "Yes", 1, 0), ifelse(income_decreased == "Yes", 1, 0), ifelse(savings_decreased == "Yes", 1, 0), ifelse(bought_cheaper_products == "Yes", 1, 0), ifelse(cut_vacation_spending == "Yes", 1, 0), ifelse(sold_car == "Yes", 1, 0), ifelse(sold_house == "Yes", 1, 0), ifelse(went_into_debt == "Yes", 1, 0), ifelse(got_divorced == "Yes", 1, 0), ifelse(had_a_bereavement == "Yes", 1, 0), ifelse(had_a_health_problem == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    faced_social_pressure = relevel(as.factor(ifelse(pmax(ifelse(pressure_gender == "Felt Pressure", 1, 0), ifelse(pressure_marital_status == "Felt Pressure", 1, 0), ifelse(pressure_age == "Felt Pressure", 1, 0), ifelse(pressure_tradition == "Felt Pressure", 1, 0), ifelse(pressure_religious_belief == "Felt Pressure", 1, 0), ifelse(pressure_political_view == "Felt Pressure", 1, 0), ifelse(pressure_hometown == "Felt Pressure", 1, 0), ifelse(pressure_clothing == "Felt Pressure", 1, 0), ifelse(pressure_income_level == "Felt Pressure", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    experienced_crime = relevel(as.factor(ifelse(pmax(ifelse(crime_bag_snatching == "Yes", 1, 0), ifelse(crime_robbery == "Yes", 1, 0), ifelse(crime_physical_assault == "Yes", 1, 0), ifelse(crime_mistreatment == "Yes", 1, 0), ifelse(crime_threat == "Yes", 1, 0), ifelse(crime_victimization_other == "Yes", 1, 0), ifelse(crime_fraud == "Yes", 1, 0), ifelse(crime_witnessed_someone_elses_crime == "Yes", 1, 0), ifelse(crime_burglary_general == "Yes", 1, 0), ifelse(crime_home_burglary == "Yes", 1, 0), ifelse(crime_workplace_burglary == "Yes", 1, 0), ifelse(crime_farmland_theft == "Yes", 1, 0), ifelse(crime_car_theft == "Yes", 1, 0), ifelse(crime_motorcycle_theft == "Yes", 1, 0), ifelse(crime_other == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    is_faced_with_a_housing_issue = relevel(as.factor(ifelse(pmax(ifelse(leaky_roof == "Yes", 1, 0), ifelse(poor_lighting == "Yes", 1, 0), ifelse(flooding == "Yes", 1, 0), ifelse(indoor_noise == "Yes", 1, 0), ifelse(outdoor_noise == "Yes", 1, 0), ifelse(power_outages == "Yes", 1, 0), ifelse(heating_problem == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    has_comparison_concern = relevel(as.factor(ifelse(pmax(ifelse(concern_clothing == "Important", 1, 0), ifelse(concern_family_lifestyle == "Important", 1, 0), ifelse(concern_personal_belongings == "Important", 1, 0), ifelse(concern_friend_circle == "Important", 1, 0), ifelse(concern_childrens_success == "Important", 1, 0), ifelse(concern_curfew_hours == "Important", 1, 0), ifelse(concern_occupation == "Important", 1, 0), ifelse(concern_income_level == "Important", 1, 0), ifelse(concern_religious_belief == "Important", 1, 0), ifelse(concern_political_view == "Important", 1, 0), ifelse(concern_education_level == "Important", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    has_reputation_concern = relevel(as.factor(ifelse(pmax(ifelse(reputation_clothing == "Important", 1, 0), ifelse(reputation_family_lifestyle == "Important", 1, 0), ifelse(reputation_personal_belongings == "Important", 1, 0), ifelse(reputation_friend_circle == "Important", 1, 0), ifelse(reputation_childrens_success == "Important", 1, 0), ifelse(reputation_curfew_hours == "Important", 1, 0), ifelse(reputation_income_level == "Important", 1, 0), ifelse(reputation_religious_belief == "Important", 1, 0), ifelse(reputation_political_view == "Important", 1, 0), ifelse(reputation_education_level == "Important", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    expects_personal_outlook_to_worsen = relevel(as.factor(ifelse(pmax(ifelse(outlook_life_in_general == "Expects Worse", 1, 0), ifelse(outlook_personal_job == "Expects Worse", 1, 0), ifelse(outlook_household_finances == "Expects Worse", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    expects_national_outlook_to_worsen = relevel(as.factor(ifelse(pmax(ifelse(outlook_national_employment == "Expects Worse", 1, 0), ifelse(outlook_national_economy == "Expects Worse", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No")
)]

## 2015 ----
data_2015 <- fread("data/data_2015.csv")
panel_2015 <- data_2015[, .(
    survey_weight = as.numeric(FAKTOR_FERT),
    happiness_ordered = scale_transformation(MUTLULUK),
    happiness_binary = happiness_transformation(MUTLULUK),
    gender = relevel(as.factor(gender(CINSIYET)), "Female"),
    age = as.numeric(BITIRILEN_YAS),
    age_squared = as.numeric(BITIRILEN_YAS)^2,
    household_size = as.numeric(HH_BUYUKLUK),
    marital_status_satisfaction = relevel(as.factor(marriage_satisfaction(marital_status(MEDENI_DURUM), scale_transformation(OLCEK_MEMNUNIYET_EVLILIK))), "Single"),
    education_level = relevel(as.factor(education_level(OKUL_BITEN, scheme = "2014_2016")), "No Schooling"),
    education_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_EGITIM, "Education")), "Neutral"),
    employment_job_satisfaction = relevel(as.factor(job_satisfaction(employment(CALISMA_DURUM, CALISMAMA_NEDEN, TRUE), scale_transformation(OLCEK_MEMNUNIYET_IS))), "Out of Labour Force"),
    materialism = relevel(as.factor(materialism(NE_MUTLU)), "Not Materialistic"),
    wellbeing_ladder = relevel(as.factor(paste0("Wellbeing Ladder: ", wellbeing_ladder_transformation(UMUT_BASAMAK, legacy_coding = TRUE))), "Wellbeing Ladder: 0"),
    hope_level = relevel(as.factor(hope_transformation(UMUT)), "Not Hopeful At All"),
    household_income_tier = relevel(as.factor(household_income_transformation(GELIR_GRUP)), "Household Income Tier 1"),
    income_sufficiency = relevel(as.factor(likert_categoric(OLCEK_GELIR_KARSILAMA, "Income Sufficiency")), "Neutral"),
    income_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_GELIR, "Income")), "Neutral"),
    health_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_SAGLIK, "Subjective Health")), "Neutral"),
    housing_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_KONUT, "Housing")), "Neutral"),
    housing_tenure = relevel(as.factor(housing_tenure_transformation(MULKIYET)), "Owner"),
    neighbourhood_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_SEMT, "Neighbourhood")), "Neutral"),
    friends_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_ARKADAS, "Friends")), "Neutral"),
    extended_family_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_AKRABA, "Extended Family")), "Neutral"),
    neighbours_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_KOMSU, "Neighbours")), "Neutral"),
    social_life_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_SOS_HAYAT, "Social Life")), "Neutral"),
    leisure_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_KISISEL_BKM, "Leisure")), "Neutral"),
    safety_at_home = relevel(as.factor(likert_categoric(GUVEN_EV, "Safety at Home")), "Neutral"),
    safety_around_home = relevel(as.factor(likert_categoric(GUVEN_CEVRE, "Safety Around Home")), "Neutral"),
    comparison_to_5y_ago = relevel(as.factor(comparison_5y_ago(GECMIS_KARSILASTIRMA)), "No Idea"),
    expectation_5y_later = relevel(as.factor(expectations_5y_later(GELECEK_KARSILASTIRMA)), "No Idea"),
    migration_experience = relevel(as.factor(ifelse(GOC_ETTI == 1, "Migrated", "Not Migrated")), "Not Migrated"),
    religiosity = relevel(as.factor(religiosity_transformation(DIN)), "Not Religious"),
    got_married = relevel(as.factor(yes_no_transformation(EVLENDI, na_as_no = TRUE)), "No"),
    got_a_job = relevel(as.factor(yes_no_transformation(ISE_GIRDI, na_as_no = TRUE)), "No"),
    opened_a_business = relevel(as.factor(yes_no_transformation(ISYERI_ACTI, na_as_no = TRUE)), "No"),
    income_increased = relevel(as.factor(yes_no_transformation(GELIRI_ARTTI, na_as_no = TRUE)), "No"),
    started_saving = relevel(as.factor(yes_no_transformation(TASARRUF_YAPTI)), "No"),
    bought_a_car = relevel(as.factor(yes_no_transformation(ARABA_ALDI)), "No"),
    bought_a_house = relevel(as.factor(yes_no_transformation(EV_ALDI)), "No"),
    paid_off_debt = relevel(as.factor(yes_no_transformation(BORC_ODEDI, na_as_no = TRUE)), "No"),
    had_a_child = relevel(as.factor(yes_no_transformation(COCUK_OLDU)), "No"),
    lost_job = relevel(as.factor(yes_no_transformation(ISINI_KAYBETTI, na_as_no = TRUE)), "No"),
    went_bankrupt = relevel(as.factor(yes_no_transformation(IFLAS_ETTI, na_as_no = TRUE)), "No"),
    income_decreased = relevel(as.factor(yes_no_transformation(GELIRI_AZALDI, na_as_no = TRUE)), "No"),
    savings_decreased = relevel(as.factor(yes_no_transformation(TASARRUFLARI_AZALDI, na_as_no = TRUE)), "No"),
    bought_cheaper_products = relevel(as.factor(yes_no_transformation(UCUZ_URUN)), "No"),
    cut_vacation_spending = relevel(as.factor(yes_no_transformation(TATIL_KISTI, na_as_no = TRUE)), "No"),
    sold_car = relevel(as.factor(yes_no_transformation(ARABA_SATTI, na_as_no = TRUE)), "No"),
    sold_house = relevel(as.factor(yes_no_transformation(EV_SATTI, na_as_no = TRUE)), "No"),
    went_into_debt = relevel(as.factor(yes_no_transformation(BORCLANDI)), "No"),
    got_divorced = relevel(as.factor(yes_no_transformation(BOSANDI, na_as_no = TRUE)), "No"),
    had_a_bereavement = relevel(as.factor(yes_no_transformation(VEFAT)), "No"),
    had_a_health_problem = relevel(as.factor(yes_no_transformation(SAGLIK_PROBLEM)), "No"),
    pressure_gender = relevel(as.factor(social_pressure_transformation(CINSIYET_BASKI)), "No Pressure"),
    pressure_marital_status = relevel(as.factor(social_pressure_transformation(MEDENI_DURUM_BASKI)), "No Pressure"),
    pressure_age = relevel(as.factor(social_pressure_transformation(YAS_BASKI)), "No Pressure"),
    pressure_tradition = relevel(as.factor(social_pressure_transformation(GELENEK_GORENEK)), "No Pressure"),
    pressure_religious_belief = relevel(as.factor(social_pressure_transformation(DINI_INANCDAN)), "No Pressure"),
    pressure_political_view = relevel(as.factor(social_pressure_transformation(SIYASI_GORUSTEN)), "No Pressure"),
    pressure_hometown = relevel(as.factor(social_pressure_transformation(MEMLEKETINDEN)), "No Pressure"),
    pressure_clothing = relevel(as.factor(social_pressure_transformation(KILIK_KIYAFET)), "No Pressure"),
    pressure_income_level = relevel(as.factor(social_pressure_transformation(GELIR_DUZEY_DURUM)), "No Pressure"),
    crime_bag_snatching = relevel(as.factor(yes_no_transformation(KAPKAC)), "No"),
    crime_robbery = relevel(as.factor(yes_no_transformation(GASP)), "No"),
    crime_physical_assault = relevel(as.factor(yes_no_transformation(DARP)), "No"),
    crime_mistreatment = relevel(as.factor(yes_no_transformation(FERT_KOTU)), "No"),
    crime_threat = relevel(as.factor(yes_no_transformation(TEHDIT)), "No"),
    crime_victimization_other = relevel(as.factor(yes_no_transformation(MAGDURIYET)), "No"),
    crime_fraud = relevel(as.factor(yes_no_transformation(DOLANDIR)), "No"),
    crime_witnessed_someone_elses_crime = relevel(as.factor(yes_no_transformation(BASKASUC)), "No"),
    crime_burglary_general = relevel(as.factor(yes_no_transformation(HIRSIZLIK)), "No"),
    crime_home_burglary = relevel(as.factor(yes_no_transformation(EV_HIRS, na_as_no = TRUE)), "No"),
    crime_workplace_burglary = relevel(as.factor(yes_no_transformation(IS_HIRS, na_as_no = TRUE)), "No"),
    crime_farmland_theft = relevel(as.factor(yes_no_transformation(TARLA_HIRS, na_as_no = TRUE)), "No"),
    crime_car_theft = relevel(as.factor(yes_no_transformation(OTO_HIRS, na_as_no = TRUE)), "No"),
    crime_motorcycle_theft = relevel(as.factor(yes_no_transformation(MOTOR_HIRS, na_as_no = TRUE)), "No"),
    crime_other = relevel(as.factor(yes_no_transformation(SUC, na_as_no = TRUE)), "No"),
    leaky_roof = relevel(as.factor(yes_no_transformation(CATI)), "No"),
    poor_lighting = relevel(as.factor(yes_no_transformation(KARANLIK)), "No"),
    flooding = relevel(as.factor(yes_no_transformation(SU_BASKINI)), "No"),
    indoor_noise = relevel(as.factor(yes_no_transformation(K_GURULTU)), "No"),
    outdoor_noise = relevel(as.factor(yes_no_transformation(S_GURULTU)), "No"),
    power_outages = relevel(as.factor(yes_no_transformation(ELEK_KESINTI)), "No"),
    heating_problem = relevel(as.factor(yes_no_transformation(ISINMA_PROBLEM)), "No"),
    concern_clothing = relevel(as.factor(comparison_concern_transformation(KIYAFET_ONEM)), "Not Important"),
    concern_family_lifestyle = relevel(as.factor(comparison_concern_transformation(AILE_YASAM_ONEM)), "Not Important"),
    concern_personal_belongings = relevel(as.factor(comparison_concern_transformation(KISISEL_ESYA_ONEM)), "Not Important"),
    concern_friend_circle = relevel(as.factor(comparison_concern_transformation(ARKADAS_CEVRE_ONEM)), "Not Important"),
    concern_childrens_success = relevel(as.factor(comparison_concern_transformation(COCUK_BASARI_ONEM)), "Not Important"),
    concern_curfew_hours = relevel(as.factor(comparison_concern_transformation(EVE_GIRIS_SAAT_ONEM)), "Not Important"),
    concern_occupation = relevel(as.factor(comparison_concern_transformation(YAPTIKLARI_IS_ONEM)), "Not Important"),
    concern_income_level = relevel(as.factor(comparison_concern_transformation(GELIR_DUZEYLERI_ONEM)), "Not Important"),
    concern_religious_belief = relevel(as.factor(comparison_concern_transformation(DINI_INANC_ONEM)), "Not Important"),
    concern_political_view = relevel(as.factor(comparison_concern_transformation(SIYASI_DUSUNCE_ONEM)), "Not Important"),
    concern_education_level = relevel(as.factor(comparison_concern_transformation(EGITIM_DUZEYI_ONEM)), "Not Important"),
    reputation_clothing = relevel(as.factor(comparison_concern_transformation(KIYAFETINIZ_ONEM)), "Not Important"),
    reputation_family_lifestyle = relevel(as.factor(comparison_concern_transformation(AILE_YASAM_BICIMI_ONEM)), "Not Important"),
    reputation_personal_belongings = relevel(as.factor(comparison_concern_transformation(KISISEL_ESYANIZ_ONEM)), "Not Important"),
    reputation_friend_circle = relevel(as.factor(comparison_concern_transformation(ARKADAS_CEVRENIZ_ONEM)), "Not Important"),
    reputation_childrens_success = relevel(as.factor(comparison_concern_transformation(COCUGUN_BASARISI_ONEM)), "Not Important"),
    reputation_curfew_hours = relevel(as.factor(comparison_concern_transformation(EVE_GIRISCIKIC_SAAT_ONEM)), "Not Important"),
    reputation_income_level = relevel(as.factor(comparison_concern_transformation(GELIR_DUZEY_ONEM)), "Not Important"),
    reputation_religious_belief = relevel(as.factor(comparison_concern_transformation(DINI_INANCI_ONEM)), "Not Important"),
    reputation_political_view = relevel(as.factor(comparison_concern_transformation(SIYASI_DUSUNCENIZ_ONEM)), "Not Important"),
    reputation_education_level = relevel(as.factor(comparison_concern_transformation(EGITIM_DURUMU_ONEM)), "Not Important"),
    outlook_life_in_general = relevel(as.factor(future_outlook_transformation(GELECEK_HAYAT)), "Same / No Opinion"),
    outlook_personal_job = relevel(as.factor(future_outlook_transformation(GELECEK_KISISEL)), "Same / No Opinion"),
    outlook_household_finances = relevel(as.factor(future_outlook_transformation(GELECEK_MALI)), "Same / No Opinion"),
    outlook_national_employment = relevel(as.factor(future_outlook_transformation(GELECEK_IS)), "Same / No Opinion"),
    outlook_national_economy = relevel(as.factor(future_outlook_transformation(GELECEK_EKONOMI)), "Same / No Opinion")
)]
panel_2015[, `:=`(
    has_positive_life_event = relevel(as.factor(ifelse(pmax(ifelse(got_married == "Yes", 1, 0), ifelse(got_a_job == "Yes", 1, 0), ifelse(opened_a_business == "Yes", 1, 0), ifelse(income_increased == "Yes", 1, 0), ifelse(started_saving == "Yes", 1, 0), ifelse(bought_a_car == "Yes", 1, 0), ifelse(bought_a_house == "Yes", 1, 0), ifelse(paid_off_debt == "Yes", 1, 0), ifelse(had_a_child == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    has_negative_life_event = relevel(as.factor(ifelse(pmax(ifelse(lost_job == "Yes", 1, 0), ifelse(went_bankrupt == "Yes", 1, 0), ifelse(income_decreased == "Yes", 1, 0), ifelse(savings_decreased == "Yes", 1, 0), ifelse(bought_cheaper_products == "Yes", 1, 0), ifelse(cut_vacation_spending == "Yes", 1, 0), ifelse(sold_car == "Yes", 1, 0), ifelse(sold_house == "Yes", 1, 0), ifelse(went_into_debt == "Yes", 1, 0), ifelse(got_divorced == "Yes", 1, 0), ifelse(had_a_bereavement == "Yes", 1, 0), ifelse(had_a_health_problem == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    faced_social_pressure = relevel(as.factor(ifelse(pmax(ifelse(pressure_gender == "Felt Pressure", 1, 0), ifelse(pressure_marital_status == "Felt Pressure", 1, 0), ifelse(pressure_age == "Felt Pressure", 1, 0), ifelse(pressure_tradition == "Felt Pressure", 1, 0), ifelse(pressure_religious_belief == "Felt Pressure", 1, 0), ifelse(pressure_political_view == "Felt Pressure", 1, 0), ifelse(pressure_hometown == "Felt Pressure", 1, 0), ifelse(pressure_clothing == "Felt Pressure", 1, 0), ifelse(pressure_income_level == "Felt Pressure", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    experienced_crime = relevel(as.factor(ifelse(pmax(ifelse(crime_bag_snatching == "Yes", 1, 0), ifelse(crime_robbery == "Yes", 1, 0), ifelse(crime_physical_assault == "Yes", 1, 0), ifelse(crime_mistreatment == "Yes", 1, 0), ifelse(crime_threat == "Yes", 1, 0), ifelse(crime_victimization_other == "Yes", 1, 0), ifelse(crime_fraud == "Yes", 1, 0), ifelse(crime_witnessed_someone_elses_crime == "Yes", 1, 0), ifelse(crime_burglary_general == "Yes", 1, 0), ifelse(crime_home_burglary == "Yes", 1, 0), ifelse(crime_workplace_burglary == "Yes", 1, 0), ifelse(crime_farmland_theft == "Yes", 1, 0), ifelse(crime_car_theft == "Yes", 1, 0), ifelse(crime_motorcycle_theft == "Yes", 1, 0), ifelse(crime_other == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    is_faced_with_a_housing_issue = relevel(as.factor(ifelse(pmax(ifelse(leaky_roof == "Yes", 1, 0), ifelse(poor_lighting == "Yes", 1, 0), ifelse(flooding == "Yes", 1, 0), ifelse(indoor_noise == "Yes", 1, 0), ifelse(outdoor_noise == "Yes", 1, 0), ifelse(power_outages == "Yes", 1, 0), ifelse(heating_problem == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    has_comparison_concern = relevel(as.factor(ifelse(pmax(ifelse(concern_clothing == "Important", 1, 0), ifelse(concern_family_lifestyle == "Important", 1, 0), ifelse(concern_personal_belongings == "Important", 1, 0), ifelse(concern_friend_circle == "Important", 1, 0), ifelse(concern_childrens_success == "Important", 1, 0), ifelse(concern_curfew_hours == "Important", 1, 0), ifelse(concern_occupation == "Important", 1, 0), ifelse(concern_income_level == "Important", 1, 0), ifelse(concern_religious_belief == "Important", 1, 0), ifelse(concern_political_view == "Important", 1, 0), ifelse(concern_education_level == "Important", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    has_reputation_concern = relevel(as.factor(ifelse(pmax(ifelse(reputation_clothing == "Important", 1, 0), ifelse(reputation_family_lifestyle == "Important", 1, 0), ifelse(reputation_personal_belongings == "Important", 1, 0), ifelse(reputation_friend_circle == "Important", 1, 0), ifelse(reputation_childrens_success == "Important", 1, 0), ifelse(reputation_curfew_hours == "Important", 1, 0), ifelse(reputation_income_level == "Important", 1, 0), ifelse(reputation_religious_belief == "Important", 1, 0), ifelse(reputation_political_view == "Important", 1, 0), ifelse(reputation_education_level == "Important", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    expects_personal_outlook_to_worsen = relevel(as.factor(ifelse(pmax(ifelse(outlook_life_in_general == "Expects Worse", 1, 0), ifelse(outlook_personal_job == "Expects Worse", 1, 0), ifelse(outlook_household_finances == "Expects Worse", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    expects_national_outlook_to_worsen = relevel(as.factor(ifelse(pmax(ifelse(outlook_national_employment == "Expects Worse", 1, 0), ifelse(outlook_national_economy == "Expects Worse", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No")
)]

## 2016 ----
data_2016 <- fread("data/data_2016.csv")
panel_2016 <- data_2016[, .(
    survey_weight = as.numeric(FAKTOR_FERT),
    happiness_ordered = scale_transformation(MUTLULUK),
    happiness_binary = happiness_transformation(MUTLULUK),
    gender = relevel(as.factor(gender(CINSIYET)), "Female"),
    age = as.numeric(BITIRILEN_YAS),
    age_squared = as.numeric(BITIRILEN_YAS)^2,
    household_size = as.numeric(HH_BUYUKLUK),
    marital_status_satisfaction = relevel(as.factor(marriage_satisfaction(marital_status(MEDENI_DURUM), scale_transformation(OLCEK_MEMNUNIYET_EVLILIK))), "Single"),
    education_level = relevel(as.factor(education_level(OKUL_BITEN, scheme = "2014_2016")), "No Schooling"),
    education_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_EGITIM, "Education")), "Neutral"),
    employment_job_satisfaction = relevel(as.factor(job_satisfaction(employment(CALISMA_DURUM, CALISMAMA_NEDEN, TRUE), scale_transformation(OLCEK_MEMNUNIYET_IS))), "Out of Labour Force"),
    materialism = relevel(as.factor(materialism(NE_MUTLU)), "Not Materialistic"),
    wellbeing_ladder = relevel(as.factor(paste0("Wellbeing Ladder: ", wellbeing_ladder_transformation(UMUT_BASAMAK, legacy_coding = TRUE))), "Wellbeing Ladder: 0"),
    hope_level = relevel(as.factor(hope_transformation(UMUT)), "Not Hopeful At All"),
    household_income_tier = relevel(as.factor(household_income_transformation(GELIR_GRUP)), "Household Income Tier 1"),
    income_sufficiency = relevel(as.factor(likert_categoric(OLCEK_GELIR_KARSILAMA, "Income Sufficiency")), "Neutral"),
    income_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_GELIR, "Income")), "Neutral"),
    health_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_SAGLIK, "Subjective Health")), "Neutral"),
    housing_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_KONUT, "Housing")), "Neutral"),
    housing_tenure = relevel(as.factor(housing_tenure_transformation(MULKIYET)), "Owner"),
    neighbourhood_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_SEMT, "Neighbourhood")), "Neutral"),
    friends_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_ARKADAS, "Friends")), "Neutral"),
    extended_family_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_AKRABA, "Extended Family")), "Neutral"),
    neighbours_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_KOMSU, "Neighbours")), "Neutral"),
    social_life_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_SOS_HAYAT, "Social Life")), "Neutral"),
    leisure_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_KISISEL_BKM, "Leisure")), "Neutral"),
    safety_at_home = relevel(as.factor(likert_categoric(GUVEN_EV, "Safety at Home")), "Neutral"),
    safety_around_home = relevel(as.factor(likert_categoric(GUVEN_CEVRE, "Safety Around Home")), "Neutral"),
    comparison_to_5y_ago = relevel(as.factor(comparison_5y_ago(GECMIS_KARSILASTIRMA)), "No Idea"),
    expectation_5y_later = relevel(as.factor(expectations_5y_later(GELECEK_KARSILASTIRMA)), "No Idea"),
    migration_experience = relevel(as.factor(ifelse(GOC_ETTI == 1, "Migrated", "Not Migrated")), "Not Migrated"),
    religiosity = relevel(as.factor(religiosity_transformation(DIN)), "Not Religious"),
    got_married = relevel(as.factor(yes_no_transformation(EVLENDI, na_as_no = TRUE)), "No"),
    got_a_job = relevel(as.factor(yes_no_transformation(ISE_GIRDI, na_as_no = TRUE)), "No"),
    opened_a_business = relevel(as.factor(yes_no_transformation(ISYERI_ACTI, na_as_no = TRUE)), "No"),
    income_increased = relevel(as.factor(yes_no_transformation(GELIRI_ARTTI, na_as_no = TRUE)), "No"),
    started_saving = relevel(as.factor(yes_no_transformation(TASARRUF_YAPTI)), "No"),
    bought_a_car = relevel(as.factor(yes_no_transformation(ARABA_ALDI)), "No"),
    bought_a_house = relevel(as.factor(yes_no_transformation(EV_ALDI)), "No"),
    paid_off_debt = relevel(as.factor(yes_no_transformation(BORC_ODEDI, na_as_no = TRUE)), "No"),
    had_a_child = relevel(as.factor(yes_no_transformation(COCUK_OLDU)), "No"),
    lost_job = relevel(as.factor(yes_no_transformation(ISINI_KAYBETTI, na_as_no = TRUE)), "No"),
    went_bankrupt = relevel(as.factor(yes_no_transformation(IFLAS_ETTI, na_as_no = TRUE)), "No"),
    income_decreased = relevel(as.factor(yes_no_transformation(GELIRI_AZALDI, na_as_no = TRUE)), "No"),
    savings_decreased = relevel(as.factor(yes_no_transformation(TASARRUFLARI_AZALDI, na_as_no = TRUE)), "No"),
    bought_cheaper_products = relevel(as.factor(yes_no_transformation(UCUZ_URUN)), "No"),
    cut_vacation_spending = relevel(as.factor(yes_no_transformation(TATIL_KISTI, na_as_no = TRUE)), "No"),
    sold_car = relevel(as.factor(yes_no_transformation(ARABA_SATTI, na_as_no = TRUE)), "No"),
    sold_house = relevel(as.factor(yes_no_transformation(EV_SATTI, na_as_no = TRUE)), "No"),
    went_into_debt = relevel(as.factor(yes_no_transformation(BORCLANDI)), "No"),
    got_divorced = relevel(as.factor(yes_no_transformation(BOSANDI, na_as_no = TRUE)), "No"),
    had_a_bereavement = relevel(as.factor(yes_no_transformation(VEFAT)), "No"),
    had_a_health_problem = relevel(as.factor(yes_no_transformation(SAGLIK_PROBLEM)), "No"),
    pressure_gender = relevel(as.factor(social_pressure_transformation(CINSIYET_BASKI)), "No Pressure"),
    pressure_marital_status = relevel(as.factor(social_pressure_transformation(MEDENI_DURUM_BASKI)), "No Pressure"),
    pressure_age = relevel(as.factor(social_pressure_transformation(YAS_BASKI)), "No Pressure"),
    pressure_tradition = relevel(as.factor(social_pressure_transformation(GELENEK_GORENEK)), "No Pressure"),
    pressure_religious_belief = relevel(as.factor(social_pressure_transformation(DINI_INANCDAN)), "No Pressure"),
    pressure_political_view = relevel(as.factor(social_pressure_transformation(SIYASI_GORUSTEN)), "No Pressure"),
    pressure_hometown = relevel(as.factor(social_pressure_transformation(MEMLEKETINDEN)), "No Pressure"),
    pressure_clothing = relevel(as.factor(social_pressure_transformation(KILIK_KIYAFET)), "No Pressure"),
    pressure_income_level = relevel(as.factor(social_pressure_transformation(GELIR_DUZEY_DURUM)), "No Pressure"),
    crime_bag_snatching = relevel(as.factor(yes_no_transformation(KAPKAC)), "No"),
    crime_robbery = relevel(as.factor(yes_no_transformation(GASP)), "No"),
    crime_physical_assault = relevel(as.factor(yes_no_transformation(DARP)), "No"),
    crime_mistreatment = relevel(as.factor(yes_no_transformation(FERT_KOTU)), "No"),
    crime_threat = relevel(as.factor(yes_no_transformation(TEHDIT)), "No"),
    crime_victimization_other = relevel(as.factor(yes_no_transformation(MAGDURIYET)), "No"),
    crime_fraud = relevel(as.factor(yes_no_transformation(DOLANDIR)), "No"),
    crime_witnessed_someone_elses_crime = relevel(as.factor(yes_no_transformation(BASKASUC)), "No"),
    crime_burglary_general = relevel(as.factor(yes_no_transformation(HIRSIZLIK)), "No"),
    crime_home_burglary = relevel(as.factor(yes_no_transformation(EV_HIRS, na_as_no = TRUE)), "No"),
    crime_workplace_burglary = relevel(as.factor(yes_no_transformation(IS_HIRS, na_as_no = TRUE)), "No"),
    crime_farmland_theft = relevel(as.factor(yes_no_transformation(TARLA_HIRS, na_as_no = TRUE)), "No"),
    crime_car_theft = relevel(as.factor(yes_no_transformation(OTO_HIRS, na_as_no = TRUE)), "No"),
    crime_motorcycle_theft = relevel(as.factor(yes_no_transformation(MOTOR_HIRS, na_as_no = TRUE)), "No"),
    crime_other = relevel(as.factor(yes_no_transformation(SUC, na_as_no = TRUE)), "No"),
    leaky_roof = relevel(as.factor(yes_no_transformation(CATI)), "No"),
    poor_lighting = relevel(as.factor(yes_no_transformation(KARANLIK)), "No"),
    flooding = relevel(as.factor(yes_no_transformation(SU_BASKINI)), "No"),
    indoor_noise = relevel(as.factor(yes_no_transformation(K_GURULTU)), "No"),
    outdoor_noise = relevel(as.factor(yes_no_transformation(S_GURULTU)), "No"),
    power_outages = relevel(as.factor(yes_no_transformation(ELEK_KESINTI)), "No"),
    heating_problem = relevel(as.factor(yes_no_transformation(ISINMA_PROBLEM)), "No"),
    concern_clothing = relevel(as.factor(comparison_concern_transformation(KIYAFET_ONEM)), "Not Important"),
    concern_family_lifestyle = relevel(as.factor(comparison_concern_transformation(AILE_YASAM_ONEM)), "Not Important"),
    concern_personal_belongings = relevel(as.factor(comparison_concern_transformation(KISISEL_ESYA_ONEM)), "Not Important"),
    concern_friend_circle = relevel(as.factor(comparison_concern_transformation(ARKADAS_CEVRE_ONEM)), "Not Important"),
    concern_childrens_success = relevel(as.factor(comparison_concern_transformation(COCUK_BASARI_ONEM)), "Not Important"),
    concern_curfew_hours = relevel(as.factor(comparison_concern_transformation(EVE_GIRIS_SAAT_ONEM)), "Not Important"),
    concern_occupation = relevel(as.factor(comparison_concern_transformation(YAPTIKLARI_IS_ONEM)), "Not Important"),
    concern_income_level = relevel(as.factor(comparison_concern_transformation(GELIR_DUZEYLERI_ONEM)), "Not Important"),
    concern_religious_belief = relevel(as.factor(comparison_concern_transformation(DINI_INANC_ONEM)), "Not Important"),
    concern_political_view = relevel(as.factor(comparison_concern_transformation(SIYASI_DUSUNCE_ONEM)), "Not Important"),
    concern_education_level = relevel(as.factor(comparison_concern_transformation(EGITIM_DUZEYI_ONEM)), "Not Important"),
    reputation_clothing = relevel(as.factor(comparison_concern_transformation(KIYAFETINIZ_ONEM)), "Not Important"),
    reputation_family_lifestyle = relevel(as.factor(comparison_concern_transformation(AILE_YASAM_BICIMI_ONEM)), "Not Important"),
    reputation_personal_belongings = relevel(as.factor(comparison_concern_transformation(KISISEL_ESYANIZ_ONEM)), "Not Important"),
    reputation_friend_circle = relevel(as.factor(comparison_concern_transformation(ARKADAS_CEVRENIZ_ONEM)), "Not Important"),
    reputation_childrens_success = relevel(as.factor(comparison_concern_transformation(COCUGUN_BASARISI_ONEM)), "Not Important"),
    reputation_curfew_hours = relevel(as.factor(comparison_concern_transformation(EVE_GIRISCIKIC_SAAT_ONEM)), "Not Important"),
    reputation_income_level = relevel(as.factor(comparison_concern_transformation(GELIR_DUZEY_ONEM)), "Not Important"),
    reputation_religious_belief = relevel(as.factor(comparison_concern_transformation(DINI_INANCI_ONEM)), "Not Important"),
    reputation_political_view = relevel(as.factor(comparison_concern_transformation(SIYASI_DUSUNCENIZ_ONEM)), "Not Important"),
    reputation_education_level = relevel(as.factor(comparison_concern_transformation(EGITIM_DURUMU_ONEM)), "Not Important"),
    outlook_life_in_general = relevel(as.factor(future_outlook_transformation(GELECEK_HAYAT)), "Same / No Opinion"),
    outlook_personal_job = relevel(as.factor(future_outlook_transformation(GELECEK_KISISEL)), "Same / No Opinion"),
    outlook_household_finances = relevel(as.factor(future_outlook_transformation(GELECEK_MALI)), "Same / No Opinion"),
    outlook_national_employment = relevel(as.factor(future_outlook_transformation(GELECEK_IS)), "Same / No Opinion"),
    outlook_national_economy = relevel(as.factor(future_outlook_transformation(GELECEK_EKONOMI)), "Same / No Opinion")
)]
panel_2016[, `:=`(
    has_positive_life_event = relevel(as.factor(ifelse(pmax(ifelse(got_married == "Yes", 1, 0), ifelse(got_a_job == "Yes", 1, 0), ifelse(opened_a_business == "Yes", 1, 0), ifelse(income_increased == "Yes", 1, 0), ifelse(started_saving == "Yes", 1, 0), ifelse(bought_a_car == "Yes", 1, 0), ifelse(bought_a_house == "Yes", 1, 0), ifelse(paid_off_debt == "Yes", 1, 0), ifelse(had_a_child == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    has_negative_life_event = relevel(as.factor(ifelse(pmax(ifelse(lost_job == "Yes", 1, 0), ifelse(went_bankrupt == "Yes", 1, 0), ifelse(income_decreased == "Yes", 1, 0), ifelse(savings_decreased == "Yes", 1, 0), ifelse(bought_cheaper_products == "Yes", 1, 0), ifelse(cut_vacation_spending == "Yes", 1, 0), ifelse(sold_car == "Yes", 1, 0), ifelse(sold_house == "Yes", 1, 0), ifelse(went_into_debt == "Yes", 1, 0), ifelse(got_divorced == "Yes", 1, 0), ifelse(had_a_bereavement == "Yes", 1, 0), ifelse(had_a_health_problem == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    faced_social_pressure = relevel(as.factor(ifelse(pmax(ifelse(pressure_gender == "Felt Pressure", 1, 0), ifelse(pressure_marital_status == "Felt Pressure", 1, 0), ifelse(pressure_age == "Felt Pressure", 1, 0), ifelse(pressure_tradition == "Felt Pressure", 1, 0), ifelse(pressure_religious_belief == "Felt Pressure", 1, 0), ifelse(pressure_political_view == "Felt Pressure", 1, 0), ifelse(pressure_hometown == "Felt Pressure", 1, 0), ifelse(pressure_clothing == "Felt Pressure", 1, 0), ifelse(pressure_income_level == "Felt Pressure", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    experienced_crime = relevel(as.factor(ifelse(pmax(ifelse(crime_bag_snatching == "Yes", 1, 0), ifelse(crime_robbery == "Yes", 1, 0), ifelse(crime_physical_assault == "Yes", 1, 0), ifelse(crime_mistreatment == "Yes", 1, 0), ifelse(crime_threat == "Yes", 1, 0), ifelse(crime_victimization_other == "Yes", 1, 0), ifelse(crime_fraud == "Yes", 1, 0), ifelse(crime_witnessed_someone_elses_crime == "Yes", 1, 0), ifelse(crime_burglary_general == "Yes", 1, 0), ifelse(crime_home_burglary == "Yes", 1, 0), ifelse(crime_workplace_burglary == "Yes", 1, 0), ifelse(crime_farmland_theft == "Yes", 1, 0), ifelse(crime_car_theft == "Yes", 1, 0), ifelse(crime_motorcycle_theft == "Yes", 1, 0), ifelse(crime_other == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    is_faced_with_a_housing_issue = relevel(as.factor(ifelse(pmax(ifelse(leaky_roof == "Yes", 1, 0), ifelse(poor_lighting == "Yes", 1, 0), ifelse(flooding == "Yes", 1, 0), ifelse(indoor_noise == "Yes", 1, 0), ifelse(outdoor_noise == "Yes", 1, 0), ifelse(power_outages == "Yes", 1, 0), ifelse(heating_problem == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    has_comparison_concern = relevel(as.factor(ifelse(pmax(ifelse(concern_clothing == "Important", 1, 0), ifelse(concern_family_lifestyle == "Important", 1, 0), ifelse(concern_personal_belongings == "Important", 1, 0), ifelse(concern_friend_circle == "Important", 1, 0), ifelse(concern_childrens_success == "Important", 1, 0), ifelse(concern_curfew_hours == "Important", 1, 0), ifelse(concern_occupation == "Important", 1, 0), ifelse(concern_income_level == "Important", 1, 0), ifelse(concern_religious_belief == "Important", 1, 0), ifelse(concern_political_view == "Important", 1, 0), ifelse(concern_education_level == "Important", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    has_reputation_concern = relevel(as.factor(ifelse(pmax(ifelse(reputation_clothing == "Important", 1, 0), ifelse(reputation_family_lifestyle == "Important", 1, 0), ifelse(reputation_personal_belongings == "Important", 1, 0), ifelse(reputation_friend_circle == "Important", 1, 0), ifelse(reputation_childrens_success == "Important", 1, 0), ifelse(reputation_curfew_hours == "Important", 1, 0), ifelse(reputation_income_level == "Important", 1, 0), ifelse(reputation_religious_belief == "Important", 1, 0), ifelse(reputation_political_view == "Important", 1, 0), ifelse(reputation_education_level == "Important", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    expects_personal_outlook_to_worsen = relevel(as.factor(ifelse(pmax(ifelse(outlook_life_in_general == "Expects Worse", 1, 0), ifelse(outlook_personal_job == "Expects Worse", 1, 0), ifelse(outlook_household_finances == "Expects Worse", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    expects_national_outlook_to_worsen = relevel(as.factor(ifelse(pmax(ifelse(outlook_national_employment == "Expects Worse", 1, 0), ifelse(outlook_national_economy == "Expects Worse", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No")
)]

## 2017 ----
data_2017 <- fread("data/data_2017.csv")
panel_2017 <- data_2017[, .(
    survey_weight = as.numeric(FAKTOR_FERT),
    happiness_ordered = scale_transformation(MUTLULUK),
    happiness_binary = happiness_transformation(MUTLULUK),
    gender = relevel(as.factor(gender(CINSIYET)), "Female"),
    age = as.numeric(BITIRILEN_YAS),
    age_squared = as.numeric(BITIRILEN_YAS)^2,
    household_size = as.numeric(HH_BUYUKLUK),
    marital_status_satisfaction = relevel(as.factor(marriage_satisfaction(marital_status(MEDENI_DURUM), scale_transformation(OLCEK_MEMNUNIYET_EVLILIK))), "Single"),
    education_level = relevel(as.factor(education_level(OKUL_BITEN, scheme = "2017")), "No Schooling"),
    education_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_EGITIM, "Education")), "Neutral"),
    employment_job_satisfaction = relevel(as.factor(job_satisfaction(employment(CALISMA_DURUM, CALISMAMA_NEDEN, TRUE), scale_transformation(OLCEK_MEMNUNIYET_IS))), "Out of Labour Force"),
    materialism = relevel(as.factor(materialism(NE_MUTLU)), "Not Materialistic"),
    wellbeing_ladder = relevel(as.factor(paste0("Wellbeing Ladder: ", wellbeing_ladder_transformation(UMUT_BASAMAK, legacy_coding = TRUE))), "Wellbeing Ladder: 0"),
    hope_level = relevel(as.factor(hope_transformation(UMUT)), "Not Hopeful At All"),
    household_income_tier = relevel(as.factor(household_income_transformation(GELIR_GRUP)), "Household Income Tier 1"),
    income_sufficiency = relevel(as.factor(likert_categoric(OLCEK_GELIR_KARSILAMA, "Income Sufficiency")), "Neutral"),
    income_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_GELIR, "Income")), "Neutral"),
    health_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_SAGLIK, "Subjective Health")), "Neutral"),
    housing_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_KONUT, "Housing")), "Neutral"),
    housing_tenure = relevel(as.factor(housing_tenure_transformation(MULKIYET)), "Owner"),
    neighbourhood_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_SEMT, "Neighbourhood")), "Neutral"),
    friends_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_ARKADAS, "Friends")), "Neutral"),
    extended_family_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_AKRABA, "Extended Family")), "Neutral"),
    neighbours_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_KOMSU, "Neighbours")), "Neutral"),
    social_life_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_SOS_HAYAT, "Social Life")), "Neutral"),
    leisure_satisfaction = relevel(as.factor(likert_categoric(OLCEK_MEMNUNIYET_KISISEL_BKM, "Leisure")), "Neutral"),
    safety_at_home = relevel(as.factor(likert_categoric(GUVEN_EV, "Safety at Home")), "Neutral"),
    safety_around_home = relevel(as.factor(likert_categoric(GUVEN_CEVRE, "Safety Around Home")), "Neutral"),
    comparison_to_5y_ago = relevel(as.factor(comparison_5y_ago(GECMIS_KARSILASTIRMA)), "No Idea"),
    expectation_5y_later = relevel(as.factor(expectations_5y_later(GELECEK_KARSILASTIRMA)), "No Idea"),
    migration_experience = relevel(as.factor(ifelse(GOC_ETTI == 1, "Migrated", "Not Migrated")), "Not Migrated"),
    religiosity = relevel(as.factor(religiosity_transformation(DIN)), "Not Religious"),
    got_married = relevel(as.factor(yes_no_transformation(EVLENDI, na_as_no = TRUE)), "No"),
    got_a_job = relevel(as.factor(yes_no_transformation(ISE_GIRDI, na_as_no = TRUE)), "No"),
    opened_a_business = relevel(as.factor(yes_no_transformation(ISYERI_ACTI, na_as_no = TRUE)), "No"),
    income_increased = relevel(as.factor(yes_no_transformation(GELIRI_ARTTI, na_as_no = TRUE)), "No"),
    started_saving = relevel(as.factor(yes_no_transformation(TASARRUF_YAPTI)), "No"),
    bought_a_car = relevel(as.factor(yes_no_transformation(ARABA_ALDI)), "No"),
    bought_a_house = relevel(as.factor(yes_no_transformation(EV_ALDI)), "No"),
    paid_off_debt = relevel(as.factor(yes_no_transformation(BORC_ODEDI, na_as_no = TRUE)), "No"),
    had_a_child = relevel(as.factor(yes_no_transformation(COCUK_OLDU)), "No"),
    lost_job = relevel(as.factor(yes_no_transformation(ISINI_KAYBETTI, na_as_no = TRUE)), "No"),
    went_bankrupt = relevel(as.factor(yes_no_transformation(IFLAS_ETTI, na_as_no = TRUE)), "No"),
    income_decreased = relevel(as.factor(yes_no_transformation(GELIRI_AZALDI, na_as_no = TRUE)), "No"),
    savings_decreased = relevel(as.factor(yes_no_transformation(TASARRUFLARI_AZALDI, na_as_no = TRUE)), "No"),
    bought_cheaper_products = relevel(as.factor(yes_no_transformation(UCUZ_URUN)), "No"),
    cut_vacation_spending = relevel(as.factor(yes_no_transformation(TATIL_KISTI, na_as_no = TRUE)), "No"),
    sold_car = relevel(as.factor(yes_no_transformation(ARABA_SATTI, na_as_no = TRUE)), "No"),
    sold_house = relevel(as.factor(yes_no_transformation(EV_SATTI, na_as_no = TRUE)), "No"),
    went_into_debt = relevel(as.factor(yes_no_transformation(BORCLANDI)), "No"),
    got_divorced = relevel(as.factor(yes_no_transformation(BOSANDI, na_as_no = TRUE)), "No"),
    had_a_bereavement = relevel(as.factor(yes_no_transformation(VEFAT)), "No"),
    had_a_health_problem = relevel(as.factor(yes_no_transformation(SAGLIK_PROBLEM)), "No"),
    pressure_gender = relevel(as.factor(social_pressure_transformation(CINSIYET_BASKI)), "No Pressure"),
    pressure_marital_status = relevel(as.factor(social_pressure_transformation(MEDENI_DURUM_BASKI)), "No Pressure"),
    pressure_age = relevel(as.factor(social_pressure_transformation(YAS_BASKI)), "No Pressure"),
    pressure_tradition = relevel(as.factor(social_pressure_transformation(GELENEK_GORENEK)), "No Pressure"),
    pressure_religious_belief = relevel(as.factor(social_pressure_transformation(DINI_INANCDAN)), "No Pressure"),
    pressure_political_view = relevel(as.factor(social_pressure_transformation(SIYASI_GORUSTEN)), "No Pressure"),
    pressure_hometown = relevel(as.factor(social_pressure_transformation(MEMLEKETINDEN)), "No Pressure"),
    pressure_clothing = relevel(as.factor(social_pressure_transformation(KILIK_KIYAFET)), "No Pressure"),
    pressure_income_level = relevel(as.factor(social_pressure_transformation(GELIR_DUZEY_DURUM)), "No Pressure"),
    crime_bag_snatching = relevel(as.factor(yes_no_transformation(KAPKAC)), "No"),
    crime_robbery = relevel(as.factor(yes_no_transformation(GASP)), "No"),
    crime_physical_assault = relevel(as.factor(yes_no_transformation(DARP)), "No"),
    crime_mistreatment = relevel(as.factor(yes_no_transformation(FERT_KOTU)), "No"),
    crime_threat = relevel(as.factor(yes_no_transformation(TEHDIT)), "No"),
    crime_victimization_other = relevel(as.factor(yes_no_transformation(MAGDURIYET)), "No"),
    crime_fraud = relevel(as.factor(yes_no_transformation(DOLANDIR)), "No"),
    crime_witnessed_someone_elses_crime = relevel(as.factor(yes_no_transformation(BASKASUC)), "No"),
    crime_burglary_general = relevel(as.factor(yes_no_transformation(HIRSIZLIK)), "No"),
    crime_home_burglary = relevel(as.factor(yes_no_transformation(EV_HIRS, na_as_no = TRUE)), "No"),
    crime_workplace_burglary = relevel(as.factor(yes_no_transformation(IS_HIRS, na_as_no = TRUE)), "No"),
    crime_farmland_theft = relevel(as.factor(yes_no_transformation(TARLA_HIRS, na_as_no = TRUE)), "No"),
    crime_car_theft = relevel(as.factor(yes_no_transformation(OTO_HIRS, na_as_no = TRUE)), "No"),
    crime_motorcycle_theft = relevel(as.factor(yes_no_transformation(MOTOR_HIRS, na_as_no = TRUE)), "No"),
    crime_other = relevel(as.factor(yes_no_transformation(SUC, na_as_no = TRUE)), "No"),
    leaky_roof = relevel(as.factor(yes_no_transformation(CATI)), "No"),
    poor_lighting = relevel(as.factor(yes_no_transformation(KARANLIK)), "No"),
    flooding = relevel(as.factor(yes_no_transformation(SU_BASKINI)), "No"),
    indoor_noise = relevel(as.factor(yes_no_transformation(K_GURULTU)), "No"),
    outdoor_noise = relevel(as.factor(yes_no_transformation(S_GURULTU)), "No"),
    power_outages = relevel(as.factor(yes_no_transformation(ELEK_KESINTI)), "No"),
    heating_problem = relevel(as.factor(yes_no_transformation(ISINMA_PROBLEM)), "No"),
    concern_clothing = relevel(as.factor(comparison_concern_transformation(KIYAFET_ONEM)), "Not Important"),
    concern_family_lifestyle = relevel(as.factor(comparison_concern_transformation(AILE_YASAM_ONEM)), "Not Important"),
    concern_personal_belongings = relevel(as.factor(comparison_concern_transformation(KISISEL_ESYA_ONEM)), "Not Important"),
    concern_friend_circle = relevel(as.factor(comparison_concern_transformation(ARKADAS_CEVRE_ONEM)), "Not Important"),
    concern_childrens_success = relevel(as.factor(comparison_concern_transformation(COCUK_BASARI_ONEM)), "Not Important"),
    concern_curfew_hours = relevel(as.factor(comparison_concern_transformation(EVE_GIRIS_SAAT_ONEM)), "Not Important"),
    concern_occupation = relevel(as.factor(comparison_concern_transformation(YAPTIKLARI_IS_ONEM)), "Not Important"),
    concern_income_level = relevel(as.factor(comparison_concern_transformation(GELIR_DUZEYLERI_ONEM)), "Not Important"),
    concern_religious_belief = relevel(as.factor(comparison_concern_transformation(DINI_INANC_ONEM)), "Not Important"),
    concern_political_view = relevel(as.factor(comparison_concern_transformation(SIYASI_DUSUNCE_ONEM)), "Not Important"),
    concern_education_level = relevel(as.factor(comparison_concern_transformation(EGITIM_DUZEYI_ONEM)), "Not Important"),
    reputation_clothing = relevel(as.factor(comparison_concern_transformation(KIYAFETINIZ_ONEM)), "Not Important"),
    reputation_family_lifestyle = relevel(as.factor(comparison_concern_transformation(AILE_YASAM_BICIMI_ONEM)), "Not Important"),
    reputation_personal_belongings = relevel(as.factor(comparison_concern_transformation(KISISEL_ESYANIZ_ONEM)), "Not Important"),
    reputation_friend_circle = relevel(as.factor(comparison_concern_transformation(ARKADAS_CEVRENIZ_ONEM)), "Not Important"),
    reputation_childrens_success = relevel(as.factor(comparison_concern_transformation(COCUGUN_BASARISI_ONEM)), "Not Important"),
    reputation_curfew_hours = relevel(as.factor(comparison_concern_transformation(EVE_GIRISCIKIC_SAAT_ONEM)), "Not Important"),
    reputation_income_level = relevel(as.factor(comparison_concern_transformation(GELIR_DUZEY_ONEM)), "Not Important"),
    reputation_religious_belief = relevel(as.factor(comparison_concern_transformation(DINI_INANCI_ONEM)), "Not Important"),
    reputation_political_view = relevel(as.factor(comparison_concern_transformation(SIYASI_DUSUNCENIZ_ONEM)), "Not Important"),
    reputation_education_level = relevel(as.factor(comparison_concern_transformation(EGITIM_DURUMU_ONEM)), "Not Important"),
    outlook_life_in_general = relevel(as.factor(future_outlook_transformation(GELECEK_HAYAT)), "Same / No Opinion"),
    outlook_personal_job = relevel(as.factor(future_outlook_transformation(GELECEK_KISISEL)), "Same / No Opinion"),
    outlook_household_finances = relevel(as.factor(future_outlook_transformation(GELECEK_MALI)), "Same / No Opinion"),
    outlook_national_employment = relevel(as.factor(future_outlook_transformation(GELECEK_IS)), "Same / No Opinion"),
    outlook_national_economy = relevel(as.factor(future_outlook_transformation(GELECEK_EKONOMI)), "Same / No Opinion")
)]
panel_2017[, `:=`(
    has_positive_life_event = relevel(as.factor(ifelse(pmax(ifelse(got_married == "Yes", 1, 0), ifelse(got_a_job == "Yes", 1, 0), ifelse(opened_a_business == "Yes", 1, 0), ifelse(income_increased == "Yes", 1, 0), ifelse(started_saving == "Yes", 1, 0), ifelse(bought_a_car == "Yes", 1, 0), ifelse(bought_a_house == "Yes", 1, 0), ifelse(paid_off_debt == "Yes", 1, 0), ifelse(had_a_child == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    has_negative_life_event = relevel(as.factor(ifelse(pmax(ifelse(lost_job == "Yes", 1, 0), ifelse(went_bankrupt == "Yes", 1, 0), ifelse(income_decreased == "Yes", 1, 0), ifelse(savings_decreased == "Yes", 1, 0), ifelse(bought_cheaper_products == "Yes", 1, 0), ifelse(cut_vacation_spending == "Yes", 1, 0), ifelse(sold_car == "Yes", 1, 0), ifelse(sold_house == "Yes", 1, 0), ifelse(went_into_debt == "Yes", 1, 0), ifelse(got_divorced == "Yes", 1, 0), ifelse(had_a_bereavement == "Yes", 1, 0), ifelse(had_a_health_problem == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    faced_social_pressure = relevel(as.factor(ifelse(pmax(ifelse(pressure_gender == "Felt Pressure", 1, 0), ifelse(pressure_marital_status == "Felt Pressure", 1, 0), ifelse(pressure_age == "Felt Pressure", 1, 0), ifelse(pressure_tradition == "Felt Pressure", 1, 0), ifelse(pressure_religious_belief == "Felt Pressure", 1, 0), ifelse(pressure_political_view == "Felt Pressure", 1, 0), ifelse(pressure_hometown == "Felt Pressure", 1, 0), ifelse(pressure_clothing == "Felt Pressure", 1, 0), ifelse(pressure_income_level == "Felt Pressure", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    experienced_crime = relevel(as.factor(ifelse(pmax(ifelse(crime_bag_snatching == "Yes", 1, 0), ifelse(crime_robbery == "Yes", 1, 0), ifelse(crime_physical_assault == "Yes", 1, 0), ifelse(crime_mistreatment == "Yes", 1, 0), ifelse(crime_threat == "Yes", 1, 0), ifelse(crime_victimization_other == "Yes", 1, 0), ifelse(crime_fraud == "Yes", 1, 0), ifelse(crime_witnessed_someone_elses_crime == "Yes", 1, 0), ifelse(crime_burglary_general == "Yes", 1, 0), ifelse(crime_home_burglary == "Yes", 1, 0), ifelse(crime_workplace_burglary == "Yes", 1, 0), ifelse(crime_farmland_theft == "Yes", 1, 0), ifelse(crime_car_theft == "Yes", 1, 0), ifelse(crime_motorcycle_theft == "Yes", 1, 0), ifelse(crime_other == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    is_faced_with_a_housing_issue = relevel(as.factor(ifelse(pmax(ifelse(leaky_roof == "Yes", 1, 0), ifelse(poor_lighting == "Yes", 1, 0), ifelse(flooding == "Yes", 1, 0), ifelse(indoor_noise == "Yes", 1, 0), ifelse(outdoor_noise == "Yes", 1, 0), ifelse(power_outages == "Yes", 1, 0), ifelse(heating_problem == "Yes", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    has_comparison_concern = relevel(as.factor(ifelse(pmax(ifelse(concern_clothing == "Important", 1, 0), ifelse(concern_family_lifestyle == "Important", 1, 0), ifelse(concern_personal_belongings == "Important", 1, 0), ifelse(concern_friend_circle == "Important", 1, 0), ifelse(concern_childrens_success == "Important", 1, 0), ifelse(concern_curfew_hours == "Important", 1, 0), ifelse(concern_occupation == "Important", 1, 0), ifelse(concern_income_level == "Important", 1, 0), ifelse(concern_religious_belief == "Important", 1, 0), ifelse(concern_political_view == "Important", 1, 0), ifelse(concern_education_level == "Important", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    has_reputation_concern = relevel(as.factor(ifelse(pmax(ifelse(reputation_clothing == "Important", 1, 0), ifelse(reputation_family_lifestyle == "Important", 1, 0), ifelse(reputation_personal_belongings == "Important", 1, 0), ifelse(reputation_friend_circle == "Important", 1, 0), ifelse(reputation_childrens_success == "Important", 1, 0), ifelse(reputation_curfew_hours == "Important", 1, 0), ifelse(reputation_income_level == "Important", 1, 0), ifelse(reputation_religious_belief == "Important", 1, 0), ifelse(reputation_political_view == "Important", 1, 0), ifelse(reputation_education_level == "Important", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    expects_personal_outlook_to_worsen = relevel(as.factor(ifelse(pmax(ifelse(outlook_life_in_general == "Expects Worse", 1, 0), ifelse(outlook_personal_job == "Expects Worse", 1, 0), ifelse(outlook_household_finances == "Expects Worse", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No"),
    expects_national_outlook_to_worsen = relevel(as.factor(ifelse(pmax(ifelse(outlook_national_employment == "Expects Worse", 1, 0), ifelse(outlook_national_economy == "Expects Worse", 1, 0), na.rm = TRUE) == 1, "Yes", "No")), "No")
)]

# Logistic Regressions ----
# Only works for 0/1 y variables. Not Ologit.

## Main (perception-based) ----
main_2013_results <- summary(glm(happiness_binary ~ gender + age + age_squared + marital_status_satisfaction + education_level + education_satisfaction + employment_job_satisfaction + materialism + wellbeing_ladder + hope_level + household_income_tier + income_sufficiency + income_satisfaction + health_satisfaction + housing_satisfaction + safety_at_home + safety_around_home + social_life_satisfaction + leisure_satisfaction + extended_family_satisfaction + friends_satisfaction + housing_tenure + expectation_5y_later + has_negative_life_event + faced_social_pressure + is_faced_with_a_housing_issue + religiosity, data = panel_2013, family = "binomial"))
main_2014_results <- summary(glm(happiness_binary ~ gender + age + age_squared + marital_status_satisfaction + education_level + education_satisfaction + employment_job_satisfaction + materialism + wellbeing_ladder + hope_level + household_income_tier + income_sufficiency + income_satisfaction + health_satisfaction + housing_satisfaction + safety_at_home + safety_around_home + social_life_satisfaction + leisure_satisfaction + extended_family_satisfaction + friends_satisfaction + housing_tenure + expectation_5y_later + has_negative_life_event + faced_social_pressure + is_faced_with_a_housing_issue + religiosity, data = panel_2014, family = "binomial"))
main_2015_results <- summary(glm(happiness_binary ~ gender + age + age_squared + marital_status_satisfaction + education_level + education_satisfaction + employment_job_satisfaction + materialism + wellbeing_ladder + hope_level + household_income_tier + income_sufficiency + income_satisfaction + health_satisfaction + housing_satisfaction + safety_at_home + safety_around_home + social_life_satisfaction + leisure_satisfaction + extended_family_satisfaction + friends_satisfaction + housing_tenure + expectation_5y_later + has_negative_life_event + faced_social_pressure + is_faced_with_a_housing_issue + religiosity, data = panel_2015, family = "binomial"))
main_2016_results <- summary(glm(happiness_binary ~ gender + age + age_squared + marital_status_satisfaction + education_level + education_satisfaction + employment_job_satisfaction + materialism + wellbeing_ladder + hope_level + household_income_tier + income_sufficiency + income_satisfaction + health_satisfaction + housing_satisfaction + safety_at_home + safety_around_home + social_life_satisfaction + leisure_satisfaction + extended_family_satisfaction + friends_satisfaction + housing_tenure + expectation_5y_later + has_negative_life_event + faced_social_pressure + is_faced_with_a_housing_issue + religiosity, data = panel_2016, family = "binomial"))
main_2017_results <- summary(glm(happiness_binary ~ gender + age + age_squared + marital_status_satisfaction + education_level + education_satisfaction + employment_job_satisfaction + materialism + wellbeing_ladder + hope_level + household_income_tier + income_sufficiency + income_satisfaction + health_satisfaction + housing_satisfaction + safety_at_home + safety_around_home + social_life_satisfaction + leisure_satisfaction + extended_family_satisfaction + friends_satisfaction + housing_tenure + expectation_5y_later + has_negative_life_event + faced_social_pressure + is_faced_with_a_housing_issue + religiosity, data = panel_2017, family = "binomial"))

## Events (occurrence-based) ----
events_2013_results <- summary(glm(happiness_binary ~ went_into_debt + income_decreased + bought_cheaper_products + heating_problem + pressure_marital_status + leaky_roof + outlook_life_in_general + had_a_health_problem + started_saving + poor_lighting + got_married + outlook_national_economy + outlook_household_finances + had_a_child + outdoor_noise + pressure_income_level + flooding + concern_clothing + crime_mistreatment, data = panel_2013, family = "binomial"))
events_2014_results <- summary(glm(happiness_binary ~ went_into_debt + income_decreased + bought_cheaper_products + heating_problem + pressure_marital_status + leaky_roof + outlook_life_in_general + had_a_health_problem + started_saving + poor_lighting + got_married + outlook_national_economy + outlook_household_finances + had_a_child + outdoor_noise + pressure_income_level + flooding + concern_clothing + crime_mistreatment, data = panel_2014, family = "binomial"))
events_2015_results <- summary(glm(happiness_binary ~ went_into_debt + income_decreased + bought_cheaper_products + heating_problem + pressure_marital_status + leaky_roof + outlook_life_in_general + had_a_health_problem + started_saving + poor_lighting + got_married + outlook_national_economy + outlook_household_finances + had_a_child + outdoor_noise + pressure_income_level + flooding + concern_clothing + crime_mistreatment, data = panel_2015, family = "binomial"))
events_2016_results <- summary(glm(happiness_binary ~ went_into_debt + income_decreased + bought_cheaper_products + heating_problem + pressure_marital_status + leaky_roof + outlook_life_in_general + had_a_health_problem + started_saving + poor_lighting + got_married + outlook_national_economy + outlook_household_finances + had_a_child + outdoor_noise + pressure_income_level + flooding + concern_clothing + crime_mistreatment, data = panel_2016, family = "binomial"))
events_2017_results <- summary(glm(happiness_binary ~ went_into_debt + income_decreased + bought_cheaper_products + heating_problem + pressure_marital_status + leaky_roof + outlook_life_in_general + had_a_health_problem + started_saving + poor_lighting + got_married + outlook_national_economy + outlook_household_finances + had_a_child + outdoor_noise + pressure_income_level + flooding + concern_clothing + crime_mistreatment, data = panel_2017, family = "binomial"))

## Combined (Main + forward-selected Events) ----
combined_2013_results <- summary(glm(happiness_binary ~ gender + age + age_squared + marital_status_satisfaction + education_level + education_satisfaction + employment_job_satisfaction + materialism + wellbeing_ladder + hope_level + household_income_tier + income_sufficiency + income_satisfaction + health_satisfaction + housing_satisfaction + safety_at_home + safety_around_home + social_life_satisfaction + leisure_satisfaction + extended_family_satisfaction + friends_satisfaction + housing_tenure + expectation_5y_later + faced_social_pressure + is_faced_with_a_housing_issue + religiosity + went_into_debt + outlook_household_finances + bought_cheaper_products + concern_clothing, data = panel_2013, family = "binomial"))
combined_2014_results <- summary(glm(happiness_binary ~ gender + age + age_squared + marital_status_satisfaction + education_level + education_satisfaction + employment_job_satisfaction + materialism + wellbeing_ladder + hope_level + household_income_tier + income_sufficiency + income_satisfaction + health_satisfaction + housing_satisfaction + safety_at_home + safety_around_home + social_life_satisfaction + leisure_satisfaction + extended_family_satisfaction + friends_satisfaction + housing_tenure + expectation_5y_later + faced_social_pressure + is_faced_with_a_housing_issue + religiosity + went_into_debt + outlook_household_finances + bought_cheaper_products + concern_clothing, data = panel_2014, family = "binomial"))
combined_2015_results <- summary(glm(happiness_binary ~ gender + age + age_squared + marital_status_satisfaction + education_level + education_satisfaction + employment_job_satisfaction + materialism + wellbeing_ladder + hope_level + household_income_tier + income_sufficiency + income_satisfaction + health_satisfaction + housing_satisfaction + safety_at_home + safety_around_home + social_life_satisfaction + leisure_satisfaction + extended_family_satisfaction + friends_satisfaction + housing_tenure + expectation_5y_later + faced_social_pressure + is_faced_with_a_housing_issue + religiosity + went_into_debt + outlook_household_finances + bought_cheaper_products + concern_clothing, data = panel_2015, family = "binomial"))
combined_2016_results <- summary(glm(happiness_binary ~ gender + age + age_squared + marital_status_satisfaction + education_level + education_satisfaction + employment_job_satisfaction + materialism + wellbeing_ladder + hope_level + household_income_tier + income_sufficiency + income_satisfaction + health_satisfaction + housing_satisfaction + safety_at_home + safety_around_home + social_life_satisfaction + leisure_satisfaction + extended_family_satisfaction + friends_satisfaction + housing_tenure + expectation_5y_later + faced_social_pressure + is_faced_with_a_housing_issue + religiosity + went_into_debt + outlook_household_finances + bought_cheaper_products + concern_clothing, data = panel_2016, family = "binomial"))
combined_2017_results <- summary(glm(happiness_binary ~ gender + age + age_squared + marital_status_satisfaction + education_level + education_satisfaction + employment_job_satisfaction + materialism + wellbeing_ladder + hope_level + household_income_tier + income_sufficiency + income_satisfaction + health_satisfaction + housing_satisfaction + safety_at_home + safety_around_home + social_life_satisfaction + leisure_satisfaction + extended_family_satisfaction + friends_satisfaction + housing_tenure + expectation_5y_later + faced_social_pressure + is_faced_with_a_housing_issue + religiosity + went_into_debt + outlook_household_finances + bought_cheaper_products + concern_clothing, data = panel_2017, family = "binomial"))

# Append-Compatibility Check ----
# Confirms panel_2013..panel_2017 share identical column names/order and
# compatible types before being rbind-ed into one panel.
panel_list <- list(panel_2013 = panel_2013, panel_2014 = panel_2014, panel_2015 = panel_2015, panel_2016 = panel_2016, panel_2017 = panel_2017)

check_append_compatibility <- function(dt_list){
    names_by_year <- lapply(dt_list, names)
    reference <- names_by_year[[1]]
    name_mismatches <- names(dt_list)[!sapply(names_by_year, function(x) identical(x, reference))]

    types_by_year <- lapply(dt_list, function(dt) sapply(dt, class)[reference])
    reference_types <- types_by_year[[1]]
    type_mismatches <- names(dt_list)[!sapply(types_by_year, function(x) identical(x, reference_types))]

    list(name_mismatches = name_mismatches, type_mismatches = type_mismatches)
}

panel_compat <- check_append_compatibility(panel_list)

if(length(panel_compat$name_mismatches) == 0 && length(panel_compat$type_mismatches) == 0){
    full_panel <- rbindlist(panel_list, idcol = "source_year")
    cat("panel_2013..panel_2017 append cleanly:", nrow(full_panel), "rows.\n")
} else {
    cat("panel_YYYY tables are NOT append-compatible.\n")
    cat("Name mismatches:", paste(panel_compat$name_mismatches, collapse = ", "), "\n")
    cat("Type mismatches:", paste(panel_compat$type_mismatches, collapse = ", "), "\n")
}

# Panel Outputs ----

events_2013 = cbind(
    as.data.table(row.names(events_2013_results$coefficients)),
    as.data.table(events_2013_results$coefficients)
)
events_2013 = events_2013[, .(
    coefficients = V1,
    estimate_2013 = Estimate,
    z_2013 = `Pr(>|z|)` < 0.05
)]

events_2014 = cbind(
    as.data.table(row.names(events_2014_results$coefficients)),
    as.data.table(events_2014_results$coefficients)
)
events_2014 = events_2014[, .(
    coefficients = V1,
    estimate_2014 = Estimate,
    z_2014 = `Pr(>|z|)` < 0.05
)]

events_2015 = cbind(
    as.data.table(row.names(events_2015_results$coefficients)),
    as.data.table(events_2015_results$coefficients)
)
events_2015 = events_2015[, .(
    coefficients = V1,
    estimate_2015 = Estimate,
    z_2015 = `Pr(>|z|)` < 0.05
)]

events_2016 = cbind(
    as.data.table(row.names(events_2016_results$coefficients)),
    as.data.table(events_2016_results$coefficients)
)
events_2016 = events_2016[, .(
    coefficients = V1,
    estimate_2016 = Estimate,
    z_2016 = `Pr(>|z|)` < 0.05
)]

events_2017 = cbind(
    as.data.table(row.names(events_2017_results$coefficients)),
    as.data.table(events_2017_results$coefficients)
)
events_2017 = events_2017[, .(
    coefficients = V1,
    estimate_2017 = Estimate,
    z_2017 = `Pr(>|z|)` < 0.05
)]

events = merge(
    events_2013,
    events_2014,
    by = 'coefficients')

events = merge(
    events,
    events_2015,
    by = 'coefficients'
)

events = merge(
    events,
    events_2016,
    by = 'coefficients'
)

events = merge(
    events,
    events_2017,
    by = 'coefficients'
)

fwrite(events, 'agg_data/events.csv', dec = ',', sep = '|')

main_2013 = cbind(
    as.data.table(row.names(main_2013_results$coefficients)),
    as.data.table(main_2013_results$coefficients)
)
main_2013 = main_2013[, .(
    coefficients = V1,
    estimate_2013 = Estimate,
    z_2013 = `Pr(>|z|)` < 0.05
)]

main_2014 = cbind(
    as.data.table(row.names(main_2014_results$coefficients)),
    as.data.table(main_2014_results$coefficients)
)
main_2014 = main_2014[, .(
    coefficients = V1,
    estimate_2014 = Estimate,
    z_2014 = `Pr(>|z|)` < 0.05
)]

main_2015 = cbind(
    as.data.table(row.names(main_2015_results$coefficients)),
    as.data.table(main_2015_results$coefficients)
)
main_2015 = main_2015[, .(
    coefficients = V1,
    estimate_2015 = Estimate,
    z_2015 = `Pr(>|z|)` < 0.05
)]

main_2016 = cbind(
    as.data.table(row.names(main_2016_results$coefficients)),
    as.data.table(main_2016_results$coefficients)
)
main_2016 = main_2016[, .(
    coefficients = V1,
    estimate_2016 = Estimate,
    z_2016 = `Pr(>|z|)` < 0.05
)]

main_2017 = cbind(
    as.data.table(row.names(main_2017_results$coefficients)),
    as.data.table(main_2017_results$coefficients)
)
main_2017 = main_2017[, .(
    coefficients = V1,
    estimate_2017 = Estimate,
    z_2017 = `Pr(>|z|)` < 0.05
)]

main =  merge(
    main_2013,
    main_2014,
    by = 'coefficients')

main =  merge(
    main,
    main_2015,
    by = 'coefficients'
)

main =  merge(
    main,
    main_2016,
    by = 'coefficients'
)

main =  merge(
    main,
    main_2017,
    by = 'coefficients'
)

fwrite(main, 'agg_data/main.csv', dec = ',', sep = '|')

combined_2013 = cbind(
    as.data.table(row.names(combined_2013_results$coefficients)),
    as.data.table(combined_2013_results$coefficients)
)
combined_2013 = combined_2013[, .(
    coefficients = V1,
    estimate_2013 = Estimate,
    z_2013 = `Pr(>|z|)` < 0.05
)]

combined_2014 = cbind(
    as.data.table(row.names(combined_2014_results$coefficients)),
    as.data.table(combined_2014_results$coefficients)
)
combined_2014 = combined_2014[, .(
    coefficients = V1,
    estimate_2014 = Estimate,
    z_2014 = `Pr(>|z|)` < 0.05
)]

combined_2015 = cbind(
    as.data.table(row.names(combined_2015_results$coefficients)),
    as.data.table(combined_2015_results$coefficients)
)
combined_2015 = combined_2015[, .(
    coefficients = V1,
    estimate_2015 = Estimate,
    z_2015 = `Pr(>|z|)` < 0.05
)]

combined_2016 = cbind(
    as.data.table(row.names(combined_2016_results$coefficients)),
    as.data.table(combined_2016_results$coefficients)
)
combined_2016 = combined_2016[, .(
    coefficients = V1,
    estimate_2016 = Estimate,
    z_2016 = `Pr(>|z|)` < 0.05
)]

combined_2017 = cbind(
    as.data.table(row.names(combined_2017_results$coefficients)),
    as.data.table(combined_2017_results$coefficients)
)
combined_2017 = combined_2017[, .(
    coefficients = V1,
    estimate_2017 = Estimate,
    z_2017 = `Pr(>|z|)` < 0.05
)]

combined =  merge(
    combined_2013,
    combined_2014,
    by = 'coefficients')

combined =  merge(
    combined,
    combined_2015,
    by = 'coefficients'
)

combined =  merge(
    combined,
    combined_2016,
    by = 'coefficients'
)

combined =  merge(
    combined,
    combined_2017,
    by = 'coefficients'
)

fwrite(combined, 'agg_data/combined.csv', dec = ',', sep = '|')

# Pressure x Satisfaction ----
gender = full_panel[, .(mean(happiness_ordered)), .(source_year, gender, pressure_gender)][order(source_year, gender, pressure_gender)]
marital = full_panel[, .(mean(happiness_ordered)), .(pressure_marital_status, marital_status_satisfaction)][order(pressure_marital_status, marital_status_satisfaction)]
employment = full_panel[, .(mean(happiness_ordered)), .(employment_job_satisfaction, pressure_income_level)][order(employment_job_satisfaction, pressure_income_level)]

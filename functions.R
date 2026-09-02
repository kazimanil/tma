## Created By: Kazım Anıl Eren
## Created On: 2020-02-17
## Edited  On: 2026-09-02
## Target    : Standardising the answers to different questions from TURKSTAT's LSS (Life Satisfaction Survey) questionnaire into 0-1 scale.
## Edits     : Edited to cover the newly added questions in the 2013 and 2014 surveys. Added functions to convert numeric values to categorical values for gender, marital
## Functions :

scale_transformation = function(value, minimum = 5, maximum = 1){
    # value: column to be transformed.
    # maximum: maximum value of the scale.
    # minimum: minimum value of the scale.
    # Unconventionally, TURKSTAT assigns higher values to worse values. 1 as Very Happy or 5 as Very Unhappy as examples.
    # Thus, a conversion is needed for 1-5 scale questions.
    if(minimum > maximum){
      number_range = seq(maximum, minimum, 1)
    } else{
      number_range = seq(minimum, maximum, 1)
    }
  if(value %in% number_range){
    return = (value - minimum) / (maximum - minimum)
  } else {
    return = as.numeric(NA)
  }
  return
}
scale_transformation = Vectorize(scale_transformation)

gender = function(value){
    # TURKSTAT assigns value 1 to female and 2 to male.
    if(value==1){
        gender = "Male"
    } else if(value==2){
        gender = "Female"
    } else {
        gender = as.character(NA)
    }
    gender
}
gender = Vectorize(gender)

marital_status = function(value){
  # Replies to marital status question are encoded in numeric values.
  # This function reverts it into categorical values.
  if(value == 1){
    marital = "Single"
  } else if(value == 2){
    marital = "Married"
  } else if(value %in% c(3,4,5)){
    marital = "Other" # Other includes: Widow, Divorced, Married but living separately.
  } else {
    marital = as.character(NA)
  }
  marital
}
marital_status = Vectorize(marital_status)

education_level = function(value, scheme = "pre2013"){
  if(scheme == "pre2013"){
    if(value > 0 & value < 3){
      education = "No Schooling"
    } else if(value > 2 & value < 8){
      education = "Primary Education"
    } else if(value > 7 & value < 13){
      education = "Secondary Education"
    } else if(value > 12 & value < 15){
      education = "Tertiary Education"
    } else {
      education = as.character(NA)
    }
  } else if(scheme == "2013"){
    if(value == 1){
      education = "No Schooling"
    } else if(value %in% c(2, 3)){
      education = "Primary Education"
    } else if(value == 4){
      education = "Secondary Education"
    } else if(value %in% c(5, 6, 7)){
      education = "Tertiary Education"
    } else {
      education = as.character(NA)
    }
  } else if(scheme == "2014_2016"){
    if(value == 0){
      education = "No Schooling"
    } else if(value %in% c(1, 21, 22, 23)){
      education = "Primary Education"
    } else if(value %in% c(31, 32)){
      education = "Secondary Education"
    } else if(value  %in% c(4, 5, 6, 7)){
      education = "Tertiary Education"
    } else {
      education = as.character(NA)
    }
  } else if(scheme == "2017"){
    if(value == 1){
      education = "No Schooling"
    } else if(value %in% c(2, 31, 32, 33)){
      education = "Primary Education"
    } else if(value %in% c(41, 42)){
      education = "Secondary Education"
    } else if(value %in% c(511, 512, 52, 53)){
      education = "Tertiary Education"
    } else {
      education = as.character(NA)
    }
  } else {
    education = as.character(NA)
  }
  education
}
education_level = Vectorize(education_level)

employment = function(value1, value2 = as.numeric(NA), after2013 = FALSE){
  if(after2013 == FALSE){
    if(value1 %in% c(1,2)){
      labour = "Employed"
    } else if(value1 == 3){
      labour = "Unemployed"
    } else if(value1 %in% seq(4,9,1)){
      labour = "Out of Labour Force"
    } else {
      labour = as.character(NA)
    } 
  } else if(after2013 == TRUE){
    if(value1 %in% c(1,2)){
      labour = "Employed"
    } else if(value1 == 3 & value2 %in% c(1,2)){
      labour = "Unemployed"
    } else if(value1 == 3 & value2 %in% c(3,4,5,6,7,8,9,98)){
      labour = "Out of Labour Force"
    } else {
      labour = as.character(NA)
    }
  } else {
    labour = as.character(NA)
  }
  labour
}
  
employment = Vectorize(employment)

comparison_5y_ago = function(value){
  if(value == 1){
    return = "Better then Before"
  } else if(value == 2){
    return = "Same as Before"
  } else if(value == 3){
    return = "Worse then Before"
  } else {
    return = "No Idea"
  }
  return
}
comparison_5y_ago = Vectorize(comparison_5y_ago)


expectations_5y_later = function(value){
  if(value == 1){
    return = "Better then Now"
  } else if(value == 2){
    return = "Same as Now"
  } else if(value == 3){
    return = "Worse then Now"
  } else {
    return = "No Idea"
  }
  return
}
expectations_5y_later = Vectorize(expectations_5y_later)

hope_transformation = function(value){
  if(value == 1){
    return = "Very Hopeful"
  } else if(value == 2){
    return = "Hopeful"
  } else if(value == 3){
    return = "Not Hopeful"
  } else if(value == 4){
    return = "Not Hopeful At All"
  }
  return
}
hope_transformation = Vectorize(hope_transformation)

happiness_transformation = function(value){
  if(value %in% c(1,2)){
    happiness = 1
  } else {
    happiness = 0
  }
  happiness
}
happiness_transformation = Vectorize(happiness_transformation)

likert_categoric = function(value, keyword = ""){
  # Change keyword if necessary
  if(value == 1){
    satisfaction = paste0("Very Satisfied from ", keyword)
  } else if(value == 2){
    satisfaction = paste0("Satisfied from ", keyword)
  } else if(value == 3){
    satisfaction = "Neutral"
  } else if(value == 4){
    satisfaction = paste0("Not Satisfied from ", keyword)
  } else if(value == 5){
    satisfaction = paste0("Not Satisfied from ", keyword, " At All")
  } else {
    satisfaction = as.character(NA)
  }
  satisfaction
}
likert_categoric = Vectorize(likert_categoric)

materialism = function(value){
  if(value %in% c(1,2,3,6, 98)){
    materialistic = "Materialistic"
  } else if(value %in% c(4,5)){
    materialistic = "Not Materialistic"
  } else {
    materialistic = as.character(NA)
  }
}

materialism = Vectorize(materialism)

household_income_transformation = function(value){
  return = paste0("Household Income Tier ", value)
  return
}
household_income_transformation = Vectorize(household_income_transformation)

marriage_satisfaction = function(categoric, satisfaction){
  if(categoric == "Married"){
    if(satisfaction > 0.5){
      return = "Happily Married"
    } else if(satisfaction == 0.5){
      return = "Neutral Married"
    } else if(satisfaction < 0.5){
      return = "Unhappily Married"
    }
  } else {
    return = categoric
  }
  return
}

marriage_satisfaction = Vectorize(marriage_satisfaction)

job_satisfaction = function(categoric, satisfaction){
  if(categoric == "Employed"){
    if(satisfaction > 0.5){
      return = "Happily Employed"
    } else if(satisfaction == 0.5){
      return = "Neutral Employed"
    } else if(satisfaction < 0.5){
      return = "Unhappily Employed"
    }
  } else {
    return = categoric
  }
  return
}

job_satisfaction = Vectorize(job_satisfaction)

comparison_concern_transformation = function(value){
  if(value == 3){
    concern = "Not Important"
  } else if(value == 2){
    concern = "Somewhat Important"
  } else if(value == 1){
    concern = "Important"
  } else {
    concern = as.character(NA)
  }
  concern
}
comparison_concern_transformation = Vectorize(comparison_concern_transformation)

future_outlook_transformation = function(value){
  if(value == 3){
    outlook = "Expects Worse"
  } else if(value %in% c(2, 4)){
    outlook = "Same / No Opinion"
  } else if(value == 1){
    outlook = "Expects Better"
  } else {
    outlook = as.character(NA)
  }
  outlook
}
future_outlook_transformation = Vectorize(future_outlook_transformation)

wellbeing_ladder_transformation = function(value, legacy_coding = FALSE){
  rung = if(legacy_coding) value - 1 else value
  if(rung %in% seq(0, 10, 1)){
    rung
  } else {
    as.numeric(NA)
  }
}
wellbeing_ladder_transformation = Vectorize(wellbeing_ladder_transformation)

yes_no_transformation = function(value, na_as_no = FALSE){
  if(is.na(value)){
    if(na_as_no){
      return("No")
    } else {
      return(as.character(NA))
    }
  }
  if(value == 1){
    result = "Yes"
  } else if(value == 2){
    result = "No"
  } else {
    result = as.character(NA)
  }
  result
}
yes_no_transformation = Vectorize(yes_no_transformation)

social_pressure_transformation = function(value){
  if(value == 1){
    result = "No Pressure"
  } else if(value %in% c(2, 3, 4)){
    result = "Felt Pressure"
  } else {
    result = as.character(NA)
  }
  result
}
social_pressure_transformation = Vectorize(social_pressure_transformation)

housing_tenure_transformation = function(value){
  if(value == 1){
    tenure = "Owner"
  } else if(value == 2){
    tenure = "Renter"
  } else if(value == 3){
    tenure = "Employer-Provided Housing"
  } else if(value == 4){
    tenure = "Rent-Free (Non-Owner)"
  } else {
    tenure = as.character(NA)
  }
  tenure
}
housing_tenure_transformation = Vectorize(housing_tenure_transformation)

religiosity_transformation = function(value){
  if(value == 1){
    religiosity = "Religious"
  } else if(value == 2){
    religiosity = "Somewhat Religious"
  } else if(value == 3){
    religiosity = "Not Religious"
  } else {
    religiosity = as.character(NA)
  }
  religiosity
}
religiosity_transformation = Vectorize(religiosity_transformation)


rowmean2 <- function(x, y) {
    m <- rowMeans(cbind(x, y), na.rm = TRUE)
    m[is.nan(m)] <- NA_real_
    m
}
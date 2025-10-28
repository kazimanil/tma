## Created By: Kazım Anıl Eren
## Created On: 2020-02-17
## Edited  On: 2022-10-19
## Target    : Standardising the answers to different questions from TURKSTAT's LSS (Life Satisfaction Survey) questionnaire into 0-1 scale.
## Edits     :
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

education_level = function(value, after2013 = FALSE){
  if(after2013 == FALSE){
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
  } else if(after2013){
    if(value == 0){
      education = "No Schooling"
    } else if(value %in% c(1, 21, 22, 23)){
      education = "Primary Education"
    } else if(value  %in% c(31, 32)){
      education = "Secondary Education"
    } else if(value  %in% c(4, 5, 6, 7)){
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

## Created By: Kazım Anıl Eren
## Created On: 2020-02-17
## Edited  On:
## Target    : Standardising the answers to different questions from TURKSTAT's LSS (Life Satisfaction Survey) questionnaire into 0-1 scale.
## Edits     :
## Functions :

scale_transformation = function(value, minimum = 1, maximum = 5, descending = TRUE){
	# value: column to be transformed.
	# maximum: maximum value of the scale.
  # minimum: minimum value of the scale.
	# descending: TRUE if higher values represent better outcomes.
  # Unconventionally, TURKSTAT assigns higher values to worse values. 1 as Very Happy or 5 as Very Unhappy as examples.
  # Thus, a conversion is needed for 1-5 scale questions.
	if(descending==TRUE){
		return = (maximum - value) / (maximum - minimum)
	} else if (descending == FALSE){
		return = (value - minimum) / (maximum - minimum)
	} else {
		return = as.numeric(NA)
	}
}
scale_transformation = Vectorize(scale_transformation)

gender = function(value){
	# TURKSTAT assigns value 1 to female and 2 to male.
	if(value==2){
		gender = "Male"
	} else if(value==1){
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

education_level = function(value){
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
  education
}
education_level = Vectorize(education_level)

employment = function(value){
  if(value %in% c(1,2)){
    labour = "Employed"
  } else if(value == 3){
    labour = "Unemployed"
  } else if(value %in% seq(4,9,1)){
    labour = "Out of Labour Force"
  } else {
    labour = as.character(NA)
  }
  labour
}
employment = Vectorize(employment)

better_or_worse = function(value, numeric=FALSE){
  if(numeric){
    return = scale_transformation(value, 1, 3)
  } else {
    if(value == 1){
      return = "Better then Before"
    } else if(value == 2){
      return = "Same as Before"
    } else if(value == 3){
      return = "Worse then Before"
    } else {
      return = as.character(NA)
    }
  }
  return
}
better_or_worse = Vectorize(better_or_worse)

happiness_transformation = function(value){
  transformed_value = scale_transformation(value, 1, 5)
  if(transformed_value < 0.5 & transformed_value >=0){
    happiness = "Not Happy"
  } else if(transformed_value > 0.5 & transformed_value <= 1){
    happiness = "Happy"
  } else {
    happiness = as.character(NA)
  }
  happiness
}
happiness_transformation = Vectorize(happiness_transformation)

likert_categoric = function(value, keyword = "Satisfied"){
  # Change keyword if necessary
  if(value == 1){
    satisfaction = paste0("Very ", keyword)
  } else if(value == 2){
    satisfaction = paste0(keyword)
  } else if(value == 3){
    satisfaction = "Neutral"
  } else if(value == 4){
    satisfaction = paste0("Not ", keyword)
  } else if(value == 5){
    satisfaction = paste0("Not ", keyword, " At All")
  } else {
    satisfaction = as.character(NA)
  }
  satisfaction
}
likert_categoric = Vectorize(likert_categoric)

materialism = function(value){
  if(value %in% c(1,2,3,6)){
    materialistic = "Materialistic"
  } else if (value %in% c(4,5)){
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

household_income_sufficiency = function(value){
  
}
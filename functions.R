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

gender = function(x){
	# TURKSTAT assigns value 1 to female and 2 to male.
	if(x==2){
		gender = "male"
	} else if(x==1){
		gender = "female"
	} else {
		gender = as.character(NA)
	}
	gender
}
gender = Vectorize(gender)

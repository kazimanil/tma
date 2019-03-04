likert = function(happiness){
	## Since TURKSTAT labels "Very Happy" as 1 while "Very Unhappy" as 5
	## a correction should be done.
	(5 - happiness) / 4
}
likert = Vectorize(likert)

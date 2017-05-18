## #' prepares a baseline function with sex and age of individuals 
## #' @name baselineFunction_sex_age
## #' @param cohortSize - the number of individuals in the cohort
## #' @usage
## #' baselineFunction_sex_age(1000) 
## #' # returns a data.frame with age and sex of individuals
## #' @author Zofia Baranczuk 
## #'  
## #' 
## #' @description This function prepares a data.frame baseline

## #'@export
baselineFunction_sex_age <- function(cohortSize) {
  age_lower_limit = 10
  age_upper_limit = 60
  data.frame(sex=rbinom(cohortSize, 1, .5), age=runif(cohortSize, age_lower_limit,age_upper_limit)) 
}

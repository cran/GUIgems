baselineFunction <- function(cohortSize) {
  data.frame(sex=rbinom(cohortSize, 1, .5), age=runif(cohortSize, 10,60), 
             Genotype=floor(runif(cohortSize, 0,4)))
}
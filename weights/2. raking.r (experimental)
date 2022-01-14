# In R there is a package so-called anesrake who implements the raking algorithm to weight. 
# You only have to take into account minor details to use its function, but I give you a function
# to simplify the process if you are not very familiar with R programming. 

# You just have to do exactly the same as with the previous file (1.simple_weighting.), passing a 
# datrame and a list with population proportions to the function and, in this case, it is 
# returned directly the vector of weights.

################
# The function #
################

#' Create a vector of weights with raking algorithm
#' 
#' Internally it is called to \code{anesrake::anesrake} function.
#'
#' @param data A dataframe or tibble.
#' @param population A named list with named vectors of population
#'                   proportions.
#' @param ... Whatever argument of \code{anesrake} function. 
#'            See help(anesrake::anesrake)
#'
#' @return
#'  A vector of weights.
#'  
weight_anesrake <- function(data,population,...){
  data$caseid <- 1:nrow(data)
  data <- as.data.frame(data)
  pesos <- as.numeric( anesrake::anesrake(inputter = population, verbose = FALSE,
        dataframe = data[names(population)], caseid= data$caseid, ...)$weightvec)
  return(pesos)
}

###############
# An example: #
###############
set.seed(7)
data <- dplyr::tibble(  # fake data
  'gender' = sample(c('F','M'),350,T,c(.3,.8)) ,
  'age'= sample(c('25','45','55'),350,T,c(.7,.2,.1)),
  'vote'= sample(c('PP','PSOE','UP'),350,T,c(.4,.45,.15))
)
targets <- list( 'gender' = c('M'=.5,'F'=.5),
                 'age' = c('25'=.50,'55'=.25,'45'=.25))
data$weights <- weight_anesrake(data, population = targets)

library(dplyr)
data %>% count(vote) %>% mutate(p=n/sum(n))
data %>% count(vote,wt = weights) %>% mutate(p=n/sum(n))



  

### The function is being checked



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
#'                   proportions; or a list of lists with the different
#'                   targets. See examples.
#' @param ... Whatever argument of \code{anesrake} function. 
#'            See help(anesrake::anesrake)
#'
#' @return
#'  A vector of weights or a list with vectors.
#'  
weight_anesrake <- function(data,population,...){
  if( is.list(population[[1]]) == FALSE ){
    df <- data[,names(population)]
    df <- as.data.frame(purrr::map_df(df,function(x){
      if(is.factor(x)==FALSE) x <- as.factor(x) }))
    df$caseid <- 1:nrow(df)
    weights <- as.numeric( anesrake::anesrake(inputter = population,
                dataframe = df, caseid= df$caseid, ...)$weightvec)
    }
  else if( is.list(population[[1]]) == TRUE ){
    weights <- purrr::map(seq_along(population), function(i){
                            population <- population[[i]]
                            weights <- weight_anesrake(data, population)
                            return(weights)  })
    names(weights) <- names(population)
  }
  return(weights)
}


###############
# An example: #
###############
set.seed(7)
data <- dplyr::tibble(  # fake data
  'gender' = sample(c('F','M'),350,T,c(.3,.8)) ,
  'age'= sample(c('25','45','55'),350,T,c(.7,.2,.1)),
  'studies' = sample(c('High','Other'),350,T,c(.45,.55)),
  'vote'= sample(c('PP','PSOE','UP'),350,T,c(.4,.45,.15))
)
targets <- list( 'gender' = c('M'=.5,'F'=.5),
                 'age' = c('25'=.50,'55'=.25,'45'=.25))
data$weights <- weight_anesrake(data, population = targets)

library(dplyr)
data %>% count(vote) %>% mutate(p=n/sum(n))
data %>% count(vote,wt = weights) %>% mutate(p=n/sum(n))

#########################
# More than one option: #
#########################
target1 <- list( 'gender' = c('M'=.5,'F'=.5),
              'age' = c('18-35'=.50,'66-100'=.25,'36-65'=.25) )
target2 <- list( 'gender' = c('M'=.5,'F'=.5),
              'age' = c('18-35'=.50,'66-100'=.25,'36-65'=.25),
              'studies' = c('High'=.3, 'Other'=.7))
target3 <- list( 'gender' = c('M'=.5,'F'=.5),
                 'studies' = c('High'=.3, 'Other'=.7))
posible_targets <- list('target1'=target1, 'target2'=target2,
                        'target3'=target3)
list_weigths <- weight_anesrake(data,posible_targets)
df_weights <- bind_cols(list_weights) # dataframe with weights
data <- data %>% bind_cols( df_weights ) # bind with data

 
 
 
 
 
 
 
 
 
 
 

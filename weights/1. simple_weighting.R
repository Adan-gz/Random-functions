# In this branch I share two functions to facilitate weighting your data. Here, a function to apply simple weighting,
# i.e. if in the population there 50% males and 50% females but in your sample the values are 60% and 40%, 
# giving more weight to females and less to males when calculating any summary stats. 
# After creating one weighting vector for each desired variable, these vectors are multiplied in order to 
# obtain the unique weighting vector for the data.

################
# The function #
################

#' Create a vector of weights
#' 
#' Given a population distribution, a vector of weights 
#' is obtained to equalize said distribution. 
#'
#' @param data A dataframe or tibble.
#' @param population Two options: a list with named vectors OR a list of lists with named vectors. 
#'                   Each vector contains the population proportions. Use a list of lists if you 
#'                   want to try different weighting options. See examples.
#' @param return_df TRUE if the desired output if the dataframe with one more
#'    column: the vector of weights. If FALSE returns only the vector. 
#'    By default is FALSE. 
#'
#' @return
#'  A dataframe or a numeric vector depending on return_df parameter. If population is a list of lists
#'  it is returned a list with vectors or dataframes. 

simple_weighting <- function(data, population,return_df=FALSE){
  if( all(is.data.frame(data), is.list(population),is.logical(return_df)) != TRUE ){
    rlang::abort('Check the class of your parameters')}
  
  if( is.list(population[[1]]) == FALSE ){
    if( all(names(population) %in% names(data)) == FALSE ){
      rlang::abort("Variable and population names don't match. Check list names.")}
    df <- data[,names(population)]
    weights <- purrr::map(names(population), function(i){
      if( all( names(population[[i]]) %in% unique(df[[i]]) ) == FALSE ){
        rlang::abort("Variable and population target levels don't match. Check named vectors in the list.")}
      
      sample <- prop.table(table(df[[i]]))
      population <- population[[i]][names(sample)]
      weights <- population/sample
      weights <- weights[ df[[i]] ]
      return(weights)
    } )
    weights <- apply(dplyr::bind_cols(weights),1,prod)
    if(return_df==FALSE) return(weights)
    data$weights <- weights
    return(data)
  }
  else if( is.list(population[[1]]) == TRUE ){
    weights <- purrr::map(seq_along(population),
                          ~ simple_weighting(data, population[[.x]],
                          return_df=return_df))
    names(weights) <- names(population)
    return(weights)
  }
}

################
# 1st example: #
################
### fake data
set.seed(7)
data <- tibble(
  'gender' = as.factor(sample(c('F','M'),350,T,c(.3,.8))) ,
  'age'= as.factor(sample(c('18-35','36-65','66-100'),350,T,c(.7,.2,.1))),
  'vote'= sample(c('PP','PSOE','UP'),350,T,c(.4,.45,.15))
  
### population distributions of gender and age 
## the list elements (gender and age in this case) are named equal to the variables
## for weighting. Inside each element you create a named vector with 'c', writing
## the label = population proportion:
population <- list( 'gender' = c('M'=.5,'F'=.5),
                    'age' = c('18-35'=.50,'66-100'=.25,'36-65'=.25) )
                    
### new dataframe with the vector of weights based on gender and age
data$weights <- simple_weighting(data, population)
    ## you can do as well:
  #data <- simple_weighting(data = data, population = population, TRUE)
  #data <- data %>% mutate(weights=simple_weighting(data, population))

### check function behaviour
check_simple_weighting <- function(df,var){
  pro <- list()
  pro$original <- round(prop.table( table(df[[var]]) )*100)
  pro$ponderada <- round(prop.table( questionr::wtd.table(df[[var]],
                                    weights = df[['weights']]))*100)
  return(pro)
}
check_simple_weighting(data,'gender')
check_simple_weighting(data,'age')
  
 # The weighted proportions of party votes are the estimation
check_simple_weighting(data,'vote')

##############################
# 2nd example: list of lists #
############################## 
  # sample:
set.seed(7)
data <- tibble(
  'gender' = as.factor(sample(c('F','M'),350,T,c(.3,.8))) ,
  'age'= as.factor(sample(c('18-35','36-65','66-100'),350,T,c(.7,.2,.1))),
  'studies' = sample(c('High','Other'),350,T,c(.45,.55)),
  'vote'= sample(c('PP','PSOE','UP'),350,T,c(.4,.45,.15)))

   # first option to weight for
target1 <- list( 'gender' = c('M'=.5,'F'=.5),
                  'age' = c('18-35'=.50,'66-100'=.25,'36-65'=.25) )
  # second one
target2 <- list( 'gender' = c('M'=.5,'F'=.5),
              'age' = c('18-35'=.50,'66-100'=.25,'36-65'=.25),
              'studies' = c('High'=.3, 'Other'=.7))
 # combine the two of them in a list
posible_targets <- list( 'target1'=target1, 'target2'=target2  )

simple_weighting(data,posible_targets) # return the vector of weights
simple_weighting(data,posible_targets,TRUE) # return dataframes


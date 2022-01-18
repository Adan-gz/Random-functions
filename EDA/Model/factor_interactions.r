#' Extract possible interactions among factors from a dataframe or a model
#' 
#' When starting to modelize our data, sometimes we want to test a model
#' with all potential interactions among factors and using 'step' function to extract
#' the best one. 
#' 
#' @usage factor_interactions(obj, return_formula = TRUE)
#' 
#' @param obj A model of class glm or lm, or a dataframe in which at least 
#'            more than one variable is a factor
#'            
#' @param return_formula Logical. If obj is a model and the desired output is 
#'            a formula object adding up the interactions
#'
#' @return
#'  A character vector with interactions
#'
#' @examples
#' set.seed(7)
#' df <- data.frame(stringsAsFactors = TRUE,
#'                  y = sample(c(1,0),350,T),
#'                  a = sample(c('A','B'),350,T),
#'                  b = sample(c('H','Y','K'),350,T),
#'                  c = sample(1:30,350,T),
#'                  d = sample(c('K','R','T','Y'),350,T))
#' mod <- glm(y~.,data=df,family = 'binomial')
#' 
#' factor_interactions(mod)
#' y ~ . + a * b + a * d + b * d
#' 
#' #' factor_interactions(mod,FALSE)
#' factor_interactions(df,FALSE)
#' # the output in both cases:
#' [1] "a*b" "a*d" "b*d"

factor_interactions <- function(obj,return_formula=TRUE){
  stopifnot('Input must be a Lm model or dataframe'={is.data.frame(obj) | 'lm' %in% class(obj)   } )
  
  if(is.data.frame(obj)==TRUE) var_fac <- names(obj %>% select_if(is.factor))
  else{
    if( 'glm' %in% class(obj))  var_fac <- names(obj[['data']] %>% select_if(is.factor))
    else if( class(obj) == 'lm' ) var_fac <- names(obj[['model']] %>% select_if(is.factor))
  }
  
  interacciones <- expand.grid(var_fac,var_fac) %>% 
    mutate(across(everything(.),as.character)) %>% 
    transmute(v_min=pmin(Var1,Var2), v_max=pmax(Var1,Var2)) %>% 
    filter( v_min != v_max ) %>% 
    distinct() %>% 
    transmute(inter=paste0(v_min,'*',v_max)) %>% pull()
  
  if(return_formula==FALSE | is.data.frame(obj)==TRUE ) return(interacciones)
  obj_call <- as.character(obj$call)[2]
  init <- str_extract(obj_call, '^(.*)~')
  vis <- str_squish(str_remove(str_extract(obj_call, '~(.*)$'),'~'))
  new_f <- as.formula(paste0(init,vis,'+',paste0(interacciones,collapse = '+')))
  return(new_f)
}


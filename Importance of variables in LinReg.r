# Following this Research Gate question 
# (https://www.researchgate.net/post/How_can_I_determine_the_relative_contribution_of_predictors_in_multiple_regression_models)
# I have designed a function to calculate the relative contribution of predictors in multiple regression models, which means
# 'the unique contribution in % in explaining the variance of the dependent variable'. 
# The function just required one parameter: your linear model. By now it is designed for simply multiple linear models, in the future
# I will adapt it to every type of model. 

#### The function:

lm_contribution <- function(mod, sort=TRUE){
  ## extract SSE 
  mod_anova <- broom::tidy(anova(mod))
  SSE.mod <- mod_anova$sumsq[mod_anova$term=='Residuals'] 
  
  ## extract coef terms, Dependent variable and data
  mod_terms <- names(mod$coefficients)[-1] # terms except intercept
  vd <- gsub('^(.*?)= ','',capture.output(mod$call) )
  vd <- gsub(' ~(.*?)$','',vd) # dependent variable
  df <- mod$model # data

  # calculate contribution (it is calculated as many models as terms,
  # but extracting one of the terms each time)
 cont <- purrr::map_dbl(mod_terms, function(x){
    reg_terms <- mod_terms[mod_terms != x ]
    reg <- lm( as.formula(paste0(vd,'~', paste0(reg_terms,collapse = ' + '))), data=df  )
    reg_anova <- broom::tidy(anova(reg))
    SSE <- reg_anova$sumsq[reg_anova$term=='Residuals'] # (Residual Sum of Squares 
    cont <- (SSE-SSE.mod)/SSE
    return(cont)
  })
  names(cont) <- mod_terms
  if(sort==TRUE) cont <- sort(cont,TRUE)
  return(cont)
} 

## Example:
mod <- lm(mpg~.,data=mtcars)
imp_var <- lm_contribution(mod)
barplot(imp_var)

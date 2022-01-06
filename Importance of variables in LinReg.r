# Following this Research Gate question 
# (https://www.researchgate.net/post/How_can_I_determine_the_relative_contribution_of_predictors_in_multiple_regression_models)
# I have designed a function to calculate the relative contribution of predictors in multiple regression models, which means
# 'the unique contribution in % in explaining the variance of the dependent variable'. 
# The function just required one parameter: your linear model. By now it is designed for simply multiple linear models, in the future
# I will adapt it to every type of model. 

#### The function:

impVarLinReg <- function(mod){
  ## extract SSE 
  mod_anova <- broom::tidy(anova(mod))
  SSE.mod <- mod_anova$sumsq[mod_anova$term=='Residuals'] 
  
  ## extract coef terms, Dependent variable and data
  mod_terms <- names(mod$coefficients)[-1] # términos
  vd <- gsub('^(.*?)= ','',capture.output(mod$call) )
  vd <- gsub(' ~(.*?)$','',vd) # variable dependiente
  df <- mod$model # data

  # calculate contribution (it is calculated as many models as terms,
  # but extracting one of the terms each time)
  cont <- map(mod_terms, function(x){
    mod_terms <- mod_terms[mod_terms != x ]
    reg <- lm( as.formula(paste0(vd,'~', str_flatten(mod_terms,'+'))), data=df  )
    reg_anova <- broom::tidy(anova(reg))
    SSE <- reg_anova$sumsq[reg_anova$term=='Residuals'] # (Residual Sum of Squares 
    cont <- (SSE-SSE.mod)/SSE
    return(cont)
  })
  names(cont) <- mod_terms
  return(cont)
} 

## Example:
mod <- lm(mpg~.,data=mtcars)
impVarLinReg(mod)

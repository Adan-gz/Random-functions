#' Remove empty rows and/or columns from a dataframe
#'  
#' @param df A tibble or dataframe.
#' @param dim A character. Options: 'r' (to remove empty rows), 'c' (empty columns) or
#'            'rc' or 'cr' (for both). It is not case sensitive.
#'
#' @return A filtered tibble/dataframe.
#'
#' @examples
#' df <- dplyr::tibble( x = c(1,2,NA,2),
#'                      y = c(4,5,NA,1),
#'                      a = c(3,8,NA,7),
#'                      b = rep(NA,4) )
#' 
#' remove_empty(df,'r')
#' remove_empty(df,'c')
#' remove_empty(df,'rc')
#' remove_empty(df,'R')
#' remove_empty(df,"B") #yields an error


#### The function:

remove_empty <- function(df,dim=c("r","c","rc","cr")){
  dim <- tolower(dim) 
  rlang::arg_match(dim)
  if(dim=='r'){
    ind <- purrr::map_dbl(1:nrow(df), \(i) sum(is.na(df[i,])) == ncol(df) ) 
    df <- df[-which(ind==1),]
  }
  else if(dim=='c'){
    ind <- purrr::map_dbl(seq_along(df), \(i) sum(is.na(df[,i])) == nrow(df) ) 
    df <- df[, -which(ind==1)]
  }
  else{
    ind_r <- purrr::map_dbl(1:nrow(df), \(i) sum(is.na(df[i,])) == ncol(df) ) 
    ind_c <- purrr::map_dbl(seq_along(df), \(i) sum(is.na(df[,i])) == nrow(df) ) 
    df <- df[-which(ind_r==1),-which(ind_c==1)]
  }
  return(df)
}

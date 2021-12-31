# I create this function to help a student with her data wrangling. She had to carry out for a lot of different variables the same operation:
# construct a new variable (and add it to her dataframe) as the total sum of other variables. And she had to do it more than 30 times. 
# So I came up with this idea: just create a named vector in which the names are the names of the new variables, and the elements are strings
# with the variables to carry out the operation.

# For example, if you use the mtcars dataframe and you want to calculate the sum of the variables cyl and mpg; cyl and disp; and
# cyl, disp and mpg. You can do it for example: mtcars %>% mutate(new_name=mpg+cyl...) and copy-paste for the other operations. But if you have 
# to do it 30 times you can save a lot of it with this:

my_operations <- c( "mpg_cyl" = "mpg,cyl", 
                   "cyl_disp"= "cyl+disp", 
                   "mpg_cyl_disp" = "mpg, cyl, disp" )

mtcars <- add_variables(df = mtcars, v = my_operations, fun=sum )

# As a result you will obtained your dataframe with 3 more variables. By default it is calculated the sum of them, but you can change this parameter.
# Also you can separate the variables by ',' or by '+' (I design the function in this way because she had written the variables in a word document
# sometimes with ',' and other with '+'), and doesn't matter if there are empty spaces. 

# So, here is the function (I add some roxygen comments, not necessary to copy them), just copy and paste in your script and call to it:


#' Add variables
#'
#' @param df Your dataframe.
#' @param v A named vector. The names of the elements will be the names of the new variables. 
#' @param fun The function to implement. By default is the sum. 
#'
#' @return The same dataframe with the new calculated columns.
#'
#' @examples
#' 
#' ops <- c("cyl_disp"= "cyl, disp,  hp",
#'          "cyl_dips_hp"= "cyl +disp+ hp" )
#'
#' add_variables(mtcars,ops) # calculate the sum
#' add_variables(mtcars,ops, fun=mean) #calculate the mean


##### The Function:

# (it is necessary to have installed stringr package)

add_variables <- function(df,v,fun=sum){
  for (i in seq_along(v)) {
    new_name <- names(v[i])
    x <- str_remove_all(v[i]," ") # remove empty spaces
    if( str_detect(x,",") ){ # if variables are given with ',' just split to select afterwards
      v_names <- str_split(x,",")[[1]]  
    } 
    else if( str_detect(x,"\\+") ){ # if given with '+' split by it
      v_names <- str_split(x,"\\+")[[1]]
      }
  
  df[,new_name] <- apply( df[,v_names],1,fun)
  }

  return(df)
}

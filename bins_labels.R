#' Extract appropiated labels whith geom_histogram
#' 
#' When making charts with ggplot2::geom_histogram and changing the binwidth parameter, usually the x axis 
#' labels are not properly positioned. This is and adhoc solution to worki with scale_x_continuos(breaks,labels).
#'
#' @param init The value to start the seq(). By default is 0.
#' @param max Max value of your breaks parameter (usually the max value of a sequence). 
#' @param bins The specified value in bindiwdth.
#' @param n_labels The number of desired x labels. 
#' @param sep The character separator of pair of values. By default ' -\n'.
#'
#' @return
#' A character vector 
#' 
#' @examples
#' bins_labels(500,50,5,' - ')
#' [1] "0 - 50"    ""     "100 - 150" ""    "200 - 250"    ""     "300 - 350" "" 
#' [9] "400 - 450"   ""    ""  
#' 
#' # plot example:
#' library(dplyr); library(ggplot2); set.seed(7)
#' data <- data.frame(x=rnorm(1500,500,100))
#' range(data$x) # from 200 to 800
#' data %>% 
#' ggplot(aes(x))+
#'  geom_histogram(binwidth = 50,col='gray')+ # binwidth = 50
#'  scale_x_continuous(breaks = seq(200,800,50), # from min to max by bins
#'                     labels = bins_labels(200,800,50,7,' - ') ) # 7 x labels
#'  
#'  # if necessary use:
#'  theme(axis.text.x = element_text(hjust=))
                                                   

bins_labels <- function(init=0, max, bins, n_labels=7,sep=' -\n'){
  p_breaks <- seq(init,max,bins) # breaks
  p_labels <- paste0(p_breaks,sep,p_breaks+bins) # labels
  width <- round( length(p_labels)/n_labels )
  ind <- 1
  for (i in 2:n_labels ) ind[i] <- ind[i-1]+width
  p_labels[-ind] <- ''
  return(p_labels)
}

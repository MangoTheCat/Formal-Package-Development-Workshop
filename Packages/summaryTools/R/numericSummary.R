#' Generate numeric data summaries
#'
#' @param vec Numeric vector of data to generate data summaries
#' @param na.rm Should missing values be removed?
#' 
#' @return This function will return a named vector containing: 
#' \describe{
#'  \item{Min}{The minimum value of the data}
#'  \item{Mean}{The mean of the data}
#'  \item{Variance}{The variance of the data}
#'  \item{Max}{The maximum value of the data}
#'  \item{Nas}{The number of missing values}
#' }
#' 
#' @examples
#' numericSummary(airquality$Ozone) 
#' 
#' @author Aimee Gott
#' 
#' @export
numericSummary <- function(vec, na.rm = TRUE){
  
  # Check the inputs are of the correct form
  if(!is.numeric(vec)) stop("You must provide a numeric vector", call. = FALSE)
  
  if(!is.logical(na.rm)) stop("na.rm must be a logical (TRUE/FALSE) value", call. = FALSE)
  
  if(length(vec) <= 1) warning(paste("Some summaries may be incorrect as length of input vector is", length(vec)), call. = FALSE)
  
  # get the numeric summaries
  vecSummaries <- statSummary(vec, na.rm = na.rm)
  
  # add the number of missing values
  out <- c(vecSummaries, "NAs" = sum(is.na(vec)))
  
  return(out)
  
}






#' Sample from a dataset
#'
#' This function has been designed to sample from the rows of a two
#' dimensional data set returning all columns of the sampled rows.
#'
#' @param data The matrix or data.frame from which rows are to be
#' sampled.
#' @param size The number of samples to take.
#' @param replace Should values be replaced? By default takes the
#' value TRUE.
#' @param ... Any other parameters to be passed to the sample
#' function
#'
#' @return Returns a dataset of the same type as the input data with \code{size} rows.
#'
#' @author Aimee Gott <agott@@mango-solutions.com>
#'
#' @export
#' @examples
#' sampleFromData(airquality, 100)
#'
sampleFromData <- function(data, size, replace = TRUE, ...){

  if(!is.numeric(size))
    stop("Size must be a numeric integer value")

  lengthData <- nrow(data)

  if(!replace & size > lengthData){
    stop("Cannot sample greater than the data size without
         replacement")
  }

  samples <- sample(seq_len(lengthData), size = size,
                    replace = replace, ...)

  invisible(data[samples, ])

  }

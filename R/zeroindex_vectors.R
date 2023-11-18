# make a vector indexed like in python,
# where 0 means the first position
#' zeroindex
#' Constructor function for 'zeroindex' S3 class
#' @param x
#' a one dimentional vector
#'
#' @return
#' a vector of class zeroindex
#'
#' @export
#'
#' @examples
#' zeroindex(1:3)
zeroindex <- function(x) {

  if (!is.vector(x)) {
   # use better check
  }

  # Create the object with attributes
  # obj <- matrix(x, nrow=2, dimnames = list(c("start","end"),c("x","y")))
  obj <- x

  # Set the class attributes
  class(obj) <- c("zeroindex", class(x))

  # Return the object
  return(obj)

}

# generic functions for zeroindex vectors

#' check class
#' @param x
#' an R object
#' @export
is.zeroindex <- function(x) {
  # Use the inherits function to check if 'x' is of class 'segment'
  inherits(x, "zeroindex")
}

# Methods for accessing elements of a 'zeroindex' vector by position
#' subset
#' @param x
#' object from which to extract element(s) or in which to replace element(s)
#' @param i
#' index
#'
#' @export
`[.zeroindex` <- function(x, i) {
  if (is.numeric(i)) {
    i <- i+1
    result <- NextMethod("[")
  } else {
    result <- NextMethod("[")
  }
  zeroindex(result)
}

#' subset
#' @param x
#' object from which to extract element(s) or in which to replace element(s)
#' @param i
#' index
#'
#' @export
`[[.zeroindex` <- function(x, i) {
  `[.zeroindex`(x, i)
}

#' assignment by index
#' @param x
#' object from which to extract element(s) or in which to replace element(s)
#' @param i
#' index
#'
#' @param value
#' assign to value
#' @export
`[<-.zeroindex` <- function(x, i, value) {
  i <- i + 1
  NextMethod('[<-')
}

#' assignment by index
#' @param x
#' object from which to extract element(s) or in which to replace element(s)
#' @param i
#' index
#'
#' @param value
#' assign to value
#' @export
`[[<-.zeroindex` <- function(x, i, value) {
  NextMethod('[<-')
}


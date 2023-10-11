#' "Not in" binary operator
#'
#' \code{\%nin\%} is a binary operator, which returns a logical vector indicating
#' if there is a match or not for its left operand. A true vector element
#' indicates no match in left operand, false indicates a match.
#'
#' It is the negation of %in% so \code{x \%notin\% y} is equivalent to \code{!(x \%in\% y)}.
#'
#' @param x vector: the values to be matched.
#' @param y vector: the values to be matched against.
#'
#' @return logical vector, indicating if a match was not found or each element
#' of x: thus the values are TRUE or FALSE and never NA.
#'
#' @seealso \code{\link{match}} \code{\link{\%in\%}}
#'
#' @examples
#' c('a','b','c') %notin% c('a','b')
#'
#' @noRd
`%notin%` <- function(x, y) {
  !(x %in% y)
}


#' Check If An Object Is A Natural Number
#'
#' @param x single object to be checked
#' @param incl_zero logical: should "0" included as natural number?
#'     Default = TRUE
#' @return logical
#' @noRd
is.natural <- function(x, incl_zero = TRUE) {
  if(!is.numeric(x))     return(FALSE)
  if(x %% 1 != 0)        return(FALSE)
  if(x < 0)              return(FALSE)
  if(!incl_zero & x < 1) return(FALSE)
  TRUE
}

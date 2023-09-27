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

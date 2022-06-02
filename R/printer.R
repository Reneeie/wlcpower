#' Silly Printer Function
#'
#' @param r what you want in the second column
#' @param x what you want in the first column
#'
#' @return A tibble
#' @export
#'
#' @importFrom tibble as_tibble
#' @importFrom utils head
#' @examples
#' printer(x = rnorm(5), r = rnorm(5))
printer = function(r, x){
  x = as_tibble(x = x, r = r)
  print(head(x))
  return(x)
}

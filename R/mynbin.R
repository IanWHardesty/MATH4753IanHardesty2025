#' Mynbin function
#'
#' @param y # a number
#' @param r # a number
#' @param p # a number
#'
#' @returns # a number
#' @export
#'
#' @examples
#' mynbin(10, 5, 0.4)
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}

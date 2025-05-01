#' My normal curve function
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a upper bound
#'
#' @returns a plot, a shaded plot, and area
#' @export
#'
#' @examples
#' myncurve(3, 1, 1)
#' myncurve(1, 3, 1)
#' myncurve(1, 1, 3)
myncurve = function(mu, sigma, a) {
  curve(dnorm(x, mean=mu, sd=sigma), xlim = c(mu-5*sigma, mu+5*sigma))

  xcurve=seq(mu-5*sigma, a,length=1000)

  ycurve=dnorm(xcurve, mean=mu, sd=sigma)

  polygon(c(mu-5*sigma,xcurve,a),c(0,ycurve,0),col="Red")

  prob=pnorm(a, mean=mu, sd=sigma)
  prob=round(prob,4)
  list(mu = mu, sigma = sigma, prob = prob)
}

#' ntickets function
#'
#' @param N number of seats in flight
#' @param gamma probability place will truly be overbooked
#' @param p probability someone shows up
#'
#' @returns 2 plots and a list
#' @export
#' @examples
#' ntickets(N=200, gamma=0.02, p=0.95)
ntickets <- function(N, gamma, p) {
  discrete <- function(n, N, p) {
    1 - pbinom(N, size=n, prob=p)
  }

  normal <- function(n, N, p) {
    1 - pnorm(N, mean=n*p, sd=sqrt(n*p*(1-p)))
  }

  ndVal <- function(N, gamma, p) {
    nVals <- seq(N, N*2, by=1)
    objectiveFunction <- sapply(nVals, function(n) discrete(n, N, p)-gamma)
    nd <- nVals[which.min(abs(objectiveFunction))]
    return(nd)
  }

  ncVal <- function(N, gamma, p) {
    nVals <- seq(N, N*2, by=1)
    objectiveFunction <- sapply(nVals, function(n) normal(n, N, p)-gamma)
    nc <- nVals[which.min(abs(objectiveFunction))]
    return(nc)
  }

  nd <- ndVal(N, gamma, p)
  nc <- ncVal(N, gamma, p)

  result <- list(nd=nd, nc=nc, N=N, p=p, gamma=gamma)

  nVals <- seq(N, N+(N/10), by=1)

  objectiveD <- sapply(nVals, function(n) discrete(n, N, p)-gamma)

  objectiveN <- sapply(nVals, function(n) normal(n, N, p)-gamma)

  par(mfrow = c(2, 1))

  plot(nVals, objectiveD, type = "l", col = "blue",
       main = "Objective Vs n to find optimal tickets sold", xlab = "n", ylab = "Objective")
  abline(h = 0, col = "red", lty = 1) #adds horizontal line
  abline(v=nd, col="red", lty=1)

  plot(nVals, objectiveN, type = "l", col = "black",
       main = "Objective Vs n to find optimal tickets sold", xlab = "n", ylab = "Objective")
  abline(h = 0, col = "red", lty = 1)
  abline(v=nc, col="red", lty=1)

  return(result)
}

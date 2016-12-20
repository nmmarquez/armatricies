#' Create an AR1 matrix
#'
#' Given values for rho and sigma creates an AR1 matrix
#'
#' @param n number of observations in the AR1 process 
#' @param rho how much each obs should be correlated with the obs before
#' @param sigma how much noise exists in the AR1 process
#'
#' @return Variance-Covrariance matrix of AR1 process
#'
#' @examples
#' AR1(20, .95, 3)
#'
#' @export
AR1 <- function(N, sigma, rho){
  Q <- matrix(0, nrow=N, ncol=N)
  Q[1,1] <- 1 + rho**2
  for(i in 2:N){
    Q[i,i] <- 1 + rho**2
    Q[i-1,i] <- -1 * rho
    Q[i,i-1] <- -1 * rho
  }
  solve((1 / sigma**2) * Q)
}
#' Title
#'
#' @param n number of individuals in each group
#' @param t number of periods
#' @param rho within-individual correlation
#' @param sigma the variance of an individual
#' @param beta treatment effect in the alternative hypothesis
#' @param sign.level alpha
#'
#' @return p
#'
#' @export
#' @importFrom stats pnorm qnorm
#' @examples
#' power.wald.continuous(n=20,t=2,rho=0.95,sigma=0.05,beta=1,sign.level=0.05)
power.wald.continuous <- function(n, t, rho, sigma, beta, sign.level) {
  Var.beta.hat=(2*t-1+rho)*(1-rho)*sigma^2/(n*t*(t-1))
  p = pnorm(abs(beta)/sqrt(Var.beta.hat)-qnorm(1-sign.level/2))+pnorm(-abs(beta)/sqrt(Var.beta.hat)-qnorm(1-sign.level/2))
  return(p)
}

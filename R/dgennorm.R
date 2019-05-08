#' PDF for the generalised gaussian function
#'
#' Calculate the PDF of the generalised normal distribution.
#'
#' The generalised normal distribution (GGD) is a generalisation of the exponential
#' family of functions. It has the form:
#' \deqn{
#' \frac{b}{2 a \Gamma(1/b)}exp[-(x/a)^b]}{
#' b/(2a\Gamma(1/b)) exp[-(x/a)^b]
#' }
#' where x is vector of positive values, a is a scale parameter, and b is a shape
#' parameter. \eqn{\Gamma} indicates the standard Euler Gamma function. The GDD
#' is identical to the normal (with SD=sqrt(a)) and exponential distributions
#' when \eqn{b} is 2 and 1 respectively. The GGD is useful because it allows for
#' modelling leptokurtosis; decreasing values of \eqn{b} indicate fatter tails.
#'
#' @param x: Vector of (positive) deviates from the mean.
#' @param scale: Float giving scale parameter.
#' @param shape: Float giving shape parameter
#'
#' @return `dgennorm` gives a vector of probabilities of the data
#' given shape and scale parameters.
#' @author Tom Ellis
#' @examples
#' # Basic example for when the gen. gaussian matches the common-or-garden gaussian.
#' d  <- rnorm(1000) # simulate a standard normal
#' # throws an error because of negative values
#' d_generalised_gaussian(d, scale = 1, shape =2)
#'
#' # Try again, but make sure values are positive
#' d <- abs(rnorm(1000))
#' # Determine the maximum likelihood value for the scale
#' (in theis case equivalent to the variance of the Gaussian).
#' sc <- seq(1, 3, 0.01) # values to test
#' # Get the likelihood of the data for each value in sc
#' yv <- sapply(sh, function(v) sum(d_generalised_gaussian(d, scale = v, shape =2, log=T)))
#' plot(sh, yv, type='l') # Plot likelihood curve.
#' # ML value is around 1.44, which is approximately sqrt(2)
#' sh[which(yv == max(yv))]
#'
#' @export
dgennorm <- function(x, scale, shape, log=F){
  if(any(x < 0)){
    warning("One or more values in x is less than zero. NaN returned.")
  }
  out <- (shape/(2 * scale * gamma(1.0/shape) )) * exp(-(x/scale)^shape)
  if(log){
    return(log(out))
  } else{
    return(out)
  }
}

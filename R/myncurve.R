#' @title myncurve
#'
#' @param mu
#' @param sigma
#' @param a
#'
#' @return normal curve, probability and shaded area.
#' @export
#'
#' @examples
myncurve = function(mu, sigma, a){
  bottom=mu-3*sigma
  top=mu+ 3*sigma
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(bottom, top))
  xcurve=seq(bottom,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(bottom,xcurve,a),c(0,ycurve,0),col="Red")

  prob=pnorm(a,mean=mu,sd=sigma)-pnorm(bottom,mean=mu,sd=sigma)
  prob=round(prob,4)
  text(a,dnorm(a,mu,sigma), paste0("Area = ", prob,sep=""))

}


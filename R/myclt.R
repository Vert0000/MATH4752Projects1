#' @title myclt
#'
#' @param n
#' @param iter
#' @param a
#' @param b
#'
#' @return Histagram with a curve of the normal random population
#' @export
#'
#' @examples
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,mean)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=(a+b)/2,sd=sqrt((b-a)^2/(n*12))),add=TRUE,lwd=2,col="Blue")
  sm
}

#' @title myci
#'
#' @param x
#'
#' @return 95% confidence interval
#' @export
#'
#' @examples
myci<- function(x){
  std <- sd(x)
  t<-qt(1-0.05/2,n-1)
  mp <- c(-1,1)
  mean(x)+ mp*t*std/sqrt(n)

}

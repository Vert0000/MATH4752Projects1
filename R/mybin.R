#' @title mybin
#'
#' @param iter
#' @param n
#' @param p
#'
#' @return Barplot and table for a binomial simulation
#' @export
#'
#' @examples
mybin=function(iter=100,n=10, p=0.5){

  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)

  succ=c()
  for( i in 1:iter){

    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))

    succ[i]=sum(sam.mat[,i])
  }

  succ.tab=table(factor(succ,levels = 0:n))

  iter.lab =paste0("iter =", iter)
  n.lab = paste0("n = ", n)
  p.lab = paste0("p =", p)
  lab= paste(iter.lab,p.lab,sep = ", ")


  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}

#' Basic Newton Raphson
#'
#' @param x0 initial value
#' @param delta increment in x
#' @param f the function to find the derivative of
#' @param fdash the first derivative
#'
#' @return a plot with the segments of the NR technique
#' @export
#'
#' @examples
#' \dontrun{mynewt(10,f=function(x) x^2-5*x +6, fdash= function(x) 2*x -5)}
mynewt=function(x0,delta=0.001,f,fdash){
  d=1000
  i=0
  x=c()
  y=c()
  x[1]=x0
  y[1]=f(x[1])
  while(d > delta & i<10000){
    i=i+1
    x[i+1]=x[i]-f(x[i])/fdash(x[i])
    y[i+1]=f(x[i+1])
    d=abs(y[i])
  }
  #windows()
  curve(f(x),xlim=range(c(range(x),-range(x))),xaxt="n", main="Newton-Raphson Algorithm")
  points(x,y,col="Red",pch=19,cex=1.5)
  axis(1,x,round(x,2),las=2)
  abline(h=0,col="Red")

  segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=2)
  segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Pink")

  list(x=x,y=y)
}

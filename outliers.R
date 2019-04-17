xx <- c(1.15, -52.99, 0.19, -2.89, -2.3, 0.45, 0.35, -0.62, -8.55, 0.56, 0.86, -2.48, -1.64, 1.18, 0.55, 1.78, 1.83, -2.44, 1.31, 0.81, -0.44, 0.7, 5.23, -0.08, 0.1, 4.26, 0.25, 1.9, -6.1, -9.68)


outliers.rm <- function(x)
{
  q <- quantile(x, probs = c(0.25, 0.75))
  q <- unname(q)
  for (i in x) 
{
    if ( i < q[1] - 1.5*IQR(x) || 
         i > q[2] + 1.5*IQR(x)) { 
      x <- x[x != i]
      return(x)
    }
}
}
outliers.rm(xx)


###tmp

outliers.rm <- function(x){
  mod_x <<- c()
  for(i in 1:length(x)){
    q1 <- quantile(x,probs=c(0.75))
    q2 <- quantile(x,probs=c(0.25))
    if (x[i]<(q1+1.5*IQR(x)) & x[i]>(q2-1.5*IQR(x))) {
      mod_x <<- append(mod_x, x[i])
    } 
  }
  return(mod_x)
}
outliers.rm(xx)

###

q <- quantile(xx, probs = c(0.25, 0.75))
q <- unname(q)
q[1] - 1.5*IQR(xx) 
q[2] + 1.5*IQR(xx)

set.seed(1337)
x <- runif(1e6, min = -1, max = 1)

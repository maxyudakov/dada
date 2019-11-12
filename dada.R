Yudakov


library(dplyr)
library(pbapply)
generate_data <- function(alpha){

x1 <- rnorm(100, 0, 1)
x2 <- rnorm(100, 5, 1)
x3 <- rnorm(100, 20, 1)

y <- x1 + x2 + x3*alpha

data <- data.frame(y=y, x1=x1, x2=x2, x3=x3)
return(data)
}

estimate <- function(data){
  lm (y ~ x1 + x2 +x3, data=data)$coefficients
}



iteration <- function(alpha) {
  data <- generate_data(alpha)
  est <- estimate(data)
  return(alpha-est[4])
}

simulation <- function(n, alpha) {
  results <- c()
  for (x in 1:n) {results <- c(results, iteration())}
  return(mean(results))
}

simulation

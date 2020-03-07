max.lik <- function(x, distribution = "norm") {
  
  if (!is.element(distribution, c("norm", "pois", "exp", "binom"))) {
    stop("Distribution not found.")
  }
  
  else {
    
    if (distribution == "binom") {
      l <- function(p) {
        return(-sum(dbinom(x, 1, p, log = TRUE)))
      }
      
      return(optimize(l, 0:1)$minimum)
    }
    
    else if (distribution == "pois") {
      n <- mean(x) + 2
      l <- function(lambda) {
        return(-sum(dpois(x, lambda, log = TRUE)))
      }
      
      if (max(x) == 1) {
        return(optimize(l, 0:n)$minimum) * 100
      } else {
        return(optimize(l, 0:n)$minimum)
      }
      
    }
    
    else if (distribution == "exp") {
      l <- function(lambda) {
        return(-sum(dexp(x, lambda, log = TRUE)))
      }
      
      lambda <- 1 / mean(x)
      return(optimize(l, lambda - 1:lambda + 1)$minimum)
    }
    
    else if (distribution == "norm") {
      l <- function(mu, sigma) {
        return(sum(dnorm(x, mu, sigma, log = TRUE)))
      }
      
      ll <- function(x) {
        return(-l(x[1], x[2]))
      }
      
      mu <- mean(x)
      return(optim(c(mu, mu), ll)$par)
    }
  }
}
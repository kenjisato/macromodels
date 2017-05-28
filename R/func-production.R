
# Aggregate Production Functions ------------------------------------------

cobb_douglas <- function(alpha, beta = NULL,
                         type = c()) {
  beta <- beta %|% 1 - alpha
  production <- function(A, K, L) {
    A * K^alpha * L^(1 - alpha)
  }
  production
}



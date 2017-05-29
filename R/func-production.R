
# Aggregate Production Functions ------------------------------------------

cobb_douglas <- function(alpha, beta = NULL) {
  beta <- beta %|% 1 - alpha
  production <- function(K, L) {
    K^alpha * L^(1 - alpha)
  }
  production
}


ces <- function(alpha, sigma) {
  gamma <- ((sigma - 1) / sigma)
  beta <- 1 - alpha

  production <- function(K, L) {
    (alpha * K ^ gamma + beta * L ^ gamma) ^ (1 / gamma)
  }
  production
}


tp_hicks <- function(production) {
  function(K, L, A = 1) {
    A * production(K, L)
  }
}

tp_harrod <- function(production) {
  function(K, L, A = 1) {
    production(K, A * L)
  }
}

tp_solow <- function(production) {
  function(K, L, A = 1) {
    production(K, A * L)
  }
}



# Intensive Form ----------------------------------------------------------

cobb_douglas1 <- function(k) {

}

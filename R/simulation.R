
#' Simulate State Space Model
#'
#' x_{t+1} = h(x_t)
#' y_{t} = g(x_t)
#'
#' @param h function for state equation
#' @param g function for output equation
#' @param x0 initial state vector
#' @param t integer, length of simulation
#'
#' @return output matrix
#' @export
#'
run_simulation <- function(h, g = NULL, x0, t) {
  if (is.null(g)) {
    run_sans_g(h, x0, t)
  } else {
    run_with_g(h, g, x0, t)
  }
}


run_with_g <- function(h, g, x0, t) {
  # Predetermined variables
  n1 <- length(x0)
  pre <- 1:n1

  # Non-predetermined variables
  n2 <- length(g(x0))
  npr <- (n1 + 1):(n1 + n2)

  # Initial conditions
  out <- matrix(0, t, n1 + n2)
  out[1, pre] <- x0

  # Populate simulation output
  out[1, npr] <- g(x0)
  for (i in 1:(t - 1)) {
    out[i + 1, pre] <- h(out[i, pre])
    out[i + 1, npr] <- g(out[i + 1, pre])
  }
  out
}


run_sans_g <- function(h, x0, t) {
  n <- length(x0)
  out <- matrix(0, t, n)
  out[1, ] <- x0

  for (i in 1:(t - 1)) {
    out[i + 1, ] <- h(out[i, ])
  }
  out
}


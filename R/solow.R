
solow_model <- function(pfunc, s, delta, g, n, guess = 10) {
  rhs <- function(k) {
    s * pfunc(k) - (delta + g + n) * k
  }
  update <- function(k, dt = 0.01){
    k + dt * rhs(k)
  }
  sol <- nleqslv::nleqslv(guess, rhs)
  steady_state <- sol$x

  invisible(list(update = update, steady_state = steady_state))
}

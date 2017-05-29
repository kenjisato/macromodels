
model_solow <- function(f, g = 0, n = 0, delta = 0) {
  f(x) - (g + n + delta) * x
}

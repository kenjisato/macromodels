
utility_log <- log

utility_crra <- function(theta) {
  function(c) (c ^ (1- theta) - 1) / (1 - theta)
}

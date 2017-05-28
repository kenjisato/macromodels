
print.datasource <- function(x, ...) {

  header <- function(s, ...) {
    cat(s, "\n", ...)
    cat(rep("-", nchar(s)), sep = "", ...)
    cat("\n")
  }

  header(x$name)

  cat("URL: ", x$url, "\n\n")

  cat("To cite this data, make the following reference: \n\n")
  cat(x$cite, fill = TRUE)
}

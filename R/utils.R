is_named <- function(x) {
  nm <- names(x)
  !is.null(nm) && !any(is.na(nm) | nm == "")
}

is_blank <- function(x) x == ""

all_functions <- function(x) {
  all(sapply(x, is.function))
}


last <- function(x) x[length(x)]


default_abort = "The script aborted! Don't worry."
escape_words <- c("!", "?")


#' Prompt the user whether the script should continue.
#' @param abort_message Character(1) printed when the script aborts.
#' @return True or Error (script abort).
#' @export
should_continue <- function(abort_message = default_abort) {
  cat("Continue? (y/n)\n")
  connection <- if(interactive()) stdin() else "stdin"
  res <- readLines(con = connection, n = 1)
  if(grepl("[Yy](es)?", res)) return(invisible(TRUE))
  if(grepl("[Nn]o?", res)) stop(abort_message, call. = FALSE)
  cat("\nChoice", paste0("(", res, ")"), "not recognized. Please confirm:\n")
  should_continue()
}


make_selection <- function(abbr, abort_message = default_abort) {
  cat("\nPlease make a selection:\n")
  connection <- if(interactive()) stdin() else "stdin"
  res <- readLines(con = connection, n = 1)
  if(res %in% abbr) return(res)
  if(res %in% escape_words) stop(abort_message, call. = FALSE)
  cat("\nSelection", paste0("(", res, ")"), "not recognized...\n")
  make_selection(abbr)
}


show_menu <- function(name, abbr = abbreviate(tolower(name), minlength = 1),
                         max.col = 5, max.width = options()$width) {
  wrd = paste(name, paste0("[", abbr, "]"))
  n = length(wrd)
  cnt = nchar(wrd)
  i = 1
  cat("Available Choices:\n")
  while(i <= n) {
    k = min(n, max.col, last(which(cumsum(cnt[i:n]) < max.width)))
    z = (i + k - 1)
    batch = i:z
    cat(paste(wrd[batch], collapse = "\t"), "\n")
    i <- z + 1
  }
  return(invisible())
}


#' Print an options menu.
#' @param choices named list of k functions.
#' @param abbr Character(k) list of abbreviations.
#' @return value of whatever function matches.
#' @export
options_menu <- function(choices, abbr = abbreviate(tolower(nm), minlength = 1), ...) {
  nm = names(choices)
  stopifnot(all_functions(choices))
  stopifnot(length(nm) == length(abbr))
  stopifnot(!any(is_blank(abbr)))
  show_menu(nm, abbr, ...)
  sel <- make_selection(abbr)
  names(choices) <- abbr
  choices[[sel]]()
}

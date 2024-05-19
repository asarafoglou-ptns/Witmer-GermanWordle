# Week 2 functions

#' @export
source("Wordlist.R")

#' @export
target <- sample(german_words, 1)

#' @export
compare_words <- function(target_str, guess_str) {
  if (nchar(target_str) != nchar(guess_str)) {
    stop("target and guess string must be the same length.")
  }
  
  target <- strsplit(target_str, "")[[1]]
  guess <- strsplit(guess_str, "")[[1]]
  result <- character(nchar(guess_str))
  
  for (i in seq_along(target)) {
    if (guess[i] == target[i]) {
      result[i] <- "correct"
    } else if (guess[i] %in% target) {
      result[i] <- "in-word"
    } else {
      result[i] <- "not-in-word"
    }
  }
  
  result
}



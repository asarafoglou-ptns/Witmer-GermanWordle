# Week 2 functions

#' @export
source("Wordlist.R")

#' @export
target <- sample(german_words, 1)

#' @title Compare Words
#' @description This function compares whether the letters in the target word are the same as in the guessed word
#' @param target_str character. Should be a word with the same length as the guessed word
#' @param guess_str character. Should be a word with the same length as the target word
#' @return Shows whether the letter is in the word and on the correct place, in the word but incorrect place, or not in the word
#' @examples
#' # compare_words("haus","maus")
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



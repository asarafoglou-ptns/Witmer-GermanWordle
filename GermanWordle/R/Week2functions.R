# Week 2 functions

#' @export
source("Wordlist.R")

#' @export
target <- sample(german_words, 1)

#' @title Compare Words
#' @description This function compares whether the letters in the target word are the same as in the guessed word
#' @param target_str character. Should be a word with the same length as the guessed word
#' @param guess_str character. Should be a word with the same length as the target word
#' 
#' @return character vector. Shows whether the letter is in the word and on the correct place, in the word but incorrect place, or not in the word
#' @examples
#' # Compare the guessed word with the target word
#' compare_words("hallo")
#' @export
compare_words <- function(guess_str) {
  # Convert the target word from a list to a character string
  target_str <- target[[1]]
  
  # Check if the guessed word is in the german_words list and has lowercase letters
  if (!guess_str %in% german_words) {
    return("Please use an existing German word in lowercase letters")
  }
  
  # Check if target and guess strings have the same length
  if (nchar(target_str) != nchar(guess_str)) {
    stop("target and guess string must be the same length.")
  }
  
  # Split the target and guess strings into individual characters
  target <- strsplit(target_str, "")[[1]]
  guess <- strsplit(guess_str, "")[[1]]
  
  # Initialize a character vector to store the result for each character in the guess
  result <- character(nchar(guess_str))
  
  for (i in seq_along(target)) {
    # If the characters at the same position in target and guess are the same
    if (guess[i] == target[i]) {
      result[i] <- "correct"         # Mark as "correct"
    } else if (guess[i] %in% target) {
      result[i] <- "in-word"         # If the guessed character is in the target word but in a different position
    } else {
      result[i] <- "not-in-word"     # If the guessed character is not in the target word at all
    }
  }
  
  result
}



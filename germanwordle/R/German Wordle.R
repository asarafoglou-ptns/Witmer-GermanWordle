
## German Word Guessing Game

# Load necessary libraries
# library(shiny)
# library(htmltools)

# Use wordlist from an external file
#' @export
source("Wordlist.R")

#' @title German Wordle
#' @description This is a function for playing the popular online-game Wordle in German in a Shiny App.
#' @param / No input needed.
#' @return A Shiny app interface for playing the German Word Guessing Game.
#' @examples
#' playwordle()
#' @export
playwordle <- function(){
# Define the UI for the Shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4),
  title = "German Wordle",
  tags$style(HTML("
  .container-fluid {
      text-align: center;
      height: calc(100vh - 30px);
      display: grid;
      grid-template-rows: 1fr auto;
  }
  .attempts {
      overflow-y: auto;
      height: 100%;
  }
  .attempts.finished {
      overflow-y: visible;
  }
  .attempts .word {
      margin: 5px;
  }
  .attempts .word > .letter {
      display: inline-block;
      width: 50px;
      height: 50px;
      text-align: center;
      vertical-align: middle;
      border-radius: 3px;
      line-height: 50px;
      font-size: 32px;
      font-weight: bold;
      vertical-align: middle;
      user-select: none;
      color: white;
      font-family: 'Clear Sans', 'Helvetica Neue', Arial, sans-serif;
  }
  .attempts .word > .correct {
      background-color: #6a5;
  }
  .attempts .word > .in-word {
      background-color: #db5;
  }
  .attempts .word > .not-in-word {
      background-color: #888;
  }
  .attempts .word > .guess {
      color: black;
      background-color: white;
      border: 1px solid black;
  }
  .keyboard {
      height: 240px;
      user-select: none;
  }
  .keyboard .keyboard-row {
      margin: 3px;
  }
  .keyboard .keyboard-row .key {
      display: inline-block;
      padding: 0;
      width: 30px;
      height: 50px;
      text-align: center;
      vertical-align: middle;
      border-radius: 3px;
      line-height: 50px;
      font-size: 18px;
      font-weight: bold;
      vertical-align: middle;
      color: black;
      font-family: 'Clear Sans', 'Helvetica Neue', Arial, sans-serif;
      background-color: #ddd;
      touch-action: none;
  }
  .keyboard .keyboard-row .key:focus {
      outline: none;
  }
  .keyboard .keyboard-row .key.wide-key {
      font-size: 15px;
      width: 50px;
  }
  .keyboard .keyboard-row .key.correct {
      background-color: #6a5;
      color: white;
  }
  .keyboard .keyboard-row .key.in-word {
      background-color: #db5;
      color: white;
  }
  .keyboard .keyboard-row .key.not-in-word {
      background-color: #888;
      color: white;
  }
  .endgame-content {
      font-family: Helvetica, Arial, sans-serif;
      display: inline-block;
      line-height: 1.4;
      letter-spacing: .2em;
      margin: 20px 8px;
      width: fit-content;
      padding: 20px;
      border-radius: 5px;
      box-shadow: 4px 4px 19px rgb(0 0 0 / 17%);
  }
  .title {
      font-size: 4em; /* Increase the font size of the title */
      margin-bottom: 20px; /* Space between title and description */
  }
  .description {
      margin-bottom: 20px; /* Space between description paragraphs */
  }
")),
  div(
    class = "attempts",
    h1("German Wordle", class = "title"),  # Change h3 to h1 for larger font size
    p("Guess the Word in 6 attempts.", class = "description"),
    p("Each guess must be a valid German 5-letter word.", class = "description"),
    p("The color of the tiles will change to show how close your guess was to the word.", class = "description"),
    uiOutput("prior_attempts"),
    uiOutput("current_attempt"),
    uiOutput("end_game"),
    uiOutput("new_game_ui")
  ),
  uiOutput("keyboard"),
  tags$script(HTML("
    const letters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
                     'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'];
    const all_key_ids = [ ...letters, 'Enter', 'Back'];
    document.addEventListener('keydown', function(e) {
      let key = e.code.replace(/^Key/, '');
      if (letters.includes(key)) {
        document.getElementById(key).click();
      } else if (key == 'Enter') {
        document.getElementById('Enter').click();
      } else if (key == 'Backspace') {
        document.getElementById('Back').click();
      }
    });

    let in_button_touch = false;
    document.addEventListener('touchstart', function(e) {
        if (all_key_ids.includes(e.target.id)) {
            e.target.click();
            e.target.style.pointerEvents = 'none';
            e.preventDefault();   // Disable text selection
            in_button_touch = true;
        }
    });
    document.addEventListener('touchend', function(e) {
        all_key_ids.map((id) => {
            document.getElementById(id).style.pointerEvents = null;
        });
        if (in_button_touch) {
            if (all_key_ids.includes(e.target.id)) {
                e.preventDefault();
            }
            in_button_touch = false;
        }
    });
  "))
)

# Define the server logic for the Shiny app
server <- function(input, output) {
  # Reactive value to store the target word
  target_word <- reactiveVal(sample(german_words, 1))
  # Reactive value to store all guesses
  all_attempts <- reactiveVal(list())
  # Reactive value to indicate if the game is finished
  finished <- reactiveVal(FALSE)
  # Reactive value to store current attempt letters
  current_attempt_letters <- reactiveVal(character(0))
  
  # Function to reset the game
  restart_game <- function() {
    target_word(sample(german_words, 1))
    all_attempts(list())
    finished(FALSE)
  }
  
  # Use enter key to "submit" the word
  observeEvent(input$Enter, {
    if (finished()) return()
    
    attempt <- paste(current_attempt_letters(), collapse = "")
    
    if (!attempt %in% german_words) {
      showModal(modalDialog(
        title = "Invalid Input",
        "This is not a valid 5-letter German word. Please try again.",
        footer = tagList(
          actionButton("close_modal", "Close", class = "btn-primary")
        )
      ))
      return()
    }
    
    all_attempts_new <- all_attempts()
    
    check_result <- evaluate_attempt(attempt, target_word())
    all_attempts_new[[length(all_attempts_new) + 1]] <- check_result
    all_attempts(all_attempts_new)
    
    if (isTRUE(check_result$win) || length(all_attempts_new) >= 6) {
      finished(TRUE)
    }
    
    current_attempt_letters(character(0))
    
    # Check if the game is over and provide feedback
    if (isTRUE(check_result$win)) {
      showModal(modalDialog(
        title = "Congratulations!",
        paste("You've guessed the word correctly within 6 attempts! The word was:", toupper(attempt)),
        footer = tagList(
          actionButton("close_modal", "Close", class = "btn-primary")
        )
      ))
    } else if (length(all_attempts_new) >= 6) {
      showModal(modalDialog(
        title = "Game Over",
        paste("Next time! The correct word was:", toupper(target_word())),
        footer = tagList(
          actionButton("close_modal", "Close", class = "btn-primary")
        )
      ))
    }
  })
  
  # Close the modal dialog when the close button is clicked
  observeEvent(input$close_modal, {
    removeModal()
  })
  
  # Render the UI for previous attempts
  output$prior_attempts <- renderUI({
    res <- lapply(all_attempts(), function(attempt) {
      letters <- attempt$letters
      row <- mapply(
        letters,
        attempt$matches,
        FUN = function(letter, match) {
          match_type <- match
          div(toupper(letter), class = paste("letter", match_type))
        },
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
      div(class = "word", row)
    })
    
    scroll_js <- "
        document.querySelector('.attempts')
          .scrollTo(0, document.querySelector('.attempts').scrollHeight);
    "
    tagList(res, tags$script(HTML(scroll_js)))
  })
  
  # Render the UI for the current attempt
  output$current_attempt <- renderUI({
    if (finished()) return()
    
    letters <- current_attempt_letters()
    
    target_length <- isolate(nchar(target_word()))
    if (length(letters) < target_length) {
      letters[(length(letters)+1) : target_length] <- ""
    }
    
    div(
      class = "word",
      lapply(letters, function(letter) {
        div(toupper(letter), class ="letter guess")
      })
    )
  })
  
  # Render the UI for the new game button
  output$new_game_ui <- renderUI({
    if (!finished())
      return()
    
    actionButton("restart_game", "New Game")
  })
  
  # Observe the new game button event to restart the game
  observeEvent(input$restart_game, {
    restart_game()
  })
  
  # Reactive expression to track used letters and their match types
  used_letters <- reactive({
    letter_matches <- list()
    
    lapply(all_attempts(), function(attempt) {
      mapply(attempt$letters, attempt$matches, SIMPLIFY = FALSE, USE.NAMES = FALSE,
             FUN = function(letter, match) {
               prev_match <- letter_matches[[letter]]
               if (is.null(prev_match)) {
                 letter_matches[[letter]] <<- match
               } else {
                 if (match == "correct" && prev_match %in% c("not-in-word", "in-word")) {
                   letter_matches[[letter]] <<- match
                 } else if (match == "in-word" && prev_match == "not-in-word") {
                   letter_matches[[letter]] <<- match
                 }
               }
             }
      )
    })
    
    letter_matches
  })
  
  # Define the keyboard layout
  keys <- list(
    c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P"),
    c("A", "S", "D", "F", "G", "H", "J", "K", "L"),
    c("Enter", "Z", "X", "C", "V", "B", "N", "M", "Back")
  )
  
  # Render the keyboard UI
  output$keyboard <- renderUI({
    prev_match_type <- used_letters()
    keyboard <- lapply(keys, function(row) {
      row_keys <- lapply(row, function(key) {
        class <- "key"
        key_lower <- tolower(key)
        if (!is.null(prev_match_type[[key_lower]])) {
          class <- c(class, prev_match_type[[key_lower]])
        }
        if (key %in% c("Enter", "Back")) {
          class <- c(class, "wide-key")
        }
        actionButton(key, key, class = class)
      })
      div(class = "keyboard-row", row_keys)
    })
    
    div(class = "keyboard", keyboard)
  })
  
  # Observe keyboard key presses and update the current attempt
  lapply(unlist(keys, recursive = FALSE), function(key) {
    if (key %in% c("Enter", "Back")) return()
    observeEvent(input[[key]], {
      if (finished())
        return()
      cur <- current_attempt_letters()
      if (length(cur) >= 5)
        return()
      current_attempt_letters(c(cur, tolower(key)))
    })
  })
  
  # Observe the Back key event to remove the last letter from the current attempt
  observeEvent(input$Back, {
    if (length(current_attempt_letters()) > 0) {
      current_attempt_letters(current_attempt_letters()[-length(current_attempt_letters())])
    }
  })
  
  # Render the UI for the endgame message
  output$game_over <- renderUI({
    if (!finished())
      return()
    
    if (all_attempts()[[length(all_attempts())]]$win) {
      return(
        div(
          class = "endgame-content",
          h4("Congratulations! You've guessed the word correctly within 6 attempts!"),
          h4(paste("The correct word was:", toupper(target_word())))
        )
      )
    } else {
      return(
        div(
          class = "endgame-content",
          h4(paste("Next time! The correct word was:", toupper(target_word())))
        )
      )
    }
  })
}

# Function to evaluate the attempt against the target word and return the result
evaluate_attempt <- function(attempt_str, target_str) {
  attempt <- strsplit(attempt_str, "")[[1]]
  target <- strsplit(target_str, "")[[1]]
  remaining <- character(0)
  
  if (length(attempt) != length(target)) {
    stop("Word lengths don't match.")
  }
  
  result <- rep("not-in-word", length(attempt))
  
  # First pass: check for correct letters in the correct position
  for (i in seq_along(attempt)) {
    if (attempt[i] == target[i]) {
      result[i] <- "correct"
    } else {
      remaining <- c(remaining, target[i])
    }
  }
  
  # Second pass: check for correct letters in the wrong position
  for (i in seq_along(attempt)) {
    if (attempt[i] != target[i] && attempt[i] %in% remaining) {
      result[i] <- "in-word"
      remaining <- remaining[-match(attempt[i], remaining)]
    }
  }
  
  list(
    word = attempt_str,
    letters = attempt,
    matches = result,
    win = all(result == "correct")
  )
}

# Run the Shiny app
shinyApp(ui, server)

}


# Shawn Chang
# Assignment 3

# 1. Loading and sampling from the dictionary ####
dictionary <- read.table("dictionary.txt", stringsAsFactors = FALSE)  # this is needed to read and load the dictionary (from which the secret keyword is selected)
keyword <- sample(dictionary$V1, 1)                                   # this is needed to sample a random word from the dictionary

# 2. Prompting the player to start the game ####
print(paste("Welcome to Hangman! This word has", nchar(keyword), "letters. Type 1 letter to begin your guess. You are allowed 10 guesses."))
                      # nchar() is needed to determine and display the number of characters of the word
                      # this print() function is needed so instructions and rule are displayed as a message

# 3. Setting the conditions for when the game starts/restarts ####
i <- 10               # this is an arbitrary value to concisely represent the number of tries left (10 at the beginning of the game)

right_guesses <- c()   # this creates a new vector, allowing us to later update the repertoire of guesses the player has already made with each new guess
wrong_guesses <- c()

# 4. Creating the loop for the game to take place continuously until the player wins or loses ####
while(i>0){           # setting i as greater than 0; this way, the reader will stay in the game (from being prompted to enter a letter to winning or losing) until they run out of tries (i equalling 0 means 0 tries left)
  
  repeat{
    guess <- readline(prompt = "Please enter a letter: ")   # this is needed to prompt the player to enter a letter (denoted by "guess")
    if ((grepl("[A-Za-z]", guess)) &                        # this is needed to validate if the input is a letter from a to z (upper and lower case okay)
        (nchar(guess) == 1)){                               # this is needed to check that the input is exactly 1 letter
      break                                                 # this is needed to let the player exit the repeated prompt (i.e., to enter a letter) only when they've met the above 2 conditions
    }
  }
  
  guess <- as.character(guess)                              # this is needed to ensure the input as a character so it matches the letters in the keyword (also characters)
  
  guess_nocaps <- tolower(guess)                            # this is needed to set the inputted letter in the lower case so it matches those in the secret keywords in the dictionary
  
  keyword <- as.character(keyword)                          # this is needed to ensure the secret keyword sampled from the dictionary also comprises characters
  
  keyword_letters <- unlist(strsplit(keyword, ""))          # this is needed to split the keyword into its constituent letters so we can check if the inputted letter is one from in the secret keyword

  right_guesses <- append(right_guesses, guess_nocaps[guess_nocaps %in% keyword_letters])
  wrong_guesses <- append(wrong_guesses, guess_nocaps[!guess_nocaps %in% keyword_letters])
  
  # this is needed to update the vector of guessed letters each time the player makes a new guess (correct or incorrect) so we can later check if they guessed all the letters in the keyword (and therefore win)
  
  if(guess_nocaps %in% keyword_letters){
    print(paste("Correct.", guess_nocaps, "is in spot", which(guess_nocaps == keyword_letters), "of the secret word. Great work. You have", i, "tries left."))
    print(paste("The correct letters you already used include:"))
    print(right_guesses)
    if(!i==10){
      print(paste("The incorrect letters you already used include:"))
      print(wrong_guesses)
    }
    } else {
    i <- i-1
    print(paste(guess_nocaps, "is not in the secret word. You have", i, "tries left."))
    print(paste("The incorrect letters you already used include:"))
    print(wrong_guesses)
    if(length(right_guesses) != 0){
      print(paste("The correct letters you already used include:"))
      print(right_guesses)
    }
  }
  # the above if-else block is to check if the guessed letter is in the secret keyword
  # if it is, we want to a) tell the player they guessed correctly and b) use the which() function to tell them the spot (through indexing) in the keyword that the letter is in (e.g., "h" is in spot 2 of the keyword "shield") 
  # if it is not, we want to tell the player they did not guess correctly and deduct a try each time they input a letter not found in the keyword

  
  if(all(keyword_letters %in% right_guesses)){
    print(paste("You won!", keyword, "is indeed the secret word!"))
    break
  }
  # the above if block is needed to check if the player has guessed all the letters in the secret keyword, and if so, instantly take them out of the loop, telling them they won, and display the "Play again" message, bypassing the below if-else
  
  if(i == 0){
    print(paste0("You lost. The secret word was ", keyword, "."))
  }
  # the above if block is needed for the case where the player enters too many letters not in the secret keyword (in other words, i is 0 since i means the number of tries left) and so loses the game
  # paste0() is chosen as we don't want a space between keyword and the period in the displayed message
} # the while loop ends here as we only want the components inside the loop to repeat (and only display the next and final message when the player has won or lost)
print("Play again.") # this is displayed to prompt the reader to play another round regardless of whether they win or lose
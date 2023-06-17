# Shawn Chang
# Assignment 3

dictionary <- read.table("dictionary.txt", stringsAsFactors = FALSE)
keyword <- sample(dictionary$V1, 1)

print(paste("Welcome to Hangman! This word has", nchar(keyword), "letters. Type 1 letter to begin your guess."))

repeat{
  guess <- readline(prompt = "Please enter a letter: ")
  if ((grepl("[A-Za-z]", guess)) &
      (nchar(guess) == 1)){
    break
  } else {
    print("Invalid input. Please ensure what you type is exactly one letter.")
  }
}


# Shawn Chang
# Assignment 3

dictionary <- read.table("dictionary.txt", stringsAsFactors = FALSE)
keyword <- sample(dictionary$V1, 1)

guessmaker <- function(){repeat{
  guess <- readline(prompt = "Please enter a letter: ")
  if ((grepl("[A-Za-z]", guess)) &
      (nchar(guess) == 1)){
    guess_nocaps <- tolower(guess)
    break
  } else {
    print("Invalid input. Please ensure what you type is exactly one letter.")
  }
}
}

print(paste("Welcome to Hangman! This word has", nchar(keyword), "letters. Type 1 letter to begin your guess."))

guessmaker()


tries <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

i <- 10

while(tries[i] < 11){
  
  letters <- unlist(strsplit(keyword, ""))
  
  if(guess_nocaps %in% letters){
    print("Great. Guess the next letter.")
    guessmaker()
  } else {
    tries <- tries - 1
    print(paste("Not that one. Please try another letter. You have", remaining_tries, "tries."))
    guessmaker()
  }
}

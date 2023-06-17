# Shawn Chang
# Assignment 3

dictionary <- read.table("dictionary.txt", stringsAsFactors = FALSE)
keyword <- sample(dictionary$V1, 1)

print(paste("Welcome to Hangman! This word has", nchar(keyword), "letters. Type 1 letter to begin your guess."))

# creating a functions

guessmaker <- function(){repeat{
  guess <- readline(prompt = "Please enter a letter: ")
  if ((grepl("[A-Za-z]", guess)) &
      (nchar(guess) == 1)){
    break
  }
}
}

# first guess
guessmaker()

# check if the guess is a letter in the keyword

as.character(guess)

guess_nocaps <- tolower(guess)

keyword <- as.character(keyword)

letters <- unlist(strsplit(keyword, ""))

i <- 5

if(guess_nocaps %in% letters){
    print(paste(guess_nocaps, "is in the secret word. Great work."))
  guessmaker()
} else {
  print(paste("Not that one. Please try another. You have", i, "tries."))
  i <- i - 1
  guessmaker()
}

# repeat{
# tries <- c(1, 2, 3, 4, 5)
# 



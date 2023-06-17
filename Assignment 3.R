# Shawn Chang
# Assignment 3

dictionary <- read.table("dictionary.txt", stringsAsFactors = FALSE)
keyword <- sample(dictionary$V1, 1)

print(paste("Welcome to Hangman! This word has", nchar(keyword), "letters. Type 1 letter to begin your guess."))

# first guess

repeat{
  guess <- readline(prompt = "Please enter a letter: ")
  if ((grepl("[A-Za-z]", guess)) &
      (nchar(guess) == 1)){
    break
  }
}


# check if the first guess is a letter in the keyword

guess <- as.character(guess)

guess_nocaps <- tolower(guess)

keyword <- as.character(keyword)

letters <- unlist(strsplit(keyword, ""))
  
i <- 5

if(guess_nocaps %in% letters){
    print(paste(guess_nocaps, "is in the secret word. Great work.")) # can specify the exact location using which()
  
  repeat{
    guess <- readline(prompt = "Please enter a letter: ")
    if ((grepl("[A-Za-z]", guess)) &
        (nchar(guess) == 1)){
      break
    }
  }
  
} else {
  print(paste("Not that one. Please try another. You have", i, "tries."))
  i <- i - 1
  
  repeat{
    guess <- readline(prompt = "Please enter a letter: ")
    if ((grepl("[A-Za-z]", guess)) &
        (nchar(guess) == 1)){
      break
    }
  }
  
}


# repeat{
# tries <- c(1, 2, 3, 4, 5)
# 



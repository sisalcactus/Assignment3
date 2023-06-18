# Shawn Chang
# Assignment 3

dictionary <- read.table("dictionary.txt", stringsAsFactors = FALSE)
keyword <- sample(dictionary$V1, 1)


dictionary <- read.table("dictionary.txt", stringsAsFactors = FALSE)
keyword <- sample(dictionary$V1, 1)

print(paste("Welcome to Hangman! This word has", nchar(keyword), "letters. Type 1 letter to begin your guess. You are allowed 10 guesses."))

i <- 10

past_guesses <- c()

# first guess
while(i>0){
  
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
  
  if(guess_nocaps %in% letters){
    print(paste(guess_nocaps, "is in spot", which(guess_nocaps == letters), "of the secret word. Great work."))
    
  } else {
    i <- i-1
    print(paste(guess_nocaps, "is not in the secret word. You have", i, "tries left."))
  }
  
  past_guesses <- append(past_guesses, guess_nocaps)
  
  if(all(letters %in% past_guesses)){
    print(paste("You won!", keyword, "is indeed the secret word!"))
    break
  }
  
  if(i == 0){
    print(paste("You lost. The secret word was", keyword, "."))
  }
}
print("Play again.")
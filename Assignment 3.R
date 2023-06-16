# Shawn Chang
# Assignment 3

dictionary <- read.table("dictionary.txt", stringsAsFactors = FALSE)
keyword <- sample(dictionary$V1, 1)
print(nchar(keyword))

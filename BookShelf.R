
# My first R program
print("Welcome to BookShelf!")
books <- read.csv("books.csv")
print(books)
#check for missing authors
#is.na(books$author)
#print(is.na(books$author))
check_missing <-function (column) {
  missing_values <-is.na(column) | trimws(column) == ""
   return(missing_values)
}

# My first R program
print("Welcome to BookShelf!")
books <- read.csv("books.csv", na.strings = c("", "NA"))
print(books)
#check for missing authors
#is.na(books$author)
#print(is.na(books$author))
check_missing <- function(column) {
  missing_values <- is.na(column) | trimws(column) == ""
  return(missing_values)
}
# counting the missing authors.
missing_author <- sum(check_missing(books$author))
print(missing_author)
#Checking all categories for errors
for (column_name in names (books)){
  missing_count <- sum(check_missing(books[[column_name]]), na.rm = TRUE)  
  cat("Missing", column_name, ":", missing_count, "\n")
}
# Find duplicate books
 duplicate_books <- duplicated(books [c("title","author")])
 duplicate_count <- sum(duplicate_books)
 cat("Duplicate Books",column_name,":",duplicate_count,"\n")
 # Create a copy of the original library
 cleaned_books <- books
 # Standardize book titles
cleaned_books$title <- tools::toTitleCase(trimws(books))
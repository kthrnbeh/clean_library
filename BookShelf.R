
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

 # Create a copy of the original library
 cleaned_books <- books
 # Standardize book titles
cleaned_books$title <- tools::toTitleCase(trimws(cleaned_books$title))
cleaned_books$title <- gsub("  +"," ",cleaned_books$title)
cleaned_books$title_key <- tolower(cleaned_books$title)

# Remove extra spaces from author names
cleaned_books$author <- trimws(cleaned_books$author)
cleaned_books$author <- gsub("  +"," ",cleaned_books$author)

cleaned_books$author_key <- tolower(cleaned_books$author)
# Find duplicate books
 duplicate_books <- duplicated(cleaned_books [c("title_key","author_key")])
 duplicate_count <- sum(duplicate_books)
 cat("Duplicate Books", duplicate_count,"\n")
 # Remove duplicate books
 cleaned_books <- cleaned_books[!duplicate_books, ]
 cat("Books remaining:", nrow(cleaned_books), "\n")
 print(cleaned_books)
 # This sections purpose is to find pages that are incorrect, or not fully a number
 #count it and replace it with NA
 invalid_pages <- cleaned_books$pages <= 0| cleaned_books$pages %% 1 != 0
 invalid_page_count <-sum(invalid_pages,na.rm = TRUE)
 cat("Invalid page amount:",invalid_page_count,"\n")
 cleaned_books$pages[which(invalid_pages)] <- NA
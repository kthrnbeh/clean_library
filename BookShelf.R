
# My first R program
print("Welcome to BookShelf!")
#This loads my csv file, the empty cells will be read as NA
books <- read.csv("books.csv", na.strings = c("", "NA"))
print(books)
#I want to check for missing authors and count them
check_missing <- function(column) {
  missing_values <- is.na(column) | trimws(column) == ""
  return(missing_values)
}
missing_author <- sum(check_missing(books$author))
print(missing_author)
#The categories will be checked for missing info instead of each column
for (column_name in names (books)){
  missing_count <- sum(check_missing(books[[column_name]]), na.rm = TRUE)  
  cat("Missing", column_name, ":", missing_count, "\n")
}

 # this is what the library I am making is all about
 #a clean library that is in here without changing the original one. 
 cleaned_books <- books
 # Standardize book titles
cleaned_books$title <- tools::toTitleCase(trimws(cleaned_books$title))
cleaned_books$title <- gsub("  +"," ",cleaned_books$title)
cleaned_books$title_key <- tolower(cleaned_books$title)

#The codes about removing extra space, and moving it to a regular one
cleaned_books$author <- trimws(cleaned_books$author)
cleaned_books$author <- gsub("  +"," ",cleaned_books$author)

cleaned_books$author_key <- tolower(cleaned_books$author)


# This section finds the duplicate books counts them and deletes them from the main count
#without changing the actual csv author file
 duplicate_books <- duplicated(cleaned_books [c("title_key","author_key")])
 duplicate_count <- sum(duplicate_books)
 cat("Duplicate Books", duplicate_count,"\n")
 cleaned_books <- cleaned_books[!duplicate_books, ]
 cat("Books remaining:", nrow(cleaned_books), "\n")

 
 # This sections purpose is to find pages that are incorrect, or not fully a number
 #count invalid pagesand replace it with NA
 invalid_pages <- cleaned_books$pages <= 0| cleaned_books$pages %% 1 != 0
 invalid_page_count <-sum(invalid_pages,na.rm = TRUE)
 cat("Invalid page amount:",invalid_page_count,"\n")
 cleaned_books$pages[which(invalid_pages)] <- NA
 # Identify invalid ratings
 #we want to figure out if the ratings in my csv are correctly 
 #written from 0-5
 invalid_ratings <- cleaned_books$rating < 0 | cleaned_books$rating > 5
 invalid_rating_count <- sum(invalid_ratings, na.rm=TRUE)
 cat("Invalid ratings:",invalid_rating_count,"\n")
 #now were replacing the invalid ratings with NA
 cleaned_books$rating[which(invalid_ratings)] <- NA

 #We made two columns that helped detect duplicates regardless of 
 #if they were captilized these will remove that by using Null
 cleaned_books$title_key <-NULL
 cleaned_books$author_key <-NULL 




 print(cleaned_books)
 # The finished book gives a true statement and not finished is false, I want to see how many are finished 
 #and the total pages
 cat('\n-- BookShelf Library Report---\n')
 total_books <- nrow(cleaned_books)
 average_rating <-mean(cleaned_books$rating,na.rm = TRUE)
 total_pages <-sum(cleaned_books$pages, na.rm=TRUE)
 finished_books <- sum(cleaned_books$finished ==TRUE,na.rm =TRUE)
 unfinished_books <-sum(cleaned_books$finished == FALSE,na.rm =TRUE)
 completion_percentage <-(finished_books/total_books)*100

#Now I want to order them in order of genre rating
average_genre_rating <-aggregate(
  rating ~ genre,
  data=cleaned_books,
  Fun = mean,
  na.rm=TRUE
)
average_genre_rating <- average_genre_rating [
  order(-average_genre_rating$rating),
]
cat("Average rating by genre:\n")
print(average_genre_rating)
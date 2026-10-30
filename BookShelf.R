
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
 

#Now I want to order them in order of genre rating
average_genre_rating <- aggregate(
  rating ~ genre,
  data=cleaned_books,
  FUN = mean,
  na.rm=TRUE
)
average_genre_rating <- average_genre_rating [
  order(-average_genre_rating$rating),
]
cat("Average rating by genre:\n")
print(average_genre_rating)
#I want to know what percentage of books I finished.
 
completion_percentage <-function(finished,unfinished){
  known_status <- finished + unfinished
  if (known_status >0){
    return(round((finished / known_status) * 100, 1))
  } else{
    return(NA_real_)
  }
}
completion_percentage <- calculate_complete(
  finished_books,unfinished_books
)
cat("Percent of book completed:", completion_percentage(),"%\n")


#Wondering what my longest and shortest books are 
longest_pages <- max(cleaned_books$pages,na.rm=TRUE)
shortest_pages <- min(cleaned_books$pages,na.rm=TRUE)
longest_book<-cleaned_books [
  !is.na(cleaned_books$pages) &
  cleaned_books$pages ==longest_pages,
]
shortest_book<-cleaned_books [
  !is.na(cleaned_books$pages) &
  cleaned_books$pages == shortest_pages,
]
cat("Shortest Book: \n")
print(shortest_book[c("title", "pages")])
cat("Longest Book : \n")
print(longest_book[c("title", "pages")])
#were making a bar graph ,which is crazy in code
reading_counts <- c(finished_books, unfinished_books)
png("reading_progress.png", width=900,height=600)
barplot(
  reading_counts,
  names.arg=c("Finished", "Unfinished"),
  col=c("darkgreen","steelblue"),
  main="My Reading Progress",
  ylab= "Number of books"
)
dev.off()

#I want to see a graph comparing genre ratings. 
png("genre_ratings.png", width=930,height=600)
barplot(
  height =average_genre_rating$rating,
  names.arg =average_genre_rating$genre,
  col="steelblue",
  main="Average Book Rating by Genre",
  ylab="Average Rating",
  ylim =c(0,5),
  las =2
)
dev.off()

#A Second CSV file to keep my ratings in
write.csv(
  average_genre_rating,
  "genre_statistics.csv",
  row.names= FALSE
)
cat("Genre statistics are now saved!")

#Now that I know how to make a csv file I want one for my new info 
#making a new csv clean books file. 
write.csv(
  cleaned_books,
  "cleaned_books.csv",
  row.names= FALSE
)
cat("All the Books with accurate info")
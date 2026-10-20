
# Student Grade Analysis

students <- c("Alex", "Sam", "Jordan", "Taylor")
grades <- c(90, 85, 95, 100)

# Display student information
data <- data.frame(students, grades)
print(data)

# Calculate statistics
print(mean(grades))
print(max(grades))
print(min(grades))

# Make a bar graph
barplot(
  grades,
  names.arg = students,
  main = "Student Grades",
  xlab = "Students",
  ylab = "Grades",
  col = "skyblue",
  ylim = c(0, 100)
)

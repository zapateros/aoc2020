fl <- readLines("input_day_06.txt")
fl[fl==""] <- "/"
groups <- trimws(unlist(strsplit(paste(fl, collapse = " "), "/")))

# Part 1
result <- sum(sapply(groups, function(x){sum(letters %in% unlist(strsplit(x, "")))}))
cat("Day 6 Part 1: ", result)

# Part 2
result <- sum(sapply(groups, function(x){
  y <- unlist(strsplit(x, " "))
  z <- paste(y, collapse = "")
  a <- table(unlist(strsplit(z, "")))
  sum(a == length(y))
}))
cat("Day 6 Part 2: ", result)

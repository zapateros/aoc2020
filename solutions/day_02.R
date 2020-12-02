fl <- readLines("input_day_02.txt")
ls <- strsplit(gsub("-|:", " ", fl), " ")
# Part 1
y <- lapply(ls, function(x){
 z <- unlist(x)
 num <- length(unlist(gregexpr(z[3], z[5])))
 num >= as.numeric(z[1]) & num <= as.numeric(z[2])
})
output <- sum(unlist(y))
cat("Problem 2 part 1: ", output)
#Part 2
y <- lapply(ls, function(x){
  z <- unlist(x)
  letters <- unlist(strsplit(z[5],""))
  sum(letters[c(as.numeric(z[c(1, 2)]))] == z[3]) == 1
})
output <- sum(unlist(y))
cat("Problem 2 part 2: ", output)

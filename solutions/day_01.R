# Problem 1 ----
# I couldn't find the function I needed fast (the same function I used about a million times already) so I panicked and turned 
# to making a matrix for part 1 and an array for part 2
# After I solved it and relaxed a bit, I found the relevant expand.grid function so I rewrote my solution as it is significanly more elegant

fl <- as.numeric(readLines("input_day_01.txt"))
# Part 1
x <- expand.grid(fl, fl)
output <- prod(x[which(rowSums(x) == 2020)[1],])
cat("Problem 1 part 1: ", output)

# Part 2
x <- expand.grid(fl, fl, fl)
output <- prod(x[which(rowSums(x) == 2020)[1],])
cat("Problem 1 part 2: ", output)

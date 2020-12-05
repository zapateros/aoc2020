# I did part 1 by hand. B's (and R's later) are getting less valuable from left to right, just like binary code
# I also solved it in code, because I'm using part of the code for part 2.
# Because of the multiplyer 8 for the first 7 characters, it practically was a bit shift by 3 bits, so the whole seat-code can
# be read as a binary string (when replacing B and R with 1 and F and L with 0)
fl     <- readLines("input_day_05.txt")
fl_bin <- gsub("B|R", 1, gsub("F|L", 0, fl))
seats  <- strtoi(fl_bin, base = 2)

# Part 1
result <- max(seats)
cat("Day 5 Part 1: ", result)

# Part 2
exg <- expand.grid(seats, seats)
dif <- eg[, 1] - eg[, 2]
sts <- eg[dif == 2,]
bet <- sts[, 1] - 1
  
result <- bet[!(bet %in% seats)]
cat("Day 5 Part 2: ", result)

fl  <- readLines("input_day_25.txt")
dpk <- as.numeric(fl[1])
cpk <- as.numeric(fl[2])

it <- 0
x  <- 1
t  <- NULL
while(x != dpk){
  it <- it + 1
  x <- (7 * x) %% 20201227
}

it2 <- 0
x   <- 1
while(it2 < it){
  x <- (cpk * x) %% 20201227
  it2 <- it2 + 1
}
result <- x
cat("Day 25 Part 1:", result)

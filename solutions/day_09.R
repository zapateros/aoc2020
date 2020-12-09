# Today was a quick one
fl  <- readLines("input_day_09.txt")
fln <- as.numeric(fl)

# Part 1
x <- 25
i <- x + 1
while(TRUE){
  rel <- fln[(i - x):(i- 1)]
  gr <- expand.grid(rel, rel)
  sm <- rowSums(gr)
  if(!fln[i] %in% sm){
    break
  }
  i <- i + 1
}
result_part1 <- fln[i]
cat("Day 9 Part 1:", result_part1)

# Part 2
n   <- result_part1
fls <- fln[fln < n]

l  <- 2
s  <- 1
sm <- 0
while(!(sm == n)){
  ra <- fls[s:(s + l - 1)]
  sm <- sum(ra)
  if(sm > n){
    s <- 1
    l <- l + 1
  }
  s <- s + 1
}

result <- sum(min(ra), max(ra))
cat("Day 9 Part 2:", result)

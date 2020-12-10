# I already had a hunch a recursive function would help me for part 2 so I tried to find a numeric solution or some other trick
# When I couldn't find one quickly, I tried the recursion and saw it (indeed) would take forever. When I wrote down the first example series (in order),
# it was pretty clear how to solve it easily
fl  <- readLines("input_day_10.txt")
fln <- as.numeric(fl)
or  <- fln[order(fln)]

# Part 1
difs <- table(diff(or))
result <- prod(difs + 1)
cat("Day 10 Part 1:", result)

# Part 2
nms <- c(0, or)
st <- sapply(nms, function(x){
  sum(c((x + 1):(x + 3)) %in% nms)
})
n     <- length(st)
st[n] <- 1
for(i in (n - 1):1){
  x     <- st[i]
  rel   <- st[(i + 1):(i + x)]
  st[i] <- sum(rel)
}
result <- as.character(st[1])
cat("Day 10 Part 2:", result)

# Certainly not my cleanest solution. For the first part I used a simple shortest path algorithm
# For the second part I was looking for a simple solution as it's still quite early this year. However,
# I couldn't think of a simple trick so I did a recursive. Maybe I'll clean up this code later. Probably not
fl    <- readLines("input_day_07.txt")
fl1   <- gsub(" bags| bag|\\.", "", fl)
rules <- unlist(strsplit(fl1, "\n"))

dt <- NULL
for(i in 1:length(rules)){
  y       <- unlist(strsplit(rules[i], " contain "))
  bag_top <- y[1]
  z       <- unlist(strsplit(y[2], ", "))
  bags_in <- trimws(gsub("[0-9]", "", z))
  amount  <- as.numeric(gsub("[^0-9]", "", z))
  part    <- cbind.data.frame(bag_top, amount, bags_in)
  dt      <- rbind(dt, part)
}

# Part 1
library(igraph)
gr <- graph_from_data_frame(dt[,c("bag_top", "bags_in")], directed = TRUE)
bags <- unique(c(dt$bag_top, dt$bags_in))
pos <- sapply(bags, function(x){
  paths <- all_shortest_paths(gr, from = x, to = "shiny gold")
  length(paths$res)
})

result <- sum(pos > 0) - 1
cat("Day 7 Part 1:", result)

# Part 2
dt  <- dt[!dt$bags_in == "no other",]
num <- 0
rec <- function(z, x, m){
  y <- as.character(dt$bags_in[dt$bag_top == x])
  k <- as.numeric(dt$amount[dt$bag_top == x])
  a <- z
  if(length(y) > 0){
    for(i in 1:length(y)){
      n <- m * k[i]
      b <- c(a, y[i])
      rec(b, y[i], n)
      num <<- num + n
    }
  }
}
path  <- NULL
start <- "shiny gold"
rec(path, start, 1)
result <- num
cat("Day 7 Part 2:", result)

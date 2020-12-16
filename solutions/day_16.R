fl      <- readLines("input_day_16.txt") 
splits  <- which(fl == "")
part_1  <- fl[1:(splits[1] - 1)]
rules   <- matrix(unlist(strsplit(unlist(strsplit(gsub(" or ", "-", part_1), ": ")), "-")), ncol = 5, byrow = TRUE)
part_2  <- fl[splits[1] + 2]
yr_tick <- as.numeric(unlist(strsplit(part_2, ",")))
part_3  <- fl[(splits[2] + 2):length(fl)]
tickets <- as.numeric(unlist(strsplit(part_3, ",")))

# Part 1
y <- sapply(tickets, function(x){
  y <- as.numeric(rules[,2]) <= x  & as.numeric(rules[,3]) >= x
  z <- as.numeric(rules[, 4]) <= x & as.numeric(rules[, 5]) >= x
  if(!any(c(y, z))){
    x
  }else{
    NULL
  }
})
result <- sum(unlist(y))
cat("Day 16 Part 1:", result)

# Part 2
mt_tickets <- matrix(tickets, ncol = nrow(rules), byrow = TRUE)
mt         <- rbind(yr_tick, mt_tickets)
invalid    <- unlist(y)
fields     <- rules[, 1]

possible <- list()
impossib <- list()
for(i in 1:ncol(mt)){
  y <- sapply(mt[, i], function(x){
    if(!x %in% invalid){
      y <- as.numeric(rules[,2]) <= x  & as.numeric(rules[,3]) >= x
      z <- as.numeric(rules[, 4]) <= x & as.numeric(rules[, 5]) >= x
      a <- !(y | z)
      fields[a]
    }
  })
  imp <- unique(unlist(y))
  pos <- fields[!fields %in% imp]
  impossib[[i]] <- imp
  possible[[i]] <- pos
}

npos <- sapply(possible, function(x){length(x)})
flds <- NULL
for(i in order(npos)){
  rel <- possible[[i]]
  flds <- c(flds, rel[!rel %in% flds])
}
correct <- flds[npos]
result  <- prod(yr_tick[grepl("departure", correct)])
cat("Day 16 Part 2:", result)

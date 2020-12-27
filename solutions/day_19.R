# This was the first one I didn't really know how to solve. I spent a day thinking of recursive functions or tricks but I couldn't find one.
# Because of this one I was a bit behind schedule so I saved it for last. When I just bruteforced (which I was certain was not the right way), 
# I got a good answer for part 1. It was not until I read part 2 when I finally understood the problem. Node 11 and node 8 were solvable independently
# This significantly increased efficiency and opened the road to solving part 2, which was easy now, as I explain below. 
# When I didn't completely understood this problem this was the worst one of this year; now I get it, it is one of the best. With this one I also complete AoC2020!
fl       <- readLines("input_day_19.txt")
split    <- which(fl == "")
messages <- fl[(split + 1):length(fl)]

part_1 <- fl[1:(split - 1)]
rules  <- matrix(unlist(strsplit(part_1, ": ")), ncol = 2, byrow = TRUE)
ind_a  <- which(rules[,2] == "\"a\"")
ind_b  <- which(rules[,2] == "\"b\"")
num_a  <- as.numeric(rules[ind_a, 1])
num_b  <- as.numeric(rules[ind_b, 1])
rules[ind_a, 2] <- num_a
rules[ind_b, 2] <- num_b

run <- function(input){
  y  <- list(input)
  while(TRUE){
    un <- unique(unlist(y))
    z  <- un[!un %in% c(num_a, num_b)]
    if(length(z) == 0) break
    x    <- z[1]
    rule <- rules[which(rules[,1] == x), 2]
    if(grepl("\\|", rule)){
      gps <- sapply(y, function(xx){any(unlist(xx) == x)})
      y3   <- y[gps]
      y    <- y[!gps]
      y1   <- y2 <- y3
      spl  <- unlist(strsplit(rule, " \\| "))
      nmt1 <- as.numeric(unlist(strsplit(spl[1], " ")))
      nm1  <- nmt1[!is.na(nmt1)]
      for(i1 in 1:length(y1)){
        ind <- which(y1[[i1]] == x)
        y1[[i1]] <- append(y1[[i1]], nm1, ind[1])
        y1[[i1]] <- y1[[i1]][-ind[1]]
      }
      nmt2 <- as.numeric(unlist(strsplit(spl[2], " ")))
      nm2  <- nmt2[!is.na(nmt2)]
      for(i2 in 1:length(y2)){
        ind <- which(y2[[i2]] == x)
        y2[[i2]] <- append(y2[[i2]], nm2, ind[1])
        y2[[i2]] <- y2[[i2]][-ind[1]]
      }
      y <- c(y, y1, y2)
    }else{
      nmt <- as.numeric(unlist(strsplit(rule, " ")))
      nm  <- nmt[!is.na(nmt)]
      for(i in 1:length(y)){
        ind <- which(y[[i]] == x)
        if(length(ind) > 0){
          y[[i]] <-  append(y[[i]], nm, ind[1])
          y[[i]] <- y[[i]][-ind[1]]
        }
      }
    }
  }
  y
}

tochar <- function(input){
  sapply(input, function(x){
    paste0(gsub(9, "b", gsub(69, "a", unlist(x))), collapse = "")
  })
}

# Part 1
# a correct string is formed by taking one from y42, y42 and y31 in that order
y42 <- tochar(run(42))
y31 <- tochar(run(31))
count <- 0
for(z in messages){
  z1 <- substr(z, 1, 8)
  z2 <- substr(z, 9, 16)
  z3 <- substr(z, 17, 24)
  if(z1 %in% y42 & z2 %in% y42 & z3 %in% y31 & nchar(z) == 24){
    count <- count + 1
  }
}
result <- count
cat("Day 19 Part 1:", result)

# Part 2
# Now a correct string could be an infinite possible combinations of y42 and y31 (in a particular order)
# The maximum characters of the messages to check is 88 (11 times y42 or y31). 
# An extra loop of 8 (42 | 42 8) means y42, y42, etc.. (plus 8 chars everytime)
# an extra loop of 11 (42 31 | 42 11 31) means y42, y42, y31, y31 (plus 16 chars everytime)
# Both 8 and 11 are present at least one time (with 8 always in front of 11)
# Therefore a message of 32 characters can only be formed by 8, 8, 11 (or y42, y42, y42, y31)
# a message of 40 chars can be formed by 8, 11, 11 (y42, y42, y42, y31, y31)
# A pattern can be seen: y42s are always first and y31s last. Also there should alway be at least 1 more y42 than y31
count <- 0
for(i in 1:length(messages)){
  m <- messages[i]
  t <- rep(1, nchar(m) / 8)
  while(TRUE){
    ind1 <- max(which(t == 1))
    if((ind1 - 1) <= (length(t) / 2)) break
    t[ind1] <- 2
    m1  <- m
    trs <- NULL
    for(j in 1:length(t)){
      relm <- substr(m1, 1, 8)
      m1   <- substring(m1, 9)
      if(t[j] == 1){
        trs <- c(trs, relm %in% y42)
      }else{
        trs <- c(trs, relm %in% y31)
      }
    }
    if(all(trs)){
      count <- count + 1
      break
    }
  }
}
result <- count
cat("Day 19 Part 2:", result)

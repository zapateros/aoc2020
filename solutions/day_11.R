# Due to the lack of time the last few days, I'm a bit behind schedule. Therefore I decided not to care too much about my solutions. 
# If they work, they work! Maybe I'll clean them up later. We both know I won't
fl  <- readLines("input_day_11.txt")
w   <- nchar(fl[1])
h   <- length(fl)
mt  <- matrix(unlist(strsplit(fl, "")), nrow = h, byrow = TRUE)
mt  <- rbind(".", cbind(".", mt, "."), ".")

mtt <- mt
while(TRUE){
  for(i in 2:(h + 1)){
    for(j in 2:(w + 1)){
      pnt <- mt[i, j]
      nbs <- c(mt[i - 1, j - 1], mt[i - 1, j], mt[i - 1, j + 1], 
               mt[i, j - 1], mt[i, j + 1],
               mt[i + 1, j - 1], mt[i + 1, j], mt[i + 1, j + 1])
      if(pnt == "L" & sum(nbs == "#") == 0){
        mtt[i, j] <- "#"
      }else if(pnt == "#" & sum(nbs == "#") >= 4){
        mtt[i, j] <- "L"
      }
    }
  }
  if(all(mt == mtt)){
    break
  }
  mt <- mtt
}

result <- sum(mtt == "#")
cat("Day 11 Part 1:", result)

# Part 2
mt <- matrix(unlist(strsplit(fl, "")), nrow = h, byrow = TRUE)
lu <- function(x, y, layout){
  t <- "."
  while(x > 1 & y > 1){
    x <- x - 1
    y <- y - 1
    t <- layout[x, y]
    if(t != "."){
      break
    }
  }
  return(t)
}

ld <- function(x, y, layout){
  t <- "."
  while(x < h & y > 1){
    x <- x + 1
    y <- y - 1
    t <- layout[x, y]
    if(t != "."){
      break
    }
  }
  return(t)
}

ru <- function(x, y, layout){
  t <- "."
  while(x > 1 & y < w){
    x <- x - 1
    y <- y + 1
    t <- layout[x, y]
    if(t != "."){
      break
    }
  }
  return(t)
}

rd <- function(x, y, layout){
  t <- "."
  while(x < (h) & y < (w)){
    x <- x + 1
    y <- y + 1
    t <- layout[x, y]
    if(t != "."){
      break
    }
  }
  return(t)
}

ri <- function(x, y, layout){
  t <- "."
  while(y < (w)){
    y <- y + 1
    t <- layout[x, y]
    if(t != "."){
      break
    }
  }
  return(t)
}

le <- function(x, y, layout){
  t <- "."
  while(y > 1){
    y <- y - 1
    t <- layout[x, y]
    if(t != "."){
      break
    }
  }
  return(t)
}

do <- function(x, y, layout){
  t <- "."
  while(x < (h)){
    x <- x + 1
    t <- layout[x, y]
    if(t != "."){
      break
    }
  }
  return(t)
}

up <- function(x, y, layout){
  t <- "."
  while(x > 1){
    x <- x - 1
    t <- layout[x, y]
    if(t != "."){
      break
    }
  }
  return(t)
}

mtt <- mt
while(TRUE){
for(q in 1:h){
  for(e in 1:w){
    pnt <- mt[q, e]
    nbs <- c(lu(q, e, mt),
             up(q, e, mt),
             ru(q, e, mt),
             ri(q, e, mt),
             rd(q, e, mt),
             do(q, e, mt),
             ld(q, e, mt),
             le(q, e, mt)) 
    if(pnt == "L" & sum(nbs == "#") == 0){
      mtt[q, e] <- "#"
    }else if(pnt == "#" & sum(nbs == "#") >= 5){
      mtt[q, e] <- "L"
    }
  }
}
  if(all(mt == mtt)){
    break
  }
  mt <- mtt
}

result <- sum(mtt == "#")
cat("Day 11 Part 2:", result)

# I set up part 1 to already anticipate for part 2. I was thinking of either many cycles or more dimensions. Therefore
# it was pretty easy to do part 2. With just minor adjustments it can run for more dimensions. Still it does take some time (like 5 minutes) to process part 2.
# As the result of every cycle is mirrored around the initial axes, this could be used to improve the runtime. 
# As you can see I didn't take the time yet to write a function (with dimensions input) that can be used for both parts.
fl <- readLines("input_day_17.txt")
mt <- matrix(unlist(strsplit(fl, "")), nrow = length(fl), byrow = TRUE)

# Part 1
# Create active locations from input
active  <- cbind(which(mt == "#", arr.ind = TRUE), 0)
activep <- apply(active, 1, paste, collapse = ".")

# Create neighbour summation matrix
pl <- c(-1, 0, 1)
sm <- expand.grid(pl, pl, pl)[-14,]
it <- 0
while(it < 6){
  # Create all possible relevant points from active matrix
  nbs <- NULL
  for(i in 1:length(activep)){
    left  <- activep[i]
    rel   <- as.numeric(unlist(strsplit(left, "\\.")))
    rlm   <- matrix(rep(rel, 26), ncol = 3, byrow = TRUE)
    rls   <- rlm + sm
    right <- apply(rls, 1, paste, collapse = ".") 
    nbs   <- rbind(nbs, cbind(left, right))
  }
  un       <- unique(c(nbs))
  nactivep <- NULL
  for(j in 1:length(un)){
    rel_nb <- unique(c(nbs[nbs[, 1] == un[j], 2], nbs[nbs[, 2] == un[j], 1]))
    nba    <- sum(rel_nb %in% activep)
    isact  <- un[j] %in% activep
    if(isact){
      if(nba %in% c(2,3)){
        nactivep <- c(nactivep, un[j])
      }
    }else{
      if(nba == 3){
        nactivep <- c(nactivep, un[j])
      }
    }
  }
  activep <- nactivep
  it      <- it + 1
}
result <- length(activep)
cat("Day 17 Part 1:", result)

# Part 2
active  <- cbind(which(mt == "#", arr.ind = TRUE), 0, 0)
activep <- apply(active, 1, paste, collapse = ".")

# Create neighbour summation matrix
pl <- c(-1, 0, 1)
sm <- expand.grid(pl, pl, pl, pl)[-41,]
it <- 0
while(it < 6){
  # Create all possible relevant points from active matrix
  nbs <- NULL
  for(i in 1:length(activep)){
    left  <- activep[i]
    rel   <- as.numeric(unlist(strsplit(left, "\\.")))
    rlm   <- matrix(rep(rel, 80), ncol = 4, byrow = TRUE)
    rls   <- rlm + sm
    right <- apply(rls, 1, paste, collapse = ".") 
    nbs   <- rbind(nbs, cbind(left, right))
  }
  un       <- unique(c(nbs))
  nactivep <- NULL
  for(j in 1:length(un)){
    rel_nb <- unique(c(nbs[nbs[, 1] == un[j], 2], nbs[nbs[, 2] == un[j], 1]))
    nba    <- sum(rel_nb %in% activep)
    isact  <- un[j] %in% activep
    if(isact){
      if(nba %in% c(2,3)){
        nactivep <- c(nactivep, un[j])
      }
    }else{
      if(nba == 3){
        nactivep <- c(nactivep, un[j])
      }
    }
  }
  activep <- nactivep
  it      <- it + 1
}
result <- length(activep)
cat("Day 17 Part 2:", result)

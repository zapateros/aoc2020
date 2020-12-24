# I had fun with the first one. The second not so much. This was just a slightly adjusted version of day 17. However, the first time I ran the code
# it took about 40 minutes for the first 30 iterations and the iteration time was increasing significantly. Therefor I tried to find another way but
# as usual, when I couldn't find one in a considerable time, I turned to a faster language. When I was rewriting the script in C#, I tried tuples but
# as I'm not very skilled in C# I just tried to do everything with separate lists, which would do the trick. However, when I was looking closely at my
# R-script, I saw one of my closing brackets for a for loop was placed at a very weird place. This misplacement didn't affect the outcome but did so
# on the performance (significantly) as it now calculated quite some tiles multiple times. When I moved the bracket the script ran in about 4 minutes (still slow yes)
fl   <- readLines("input_day_24.txt")
dirs <- c("nw", "ne", "sw", "se", "w", "e")
dirx <- c(-1, 1, -1, 1, -2, 2)
diry <- c(-2, -2, 2, 2, 0, 0)

tiles <- NULL
for(i in fl){
  steps <- NULL
  while(nchar(i) > 0){
    st <- substr(i, 1, 1)  
    if(st %in% c("w", "e")){
      steps <- c(steps, st)
      i <- substring(i, 2)
    }else{
      steps <- c(steps, substr(i, 1, 2))
      i <- substring(i, 3)
    }
  }
  inds  <- sapply(steps, function(x){which(dirs == x)})
  px    <- sum(dirx[inds])
  py    <- sum(diry[inds])
  p     <- paste(py, px, sep = ",")  
  tiles <- c(tiles, p)
}
result <- sum(table(tiles) %% 2 == 1)
cat("Day 24 Part 1:", result)

# Part 2
z     <- table(tiles) %% 2 == 1
black <- names(z[z == TRUE])
sm    <- cbind(diry, dirx)

it <- 0
while(it < 100){
  cat(paste(Sys.time()), ":", it, ":", length(black), "\n")
  nbs <- NULL
  for(i in 1:length(black)){
    left  <- black[i]
    rel   <- as.numeric(unlist(strsplit(left, ",")))
    rlm   <- matrix(rep(rel, 6), ncol = 2, byrow = TRUE)
    rls   <- rlm + sm
    right <- apply(rls, 1, paste, collapse = ",") 
    nbs   <- rbind(nbs, cbind(left, right))
  }
  un     <- unique(c(nbs))
  nblack <- NULL
  for(j in 1:length(un)){
    rel_nb <- unique(c(nbs[nbs[, 1] == un[j], 2], nbs[nbs[, 2] == un[j], 1]))
    nba    <- sum(rel_nb %in% black)
    isact  <- un[j] %in% black
    if(isact){
      if(nba %in% c(1, 2)){
        nblack <- c(nblack, un[j])
      }
    }else{
      if(nba == 2){
        nblack <- c(nblack, un[j])
      }
    }
  }
  black <- nblack
  it    <- it + 1
}
result <- length(black)
cat("Day 24 Part 2:", result)

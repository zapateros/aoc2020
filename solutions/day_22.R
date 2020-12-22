# For part 2 it's a bit slow. I haven't found a nice solution where I can remember previous rounds without saving them in a vector or list but I'm sure there is
# The hardest part of todays problem was to read the problem carefully. I didn't process the part where you copy just n cards of your deck in the recursive game; I copied all
# of the cards so that led to quite some debugging hours
fl <- readLines("input_day_22.txt")
split <- which(fl == "")
p1 <- as.numeric(fl[2:(split - 1)])
p2 <- as.numeric(fl[(split + 2):length(fl)])

# Part 1
while(length(p1) > 0 & length(p2) > 0){
  x1 <- p1[1]
  x2 <- p2[1]
  p1 <- p1[-1]  
  p2 <- p2[-1]
  if(x1 > x2){
    p1 <- c(p1, x1, x2)
  }else{
    p2 <- c(p2, x2, x1)
  }
}
result <- sum(p2 * length(p2):1)
cat("Day 22 Part 1:", result)

# Part 2
p1   <- as.numeric(fl[2:(split - 1)])
p2   <- as.numeric(fl[(split + 2):length(fl)])
crds <- NULL
run <- function(player_1, player_2){
  while(length(player_1) > 0 & length(player_2) > 0){
    z <- paste(paste0(player_1, collapse = ","), paste0(player_2, collapse = ","))
    if(z %in% crds){
      player_2 <- NULL
      break
    }else{
      crds <- c(crds, z)
    }
    x1       <- player_1[1]
    x2       <- player_2[1]
    player_1 <- player_1[-1]
    player_2 <- player_2[-1]
    if(x1 <= length(player_1) & x2 <= length(player_2)){
      y <- run(player_1[1:x1], player_2[1:x2])
      if(length(y[[2]]) == 0){
        player_1 <- c(player_1, x1, x2)
      }else{
        player_2 <- c(player_2, x2, x1)
      }
    }else{
      if(x1 > x2){
        player_1 <- c(player_1, x1, x2)
      }else{
        player_2 <- c(player_2, x2, x1)
      }
    }
  }
  list(player_1, player_2)
}
out    <- run(p1, p2)
result <- sum(unlist(out) * (length(p1) + length(p2)):1)
cat("Day 22 Part 2:", result)

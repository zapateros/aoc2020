# I liked this one. It could have been a lot harder if de pieces would fit on multiple others, then a recursive function would be helpful. Now
# it was pretty easy to solve the problems, but there were quite some steps where things could go wrong. First I calculated every possible
# state of the pieces and rewrote the sides as bitvalues. With the sides as numeric values and knowing the pieces just fit on one other, the rest was a straightforward job
# The hardest part of today was to write every step concisely and think of variable names. In the code there are still some manual parts (choose a corner and rotate the dragon)
fl     <- readLines("input_day_20.txt")
split  <- c(0, which(fl == ""), length(fl) + 1)
w      <- nchar(fl[2])
l      <- split[2] - 2
bits   <- 2^c(0:(l - 1))
bits_m <- rev(bits)

dt <- NULL
for(i in 1:(length(split) - 1)){
  rel   <- fl[(split[i] + 1):(split[i + 1] - 1)]
  tile  <- as.numeric(gsub("[^0-9]", "", rel[1]))
  rel   <- rel[-1]
  
  # Get bit values
  up_bits    <- unlist(gregexpr("#", rel[1]))
  right_bits <- which(substring(rel, l) == "#")
  down_bits  <- unlist(gregexpr("#", rel[l]))
  left_bits  <- which(substr(rel, 1, 1) == "#")
  
  # Normal piece clockwise
  n0     <- data.frame(up = sum(bits[up_bits]), right = sum(bits[right_bits]), down = sum(bits[down_bits]), left = sum(bits[left_bits]), deg = 0)
  n90    <- data.frame(up = sum(bits[11 - left_bits]), right = sum(bits[up_bits]), down = sum(bits[11 - right_bits]), left = sum(bits[down_bits]), deg = 90)
  n180   <- data.frame(up = sum(bits[11 - down_bits]), right = sum(bits[11 - left_bits]), down = sum(bits[11 - up_bits]), left = sum(bits[11 - right_bits]), deg = 180)
  n270   <- data.frame(up = sum(bits[right_bits]), right = sum(bits[11 - down_bits]), down = sum(bits[left_bits]), left = sum(bits[11 - up_bits]), deg = 270)
  normal <- rbind(n0, n90, n180, n270)
  normal$state <- "normal"
  
  # Mirrored piece clockwise (yaxis)
  m0     <- data.frame(up = sum(bits[11 - up_bits]), right = sum(bits[left_bits]), down = sum(bits[11 - down_bits]), left = sum(bits[right_bits]), deg = 0)
  m90    <- data.frame(up = sum(bits[11 - right_bits]), right = sum(bits[11 - up_bits]), down = sum(bits[11 - left_bits]), left = sum(bits[11 - down_bits]), deg = 90)
  m180   <- data.frame(up = sum(bits[down_bits]), right = sum(bits[11 - right_bits]), down = sum(bits[up_bits]), left = sum(bits[11 - left_bits]), deg = 180)
  m270   <- data.frame(up = sum(bits[left_bits]), right = sum(bits[down_bits]), down = sum(bits[right_bits]), left = sum(bits[up_bits]), deg = 270)
  mirror <- rbind(m0, m90, m180, m270)
  mirror$state <- "mirror"
  
  all_states <- rbind(normal, mirror)
  all_states$tile <- tile
  
  dt <- rbind(dt, all_states)
}

# Part 1
poss_nbs <- NULL
for(j in 1:nrow(dt)){
  rel      <- dt[j, 1:4]
  tile     <- dt$tile[j]
  trs      <- sapply(rel, function(x){nrow(which(dt[-which(dt$tile == tile), 1:4] == x, arr.ind = TRUE)) > 0})  
  poss_nbs <- rbind(poss_nbs, c(tile, trs)) 
}
nums         <- cbind(poss_nbs[, 1], rowSums(poss_nbs[, 2:5]))
mxs          <- aggregate(nums[,2], by = list(nums[,1]), FUN = max)
corner_tiles <- mxs[mxs[,2] == 2, 1]
result       <- as.character(prod(corner_tiles))
cat("Day 20 Part 1:", result)

# Part 2
dt$id <- 1:nrow(dt)
bu <- dt

# First calculate the correct pieces and states of one column. I chose one tile manually
dt <- bu
tile <- list(3187)
dt <- dt[-which(dt$tile == tile),]
d  <- 782
r  <- 901
ids <- list(393)

while(TRUE){
  ai <- which(dt[,1] == d, arr.ind = TRUE)
  if(length(ai) == 0) break
  row  <- ai
  col  <- 1
  id   <- dt$id[row]
  ids  <- c(ids, id)
  d    <- dt[row, (col + 1) %% 4 + 1]
  rx   <- dt[row, (col) %% 4 + 1]
  tile <- c(tile, dt$tile[row])
  r    <- c(r, rx)
  dt   <- dt[-which(dt$tile == dt$tile[row]),]
}

# Fill in the rest
for(k in 1:length(r)){
  r1 <- r[k]
  while(nrow(dt) > 0){
    ai <- which(dt[,4] == r1, arr.ind = TRUE)
    if(length(ai) == 0) break
    row <- ai
    col <- 4
    id   <- dt$id[row]
    ids[[k]]  <- c(ids[[k]], id)
    r1  <- dt[row, (col + 1) %% 4 + 1]
    tile[[k]] <- c(tile[[k]], dt$tile[row])
    dt  <- dt[-which(dt$tile == dt$tile[row]),]
  }
}

# Put together the pieces
rotate <- function(x) t(apply(x, 2, rev))

allmt <- NULL
for(rw in 1:12){
  rowmt <- NULL
  for(cl in 1:12){
    relid    <- ids[[rw]][cl]
    relti    <- bu[bu$id == relid,]
    relstate <- relti$state
    reldeg   <- relti$deg
    tl       <- relti$tile
    ind      <- unlist(gregexpr(tl, str))
    relstr   <- substr(str, ind + 5, ind + 104)
    relmt    <- matrix(unlist(strsplit(relstr, "")), nrow = 10 , byrow = TRUE)
    if(relstate == "mirror"){
      relmt <- relmt[, 10:1]
    }
    if(reldeg == 90){
      relmt <- rotate(relmt)
    }else if(reldeg == 180){
      relmt <- rotate(rotate(relmt))
    }else if(reldeg == 270){
      relmt <- rotate(rotate(rotate(relmt)))
    }
    rowmt <- cbind(rowmt, relmt)
  }
  allmt <- rbind(allmt, rowmt)
}

# remove piece edges
remind <- cumsum(rep(c(1,9), 12))
mdlmt <- allmt[-remind, -remind]
indst <- which(mdlmt == "#", arr.ind = TRUE)
inds <- apply(indst, 1, paste, collapse = "-")

# Look for dragons, manually rotate/flip the dragon until you find matches
drimp <- "                  # 
#    ##    ##    ###
 #  #  #  #  #  #   "
dragon <- matrix(unlist(strsplit(gsub("\n","", drimp), "")), nrow = 3, byrow = TRUE)
# dragon <- dragon[,ncol(dragon):1] SOME MANUAL WORK HERE
# dragon <- rotate(dragon)
dindst <- which(dragon == "#", arr.ind = TRUE)
dtiles <- NULL
for(rw in 0:(nrow(mdlmt) - nrow(dragon))){
  xx <- dindst
  xx[,1] <- xx[,1] + rw
  for(cl in 0:(ncol(mdlmt) - ncol(dragon))){
    zz <- xx
    zz[,2] <- zz[,2] + cl
    yy <- apply(zz, 1, paste, collapse = "-")
    if(all(yy %in% inds)){
      dtiles <- c(dtiles, yy)
    }
  }
}

result <- sum(mdlmt == "#") - length(unique(dtiles))
cat("Day 20 Part 2:", result)

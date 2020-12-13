# This one wouldn't be too hard if I didn't mess up my axes for part 2. I use a negative north y axis so rotations should be adjusted for that
# Also, obviously a lot of hardcoded stuff could be rewritten but for now it's okay
fl    <- readLines("input_day_12.txt") 
dir   <- 1
x     <- c(1, 0, -1, 0)
y     <- c(0, 1, 0, -1)
pos_x <- 0
pos_y <- 0
for(i in 1:length(fl)){
  act <- fl[i]
  cha <- gsub("[0-9]", "", act)
  num <- as.numeric(gsub("[A-Z]", "", act))
  if(cha == "R"){
    st <- num / 90
    dir <- (dir + st - 1) %% 4 + 1
  }else if(cha == "L"){
    st <- num / 90
    dir <- (dir - st - 1) %% 4 + 1
  }else if(cha == "N"){
    pos_y <- pos_y - num
  }else if(cha == "S"){
    pos_y <- pos_y + num
  }else if(cha == "E"){
    pos_x <- pos_x + num
  }else if(cha == "W"){
    pos_x <- pos_x - num
  }else if(cha == "F"){
    pos_x <- pos_x + num * x[dir]
    pos_y <- pos_y + num * y[dir]
  }
}
result <- abs(pos_x) + abs(pos_y)
cat("Day 12 Part 1:", result)

# Part 2
rotate_right <- function(a, b, n){
  if(n == 90){
    return(c(-b, a))
  }else if(n == 180){
    return(c(-a, -b))
  }else if(n == 270){
    return(c(b, -a))
  }
}

rotate_left <- function(a, b, n){
  if(n == 90){
    return(c(b, -a))
  }else if(n == 180){
    return(c(-a, -b))
  }else if(n == 270){
    return(c(-b, a))
  }
}

wp_x  <- 10
wp_y  <- -1
pos_x <- 0
pos_y <- 0
for(i in 1:length(fl)){
  act <- fl[i]
  cha <- gsub("[0-9]", "", act)
  num <- as.numeric(gsub("[A-Z]", "", act))
  if(cha == "L"){
   out  <- rotate_left(wp_x, wp_y, num)
   wp_x <- out[1]
   wp_y <- out[2]
  }else if(cha == "R"){
    out  <- rotate_right(wp_x,wp_y,  num)
    wp_x <- out[1]
    wp_y <- out[2]
  }else if(cha == "N"){
    wp_y <- wp_y - num
  }else if(cha == "S"){
    wp_y <- wp_y + num
  }else if(cha == "E"){
    wp_x <- wp_x + num
  }else if(cha == "W"){
    wp_x <- wp_x - num
  }else if(cha == "F"){
    pos_x <- pos_x + num * wp_x
    pos_y <- pos_y + num * wp_y
  }
}
result <- abs(pos_x) + abs(pos_y)
cat("Day 12 Part 2:", result)

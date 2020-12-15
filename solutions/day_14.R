# I'm sorry for this ugly solution. It's not even fast
fl <- readLines("input_day_14.txt") 

# Part 1
mask <- NULL
mems <- data.frame(memory = NULL, number = NULL)
for(i in 1:length(fl)){
  rel <- fl[i]
  if(grepl("mask", rel)){
   mask <- unlist(strsplit(gsub("mask = ", "", rel), ""))
  }else{
    nms <- unlist(strsplit(gsub("mem\\[", "", rel), "] = "))
    mem <- nms[1]
    num <- c(rep(0, 4), as.numeric(rev(intToBits(nms[2]))))
    nm  <- mask!="X"
    num[nm] <- as.numeric(mask[nm])
    dig <- sum(num * 2^c(35:0))
    ind <- which(mems$memory == mem)
    if(length(ind) == 0){
      mems <- rbind(mems, data.frame(memory = mem, number = dig))
    }else{
      mems$number[ind] <- dig 
    }
  }
}

result <- as.character(sum(mems$number))
cat("Day 14 Part 1:", result)

# Part 2
mask <- NULL
mems <- data.frame(memory = NULL, number = NULL)
for(i in 1:length(fl)){
  rel <- fl[i]
  if(grepl("mask", rel)){
    mask <- unlist(strsplit(gsub("mask = ", "", rel), ""))
  }else{
    nms <- unlist(strsplit(gsub("mem\\[", "", rel), "] = "))
    mem <- as.numeric(nms[2])
    num <- c(rep("0", 4), as.numeric(rev(intToBits(nms[1]))))
    nmx <- mask == "X"
    num[nmx] <- "X"
    nm1 <- mask == "1"
    num[nm1] <- "1"
    xs <- which(num == "X")
    num[xs] <- 0
    base_number <- sum(2^c(35:0) * as.numeric(num))
    pos <- 2^(36 - xs)
    ex <- base_number
    
    for(j in pos){
      one <- ex
      two <- ex + j
      ex  <- c(one, two)
    }
    
    for(k in ex){
      ind <- which(mems$memory == k)
      if(length(ind) == 0){
        mems <- rbind(mems, data.frame(memory = k, number = mem))
      }else{
        mems$number[ind] <- mem 
      }
    }
  }
}
result <- as.character(sum(mems$number))
cat("Day 14 Part 2:", result)

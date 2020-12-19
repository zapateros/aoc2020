# I love this type of problems, where you are perfectly able to solve it by hand, but to write your actions in code is a whole different story.
# There is probably a nice numeric or recursive solution but I managed to put every manual step into code.
# For part one I have two steps I repeat until there are no brackets left anymore: replace the formula inside a clean bracket (no nests) and remove those brackets
# For part two you have to differ between multiplier and plus but the idea is similar: first you evaluate all plusses/pluses/plusi? (ha ha) where they are surrounded 
# by numbers (so '8+7' yes, '8+(3' no). Then you evaluate the formulas inside the clean bracket (with only multiplies if everything works correctly) and then remove the brackets
# For both parts you end up with a formula which you can easily evaluate and sum with the total

fl <- readLines("input_day_18.txt")
# Part 1
run <- lapply(fl, function(x){
  rel <- unlist(strsplit(gsub(" ", "", x),""))
  while(length(grep("\\(", rel)) > 0){
    # Clean brackets
    i <- 1
    while(i < (length(rel) -1)){
      inds <- i:(i + 3)
      pt   <- rel[inds]
      nms  <- grepl("[0-9]", pt)
      if(nms[2] & nms[4] & pt[1] == "(" ){
        x <- eval(parse(text = paste(pt[2:4], collapse = "")))
        rel[i + 1] <- x
        rel <- rel[-c(i + 2, i + 3)]
      }else{
        i <- i + 1
      }
    }
    
    # Remove brackets
    i <- 1
    while(i < (length(rel) - 1)){
      inds <- i:(i + 2)
      pt   <- rel[inds]
      if(pt[1] == "(" & pt[3] == ")"){
        rel <- rel[-c(i, i + 2)]
      }else{
        i <- i + 1
      }
    }
  }
  
  # evaluate resulting formula
  j <- 1
  while(length(rel) > 1){
    rl  <- rel[1:3]
    y   <- eval(parse(text = paste(rl, collapse = "")))
    rel[3] <- y
    rel <- rel[-c(1,2)]
  }
  res <- as.numeric(rel)
  res
})
result <- as.character(sum(unlist(run)))
cat("Day 18 Part 1:", result)

# Part 2
run <- lapply(fl, function(x){
  rel <- unlist(strsplit(gsub(" ", "", x),""))
  while(length(grep("\\(", rel)) > 0){
    # Remove plusses
    i <- 1
    while(i < (length(rel) -1)){
      inds <- i:(i + 2)
      pt   <- rel[inds]
      nms  <- grepl("[0-9]", pt)
      sig  <- pt[2] == "+"
      if(nms[1] & nms[3] & sig){
        x   <- eval(parse(text = paste(pt[1:3], collapse = "")))
        rel[i ] <- x
        rel <- rel[-c(i + 1, i + 2)]
      }else{
        i <- i + 1
      }
    }
    
    # evaluate formula inside brackets with only multiplies 
    lbef <- length(rel)
    laft <- 0
    while(lbef != laft){
      lbef <- length(rel)
      lbrs <- grep("\\(", rel)
      rbrs <- grep("\\)", rel)
      j <- 1
      while(TRUE){
        l1     <- lbrs[j]
        rbrs_t <- rbrs[rbrs > l1]
        ot     <- sapply(rbrs_t, function(x){sum(grepl("\\(", rel[l1:x])) == 1})
        if(any(ot)){
          r1 <- rbrs_t[min(which(ot))]
          break
        }else{
          j <- j + 1
        } 
      }
      pt <- rel[l1:r1]
      x  <- eval(parse(text = paste(pt, collapse = "")))
      if(length(pt) > 3){
        rel[l1 + 1] <- x
        rel <- rel[-c((l1 + 2):(r1 - 1))]
      }
      laft <- length(rel)
    }
    
    # Remove brackets
    i <- 1
    while(i < (length(rel) - 1)){
      inds <- i:(i + 2)
      pt   <- rel[inds]
      if(pt[1] == "(" & pt[3] == ")"){
        rel <- rel[-c(i, i + 2)]
      }else{
        i <- i + 1
      }
    }
  }
  
  # evaluate resulting formula
  i <- 1
  while(i < (length(rel) - 1)){
    inds <- i:(i + 2)
    pt   <- rel[inds]
    nms  <- grepl("[0-9]", pt)
    sig  <- pt[2] == "+"
    if(nms[1] & nms[3] & sig){
      x   <- eval(parse(text = paste(pt[1:3], collapse = "")))
      rel[i ] <- x
      rel <- rel[-c(i + 1, i + 2)]
    }else{
      i <- i + 1
    }
  }
  
  # evaluate resulting formula
  res <- eval(parse(text = paste(rel, collapse = "")))
  res
})
result <- as.character(sum(unlist(run)))
cat("Day 18 Part 2:", result)

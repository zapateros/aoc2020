# My solutions are getting uglier and uglier. 
# Today I was struggling with making a function that can be used for both parts. Then I coulnd't think of an elegant solution to stop the while loop (in part 2)
# from within a function, other than using a global variable. I learned you should think twice before using those.
# Now I'm looking at the function again, I see the part that makes the function usable for both problems is almost as large as the rest of the function. Makes you think
fl  <- readLines("input_day_08.txt")
n   <- length(fl)
end <- 0
run <- function(instr, problem){
  st <- NULL
  i  <- 1
  a  <- 0
  while(!i %in% st){
    st  <- c(st, i)
    x   <- instr[i]
    num <- as.numeric(gsub("[^-0-9]", "", x))
    if(grepl("acc", x)){
      a <- a + num
    }else if(grepl("jmp", x)){
      i <- i + num -1
    }
    i <- i + 1
    if(i %in% st){
      if(problem == 1){
        return(a)
      }else{
        return(a)
      }
      break
    }else if(i > n){
      cat("Day 8 Part 2:", a)
      end <<- 1
      break
    }
  }
}

# Part 1
result <- run(fl, 1)
cat("Day 8 Part 1:", result)

# Part 2
switches <- which(grepl("nop|jmp", fl))
end      <- 0
j        <- 1
while(end == 0){
  tfl <- fl
  if(grepl("nop", tfl[j])){
    tfl[j] <- gsub("nop", "jmp", tfl[j])
  }else{
    tfl[j] <- gsub("jmp", "nop", tfl[j])
  }
  run(tfl, 2)
  j <- j + 1
}

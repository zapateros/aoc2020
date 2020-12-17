# I suffered minor inconvenience with this one, as I couldn't find a fast solution. The brute forced solution (with a vector/matrix) worked in C# in 8.5 hours!
# However, I heard people were surprised about the rather easy solution for part 2, which should work in under a minute or so. I just couldn't find out how (without 
# looking at other's solutions of course). I tried a solution with a vector, where the all the consecutive numbers were stored: didn't work. Then I tried
# two vectors, where one is the index vector and the other the unique number vector: this one worked in 8.5 hours in C#; in R this would never work. 
# After I got the solution, I thought: hmm, let's just try the list method. This one worked in 51s. Apparantly a sparse list is not that difficult to work
# with by R, in comparison with a vector with NULL values.
fl  <- readLines("input_day_15.txt") 
nms <- as.numeric(unlist(strsplit(fl, ",")))

game <- function(nth_number){
  lis <- list()
  l   <- length(nms)
  for(j in 1:(l - 1)){
    lis[[nms[j] + 1]] <- j
  } 
  x <- nms[l] + 1
  i <- l
  while(i < nth_number){
    if(x > length(lis)){
      lis[[x]] <- i
      x <- 1
    }else if(is.null(lis[[x]])){
      lis[[x]] <- i
      x <- 1
    }else{
      y <- x
      x <- i - lis[[x]] + 1
      lis[[y]] <- i
    }
    i <- i + 1
  }
  x - 1
}

# Part 1
result <- game(2020)
cat("Day 15 Part 1:", result)

# Part 1
result <- game(30000000)
cat("Day 15 Part 2:", result)

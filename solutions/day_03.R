# Obviously a nice for-loop or an apply function should do the trick here, but I wanted to 
# solve it with a numeric series and a vector; turned out pretty neat
fl  <- readLines("C:/Users/paul/Documents/r-projects/aoc 2020/input/input_day_03.txt")
spl <- strsplit(fl, "")
l   <- length(spl)
w   <- length(unlist(spl[1]))
st  <- unlist(spl)

tree_slope <- function(step_down, step_right){
  sum(st[0:((l / step_down) - 1) * step_down * w + (0:((l / step_down) - 1) * step_right) %% w + 1] == "#")
}

# Part 1
result <- tree_slope(1, 3)
cat("Problem 3 part 1: ", result)

# Part 2
result <- prod(tree_slope(1, 1), tree_slope(1, 3), tree_slope(1, 5), tree_slope(1, 7), tree_slope(2, 1))
cat("Problem 3 Part 2: ", result)

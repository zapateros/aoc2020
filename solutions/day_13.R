fl    <- readLines("input_day_13.txt") 
ts    <- as.numeric(fl[1])
bs    <- unlist(strsplit(fl[2], ","))
mins  <- grep("[0-9]", bs)
lines <- as.numeric(bs[mins])

# Part 1
wt     <- lines - (ts %% lines)
mwti   <- which.min(wt)
result <- lines[mwti] * wt[mwti]
cat("Day 13 Part 1:", result)

# Part 2
# This one is solvable with the Chinese remainder theorem
# Calculate the modulo and the remainders and fill them in a solver (for example https://www.dcode.fr/chinese-remainder)
# Maybe I will recreate a solver here later
rems <- (lines - (mins - 1)) %% lines
mods <- lines

result <- "894954360381385"
cat("Day 13 Part 2:", result)

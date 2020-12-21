# This one frustrated me. Not because I didn't know what to do or because I didn't like the problem: I spent about 4 hours debugging.
# I was constantly in a split between trying to find another solution or continue debugging.
# In the end it was my grep function that was not working. Okay, it was exactly doing what it was supposed to do, but not for me
# When 'grepping' "nuts", it also counted/extracted/removed "peanuts" (same for "fish"/"shellfish"). 
# With the solution for part 1, I immediately had part 2. I'm happy now
fl <- readLines("input_day_21.txt")

# Part 1
allergens      <- lapply(gsub(".*contains |\\)|,","",  fl), function(x){unlist(strsplit(x, " "))})
ingredients    <- lapply(gsub(" \\(contains.*","",  fl), function(x){unlist(strsplit(x, " "))})
ul_ingredients <- unlist(ingredients)
l_ing          <- length(ingredients)
uni_all        <- unique(unlist(allergens, " "))
n              <- length(uni_all)
known          <- setNames(vector("list", length = n), uni_all)

i <- 1
while(length(unlist(known)) != n){
  rec <- allergens[[i]]
  if(length(rec) == 1){
    rel_inds <- which(sapply(allergens, function(x){any(x == rec)}))
    for(j in rel_inds){
      y <- ingredients[[i]] %in% ingredients[[j]]
      ingredients[[i]] <- ingredients[[i]][y]
    }
    z <- ingredients[[i]]
    if(length(z) == 1){
      # add allergen to known list
      known[[rec]] <- z
      # remove ingredient from all recipes
      for(k in grep(z, ingredients)){
        a <- ingredients[[k]]
        ingredients[[k]] <- a[!a == z]
      }
      b <- allergens[[i]]
      # remove allergen from all recipes
      for(k in grep(b, allergens)){
        d <- allergens[[k]]
        allergens[[k]] <- d[!d == b]
      }
    }
  }
  i <- i %% l_ing  + 1
}
xx     <- sapply(unlist(known), function(x){sum(ul_ingredients == x)})
result <- length(ul_ingredients) - sum(xx)
cat("Day 21 Part 1:", result)

# Part 2
result <- paste0(known[order(names(known))], collapse = ",")
cat("Day 21 Part 2:", result)

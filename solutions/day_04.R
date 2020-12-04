fl <- readLines("C:/Users/paul/Documents/r-projects/aoc 2020/input/input_day_04.txt")
fl[fl == ""] <- "/"
passports <- trimws(unlist(strsplit(paste0(fl, collapse =" "), "/")))
fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid")
fields_req <- fields[!fields == "cid"]

# Part 1
result <- sum(rowSums(sapply(fields_req, grepl, passports)) == length(fields_req))
cat("Problem 4 part 1: ", result)

# Part 2
check_digits <- function(z, min, max){
  if(grepl("^[0-9]*$", z) & nchar(z) == 4){
    num <- as.numeric(z)
    num >= min & num <= max
  }else{
    return(FALSE)
  }
}

byr <- function(z){
  check_digits(z, 1920, 2002)
}

iyr <- function(z){
  check_digits(z, 2010, 2020)
} 

eyr <- function(z){
  check_digits(z, 2020, 2030)
}

hgt <- function(z){
  num <- as.numeric(gsub("cm|in", "", z))
  if(grepl("cm", z)){
    num >= 150 & num <= 193
  }else if(grepl("in", z)){
    num >= 59 & num <= 76
  }else{
    return(FALSE)
  }
}

hcl <- function(z){
  if(substring(z, 1, 1) == "#" & nchar(z) == 7){
    grepl("^[0-9a-f]*$", gsub("#", "", z))
  }else{
    return(FALSE)
  }
}

ecl <- function(z){
  x <- unlist(gregexpr("amb|blu|brn|gry|grn|hzl|oth", z))
  if(x[1] != -1){
    length(x) == 1
  }else{
    return(FALSE)
  }
}

pid <- function(z){
  grepl("^[0-9]*$", z) & nchar(z) == 9
}

validation <- sapply(passports, function(g){
  y           <- unlist(strsplit(g, " "))
  field_names <- gsub(":.*", "", y)
  vals        <- gsub(".*:", "", y)
  if(all(fields_req %in% field_names)){
    trs <- sapply(fields_req, function(x){get(x)(vals[which(field_names == x)])})
    return(all(trs))
  }else{
    return(FALSE)
  }
})

result <- sum(validation)
cat("Problem 4 Part 2: ", result)

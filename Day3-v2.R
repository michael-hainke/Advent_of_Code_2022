# find match between 2 compartments 
find_match <- function(txt) {
  match(intersect( str_split ( str_sub(txt, 1, (nchar(txt)/2)), "")[[1]] , str_split ( str_sub(txt, -(nchar(txt)/2)), "")[[1]] ),
        c(letters[1:26],LETTERS[1:26]))
}

# find match between 3 elves
find_match2 <- function(one,two,three) {
  match(intersect(intersect(str_split(one,"")[[1]],str_split(two,"")[[1]]),str_split(three,"")[[1]]),
        c(letters[1:26],LETTERS[1:26]))
}

# read data
df <- readLines("day3_input.txt")

### Part 1
sum(sapply(df, find_match))

### Part 2
total <- 0
for (i in seq(1, length(df), 3)) {
  total <- total + find_match2(df[i],df[i+1],df[i+2])
} 
total

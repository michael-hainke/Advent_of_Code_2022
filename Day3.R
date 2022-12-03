
# find match between 2 compartments 
find_match <- function(txt) {
  pos = match( str_split ( str_sub(txt, 1, (nchar(txt)/2)), "")[[1]] , str_split ( str_sub(txt, -(nchar(txt)/2)), "")[[1]] )
  pos = pos[!is.na(pos)][1]
  match(str_sub(str_sub(txt, -(nchar(txt)/2)),pos,pos),c(letters[1:26],LETTERS[1:26]))
}

# find match between 3 elves
find_match2 <- function(one,two,three) {
  pos = match( str_split(one,"")[[1]],str_split(two,"")[[1]])
  pos = unique(pos[!is.na(pos)])
  pos2 = match( str_split(two,"")[[1]][pos],str_split(three,"")[[1]])
  pos2 = pos2[!is.na(pos2)][1]
  match(str_split(three,"")[[1]][pos2],c(letters[1:26],LETTERS[1:26]))
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
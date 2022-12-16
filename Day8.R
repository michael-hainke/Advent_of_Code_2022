library(tidyverse)

size = 99

# read data
df <- as.data.frame(str_split_fixed(readLines("day8_input.txt"),"",size)) %>%
      mutate_if(is.character, as.integer)

vis <- as.data.frame(matrix(1, ncol = size, nrow = size))
vis[2:(size-1),2:(size-1)] <- 0

check_rows <- function(rows,cols,dir,vis) {
  for (row in rows) {
    mx <- df[row,(cols[1]+dir)]
    for (col in cols) {
      if (df[row,col] > mx ) { vis[row,col] <- 1
                               mx = df[row,col] }
    }
  }
  return(vis)
}

check_columns <- function(cols,rows,dir,vis) {
  for (col in cols) {
    mx <- df[(rows[1]+dir),col]
    for (row in rows) {
      if (df[row,col] > mx ) { vis[row,col] <- 1
                               mx = df[row,col] } 
    }
  }
  return(vis)  
}

vis <- check_rows(c(2:(size-1)),c(2:(size-1)),-1,vis)
vis <- check_rows(c(2:(size-1)),c((size-1):2),1,vis) 
vis <- check_columns(c(2:(size-1)),c(2:(size-1)),-1,vis)
vis <- check_columns(c(2:(size-1)),c((size-1):2),1,vis)

# Part 1
sum(vis)

# Part 2

view <- matrix(0, ncol = size, nrow = size)

# Loop through each tree and measure view
for (row in 1:size) {
  for (col in 1:size) {
    # look left
    left = 0
    x = col
    while(x > 1) { x = x - 1
                   left = left + 1
                   if (df[row,col] <= df[row,x]) { break } }
    # look up
    up = 0
    y = row
    while(y > 1) { y = y - 1
                   up = up + 1
                   if (df[row,col] <= df[y,col]) { break } }
    # look right
    right = 0
    x = col
    while(x < size) { x = x + 1
                      right = right + 1
                      if (df[row,col] <= df[row,x]) { break } }
    # look down
    down = 0
    y = row
    while(y < size) { y = y + 1
                      down = down + 1
                      if (df[row,col] <= df[y,col]) { break } }
    view[row,col] = left * up * right * down   
  }
}

max(view)



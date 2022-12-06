# read data
txt <- readLines("day6_input.txt") |>
       strsplit("") |>
       unlist()

# find marker
marker <- function(txt,len) { length(unique(tail(txt,len))) == len }

# check message
check_message <- function(txt,len) { 
  for (n in len:length(txt)) { 
    if (marker(txt[1:n],len)) { return(n) }
  }
}

### Part 1: Find first start-of-packet marker
print(check_message(txt,4))

### Part 2: Find first start-of-message marker
print(check_message(txt,14))


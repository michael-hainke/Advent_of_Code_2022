library(tidyverse)

commands <- str_split(readLines("day10_input.txt")," ")

X <- 1

for (row in commands) {
  if ( unlist(row)[1] == 'noop' ) { X <- c(X, tail(X,1)) }
  if ( unlist(row)[1] == 'addx' ) { X <- c(X, tail(X,1), (tail(X,1)+as.integer(unlist(row)[2]))) }
}

# Part 1: sum of signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles
sum(X[20]*20,X[60]*60,X[100]*100,X[140]*140,X[180]*180,X[220]*220)

# Part 2: render image
display <- NULL
for (line in seq(from=1,to=240,by=40)) {
  Y <- NULL
  for (crt in 0:39) {
    if (abs(X[line+crt]-crt)<=1) { Y <- paste0(Y,'#')} else { Y <- paste0(Y, '.')}
  }
  if (is.null(display)) { display <- Y } else { display <- rbind(display,Y) }
}

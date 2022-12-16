library(tidyverse)

commands <- str_split(readLines("day10_input.txt")," ")
commands <- str_split(readLines("day10_test_input.txt")," ")

X <- 1

for (row in commands) {
  if ( unlist(row)[1] == 'noop' ) { X <- c(X, tail(X,1)) }
  if ( unlist(row)[1] == 'addx' ) { X <- c(X, tail(X,1), (tail(X,1)+as.integer(unlist(row)[2]))) }
}

# Part 1: sum of signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles
sum(X[20]*20,X[60]*60,X[100]*100,X[140]*140,X[180]*180,X[220]*220)

# Part 2: render image
Y <- NULL
for (crt in 0:239) {
  if (abs(X[crt+1]-crt)<=1) { Y <- c(Y,'#')} else { Y <- c(Y, '.')}
}

display <- as.data.frame(rbind(Y[1:40],Y[41:80],substring(Y,81,120),substring(Y,121,160),substring(Y,161,200),substring(Y,201,240)))

substring(Y,1,40)

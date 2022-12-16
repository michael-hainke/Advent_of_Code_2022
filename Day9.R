library(tidyverse)

moves <- str_split(readLines("day9_input.txt")," ")

rope_move <- function(rope, first, second) {
  vert_diff <- rope[[first]][2] - rope[[second]][2]
  horz_diff <- rope[[first]][1] - rope[[second]][1]
  if (vert_diff < 0) { vert_move <- floor(vert_diff/2) } else { vert_move <- ceiling(vert_diff/2) }
  if (horz_diff < 0) { horz_move <- floor(horz_diff/2) } else { horz_move <- ceiling(horz_diff/2) }
  if (abs(vert_diff) == 2 | abs(horz_diff) == 2) {
    rope[[second]][1] <- rope[[second]][1] + horz_move
    rope[[second]][2] <- rope[[second]][2] + vert_move
  }
  return(rope)
}

unique_tail_locations <- function(moves,rope) {
  tail_locs <- c(paste0(rope[[length(rope)]][1],',',rope[[length(rope)]][2]))
  for (i in 1:length(moves)) {
    dir <- moves[[i]][1]
    dist <- as.integer(moves[[i]][2])
    for (j in 1:dist) {
      if (dir %in% c('U','R')) { direction = 1 } else { direction = -1 }
      if (dir %in% c('U','D')) { rope[[1]][2] <- rope[[1]][2] + direction }
      if (dir %in% c('L','R')) { rope[[1]][1] <- rope[[1]][1] + direction }
      for (k in 1:(length(rope)-1)) {
        rope <- rope_move(rope, k, k+1)
        tail_locs <- c(tail_locs,paste0(rope[[length(rope)]][1],',',rope[[length(rope)]][2]))
      }
    }
  }
  length(unique(tail_locs))
}

# Part 1: how many locations has tail been (rope length 2)
rope <- list(c(1,1),c(1,1))
unique_tail_locations(moves,rope)

# Part 2: how many locations has tail been (rope length 10)
rope <- list(c(1,1),c(1,1),c(1,1),c(1,1),c(1,1),c(1,1),c(1,1),c(1,1),c(1,1),c(1,1))
unique_tail_locations(moves,rope)


library(tidyverse)

# read data
txt <- readLines("day3_input.txt")

# convert to dataframe with one letter per row
df <- data.frame(group = integer(), elf = integer(), compartment = integer(), letter = character())

for (elf in 1:length(txt)) {
  for (letter in 1:nchar(txt[elf])) {
    df <- rbind(df, list(group=ceiling(elf/3), elf=elf, compartment=ceiling(letter/nchar(txt[elf])*2), letter=str_sub(txt[elf],letter,letter))) 
  }
}

### Part 1
sum(match(df %>% distinct() %>% group_by(elf,letter) %>% tally() %>% filter(n==2) %>% pull(letter) , c(letters[1:26],LETTERS[1:26])))

### Part 2
sum(match(df %>% distinct() %>% group_by(group,letter) %>% tally() %>% filter(n==3) %>% pull(letter) , c(letters[1:26],LETTERS[1:26])))


# packages
library(tidyverse)

# read data
txt <- readLines("day1_input.txt")

df <- as.data.frame(list(elf = 0, cal = 0))
elf = 1

for (line in txt) {
  if (line == '') { elf = elf + 1 }
  else { df <- rbind(df,list(elf,as.numeric(line))) }
}

### Part 1 : Find Elf with most calories
df %>% group_by(elf) %>% summarise(calories = sum(cal)) %>% slice_max(calories) %>% pull(calories)

### Part 2 : Find top 3 Elves with most calories
df %>% group_by(elf) %>% summarise(calories = sum(cal)) %>% slice_max(calories, n=3) %>% pull(calories) %>% sum()

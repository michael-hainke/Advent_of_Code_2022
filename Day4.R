library(tidyverse)

# read data
txt <- str_split(readLines("day4_input.txt"),",|-")

# convert to dataframe
df <- data.frame(matrix(unlist(txt), nrow=length(txt), byrow=T))
colnames(df) <- c("S1","E1","S2","E2")
df <- df %>% mutate(S1 = as.integer(S1), E1 = as.integer(E1), S2 = as.integer(S2), E2 = as.integer(E2))

### Part 1
df %>% mutate(contained = (S1>=S2 & E1<=E2) | (S2>=S1 & E2<=E1)) %>% pull(contained) %>% sum()

### Part 2
df %>% mutate(overlap = (S1<=S2 & E1>=S2) | (S2<=S1 & E2>=S1)) %>% pull(overlap) %>% sum()


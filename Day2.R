# packages
library(tidyverse)

# read data
df <- as.data.frame(str_split_fixed(readLines("day2_input.txt")," ",2)) %>%
      mutate(V1 = as.numeric(match(V1,LETTERS[1:26])),
             V2 = as.numeric(match(V2,LETTERS[24:26])))

### Part 1
df %>%
mutate(score = case_when(V2-V1 == 0 ~ (V2 + 3),
                         (V2-V1) %in% c(1,-2) ~ (V2 + 6),
                         TRUE ~ V2)) %>%
pull(score) %>% sum()

### Part 2
df %>%
mutate(V2 = as.numeric(case_when(V2 == '1' ~ case_when(V1 == 1 ~ 3,
                                                       V1 == 2 ~ 1,
                                                       TRUE ~ 2),
                                 V2 == '2' ~ V1,
                                 TRUE ~ case_when(V1 == 1 ~ 2,
                                                  V1 == 2 ~ 3,
                                                  TRUE ~ 1)))) %>%
mutate(score = case_when(V2-V1 == 0 ~ (V2 + 3),
                         (V2-V1) %in% c(1,-2) ~ (V2 + 6),
                         TRUE ~ V2)) %>%
pull(score) %>% sum()



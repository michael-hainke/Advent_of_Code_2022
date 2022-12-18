library(tidyverse)

df <- readLines("day11_input.txt")

# parse data into dataframes
monkeys <- NULL
items <- NULL
for (line in seq(from=1,to=50,by=7)) {
  monkey <- data.frame(str_extract(df[line],regex("[0-9]")),
                 str_extract(df[line+2],regex("[+/*-]")),
                 str_extract(df[line+2],regex("[0-9]+")),
                 str_extract(df[line+3],regex("[0-9]+")),
                 str_extract(df[line+4],regex("[0-9]+")),
                 str_extract(df[line+5],regex("[0-9]+"))) 
  if (is.null(monkeys)) { monkeys <- monkey } else { monkeys <- rbind(monkeys,monkey) }
  item_list <- unlist(str_extract_all(df[line+1],regex("[0-9]+")))
  for (item in item_list) {
    if(is.null(items)) { items <- data.frame(list(monkey[1,1],item)) } else { items <- rbind(items,c(monkey[1,1],item))}
  }
}
names(monkeys) <- c('monkey','operation','amount','test','if_true','if_false')
monkeys <- monkeys %>% mutate_at(c('monkey','amount', 'test','if_true','if_false'), as.numeric) %>% mutate(inspections = 0)
names(items) <- c('monkey','item')
items <- items %>% mutate_at(c('monkey','item'), as.numeric)

divisor = prod(monkeys$test)

# monkey business function
monkey_business <- function(monkeys,items,rounds,worry_reduction) {
  for (round in 1:rounds) {
    for (monkey in monkeys$monkey) {
      cur_monkey <- monkeys %>% filter(monkey == {{monkey}})
      item_monkey <- items %>% filter(monkey == {{monkey}})
      for (item in item_monkey$item) {
        if (!is.na(cur_monkey$amount[1])) {
          if (cur_monkey$operation[1] == '+') { new = item + cur_monkey$amount[1] }
          if (cur_monkey$operation[1] == '*') { new = item * cur_monkey$amount[1] }
        } else                                { new = item * item }
        new = floor(new / worry_reduction)
        if (new %% cur_monkey$test[1] == 0) { items <- rbind(items, c(cur_monkey$if_true[1],new)) }
                                       else { items <- rbind(items, c(cur_monkey$if_false[1],new)) } 
        monkeys[monkeys$monkey == cur_monkey$monkey,'inspections'] = monkeys[monkeys$monkey == cur_monkey$monkey,'inspections'] + 1
      }
      items <- items %>% filter(!monkey == cur_monkey$monkey) %>% mutate(item = item %% divisor)
    } 
  }
  top_monkeys <- monkeys %>% slice_max(inspections,n=2) %>% pull(inspections)
  return(top_monkeys[1] * top_monkeys[2])
}

# Part 1
monkey_business(monkeys,items,20,3)

# Part 2
monkey_business(monkeys,items,10000,1)


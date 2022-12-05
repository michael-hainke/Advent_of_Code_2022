library(tidyverse)

# read data
txt <- readLines("day5_input.txt")

# get stack data
get_stacks <- function() { do.call(rbind,
                                   lapply(c(2,6,10,14,18,22,26,30,34),
                                   function(x) { paste0(rev(substr(txt[1:8],x,x)),collapse="") %>% str_trim() } )) }

# get procedures
get_procedures <- function() { do.call(rbind, lapply(txt[11:length(txt)], function(x) { c(str_sub(x, 6,7),
                                                                         str_sub(x, -6,-6),
                                                                         str_sub(x, -1,-1)) } )) }

# execute procedures
move_stack <- function(stacks,num,from,to,model) { move <- str_sub(stacks[from],-num,-1)
                                                   if (model == 9000 ) { move <- paste0(rev( move %>% str_split("") %>% unlist), collapse="") } 
                                                   stacks[to] <- paste0(stacks[to],move)
                                                   stacks[from] <- str_sub(stacks[from],1,-(num+1))
                                                   return(stacks)}

execute_procedures <- function(stacks, procedures, model) {
  for (procedure in 1:dim(procedures)[1]) {
    stacks <- move_stack(stacks,as.integer(procedures[procedure,1]),as.integer(procedures[procedure,2]),as.integer(procedures[procedure,3]),model)
  }
  return(stacks)
}

### Part 1: get top crate from each stack (model 9000)
paste0(unlist(lapply(seq(1:9), function(x) { str_sub(execute_procedures(get_stacks(),get_procedures(),9000)[x],-1) } )), collapse="")

### Part 2: get top crate from each stack (model 9001)
paste0(unlist(lapply(seq(1:9), function(x) { str_sub(execute_procedures(get_stacks(),get_procedures(),9001)[x],-1) } )), collapse="")


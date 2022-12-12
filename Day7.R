# read data
txt <- readLines("day7_input.txt") |>
       strsplit(" ")

# get size of all individual folders
df <- NULL
lvls <- NULL
first <- TRUE

for (row in txt) {
  row <- unlist(row)
  if (row[1] == '$' & row[2] == 'cd') { if (row[3] == '..') { lvls = tail(lvls,-1)
                                      } else if (row[3] == '/') { lvls = 'root'
                                      } else { lvls = c(row[3], lvls) } }
  if (row[1] == '$' & row[2] == 'ls') { if (!first) { if (!is.null(df)) { df <- rbind(df, c(cur_folder, size, parent_folder, level))
                                                    } else { df <- data.frame(cur_folder, size, parent_folder, level) } }
                                        first <- FALSE
                                        cur_folder <- paste(lvls,collapse='/')
                                        size <- 0
                                        if (length(lvls) == 1) {parent_folder = '' } else {parent_folder = paste(tail(lvls,-1),collapse='/') }
                                        level <- length(lvls)
                                      }  
  if (!row[1] %in% c('dir','$')) { size = size + as.numeric(row[1])}
  if (row[1] == 'dir') { child_folders = 1 }
}
df <-  rbind(df, c(cur_folder, size, parent_folder, level))
df$size = as.numeric(df$size)
df$level = as.integer(df$level)

# update folder sizes with all sub folders
for (level in max(df$level):2) {
  child_folders <- df[df$level==level,]
  for (folder in 1:length(child_folders$cur_folder)) {
    df_folder <- child_folders[folder,]
    df[df$cur_folder==df_folder$parent_folder,'size'] = df[df$cur_folder==df_folder$parent_folder,'size'] + df_folder$size    
  }
}

# Part 1: Find all of the directories with a total size of at most 100000. What is the sum of the total sizes of those directories?
sum(df[df$size<=100000,'size'])

# Part 2: Free up 30000000 space
space_required = 30000000 - (70000000 - df[df$level==1,'size'])
min(df[df$size > space_required,'size'])




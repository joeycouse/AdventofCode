library(tidyverse)

code <- read_lines('day6.txt')

parse_code <- function(code, start, end){
  
  str_sub(code, start = start, end = end)
  
}

code_pieces <- 
  tibble(start_char = seq(from = 1, to = 4092, by = 1),
       end_char = seq(from = 4, to = 4095, by = 1)) |> 
  rowwise() |> 
  mutate(code_piece = parse_code(code, start_char, end_char),
         code_start = length(unique(str_split(code_piece, pattern = '')[[1]])) == 4)

##### Part Two

message_pieces <- 
  tibble(start_char = seq(from = 1, to = 4082, by = 1),
         end_char = seq(from = 14, to = 4095, by = 1)) |> 
  rowwise() |> 
  mutate(code_piece = parse_code(code, start_char, end_char),
         code_start = length(unique(str_split(code_piece, pattern = '')[[1]])) == 14)


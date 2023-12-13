# Day 2
library(tidyverse)

input <- readLines("./2023/day2.txt", warn = F)
input <- gsub("Game [0-9]+\\:", replacement = '', input)

strsplit(input, split = ';')[[1]]

df <-
  tibble(input) |> 
  mutate(game_num = str_extract(input, regex("(?<=Game\\s)[0-9]{1,3}")) |> as.numeric(),
         max_red = str_extract_all(input, regex("[0-9]+(?=\\sred)")),
         max_red = map_dbl(max_red, \(x) as.numeric(x) |> max()),
         
         max_blue = str_extract_all(input, regex("[0-9]+(?=\\sblue)")),
         max_blue = map_dbl(max_blue, \(x) as.numeric(x) |> max()),
         
         max_green = str_extract_all(input, regex("[0-9]+(?=\\sgreen)")),
         max_green = map_dbl(max_green, \(x) as.numeric(x) |> max()),
         
         possible = if_else(max_red <= 12 & max_blue <= 14 & max_green <= 13, T, F)
  ) 

df |> 
  filter(possible == T) |> 
  pluck('game_num') |> 
  sum() 


# Part 2
df |> 
  mutate(power = max_red * max_blue * max_green) |> 
  pluck('power') |> 
  sum()


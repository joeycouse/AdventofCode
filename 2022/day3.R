library(tidyverse)

matches <- function(string, indexs){
  
  value <- c(letters, LETTERS)
  
  my_items <- c()
  
  for (i in indexs){
    
    item <- str_sub(string, start = i, end = i)  
    
    my_items <- c(my_items, item)
    
  }
  
  my_items
  
  items_value = which(value %in% my_items) |> sum()
  
  items_value
  
}

rucksacks <- 
  readLines('day3.txt') |> 
  as_tibble_col(column_name = 'rucksack')

rucks <-
  rucksacks |> 
  mutate(sack_size = str_length(rucksack)) |> 
  rowwise() |> 
  mutate(sack_one = str_sub(rucksack, 1, end = sack_size/2),
         sack_two = str_sub(rucksack, sack_size/2 + 1, end = -1),
         .keep = 'unused')|> 
  mutate(sharedItem = list(which(str_split(sack_one, pattern = '')[[1]] %in% str_split(sack_two, pattern = '')[[1]]))) |> 
  mutate(value = matches(sack_one, sharedItem))

rucks |> 
  ungroup() |> 
  summarise(total = sum(value))
  

# part 2

group_badges <- function(rucksacks){

  str1 <- rucksacks[[1]][[1]]
  str2 <- rucksacks[[1]][[2]]
  str3 <- rucksacks[[1]][[3]]


  first_match <- str_extract_all(str1, paste0('[', str2, ']')) |>  unlist() |> unique() |> paste0(collapse = '')
  final_match <- str_extract(first_match, paste0('[', str3, ']'))
  
  final_match

}


dat <- rucksacks |> 
  mutate(group = rep(1:100, each = 3)) |> 
  nest_by(group)|> 
  mutate(final_match = group_badges(data))|> 
  ungroup() |> 
  rowwise() |> 
  mutate(value = list(which(c(letters, LETTERS) %in% final_match))) |> 
  unnest(value) |> 
  summarise(total_value = sum(value))


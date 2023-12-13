library(tidyverse)

assignments <-
  read_csv('day4.txt', col_names = c('elf_one', 'elf_two')) |> 
  mutate(elf_one_start = str_extract(elf_one, '.+(?=\\-)'),
         elf_one_end = str_extract(elf_one, '(?<=\\-).+'),
         elf_two_start = str_extract(elf_two, '.+(?=\\-)'),
         elf_two_end = str_extract(elf_two, '(?<=\\-).+')) |> 
  rowwise() |> 
  mutate(elf_one_contained = between(elf_one_start, elf_two_start, elf_two_end) && between(elf_one_end, elf_two_start, elf_two_end),
         elf_two_contained = between(elf_two_start, elf_one_start, elf_one_end) && between(elf_two_end, elf_one_start, elf_one_end),
         someone_contained = elf_two_contained | elf_one_contained)|> 
  ungroup()

assignments |> 
  summarise(total_contained = sum(someone_contained))


### Part Two


assignments |> 
  select(elf_one_start:elf_two_end) |> 
  rowwise() |> 
  mutate(elf_one_overlap = between(elf_one_start, elf_two_start, elf_two_end) | between(elf_one_end, elf_two_start, elf_two_end),
         elf_two_overlap = between(elf_two_start, elf_one_start, elf_one_end) | between(elf_two_end, elf_one_start, elf_one_end),
         someone_overlap = elf_one_overlap | elf_two_overlap) |> 
  ungroup() |> 
  summarise(someone_overlap = sum(someone_overlap))

calories <- read_lines('calories.txt')

elves <- 
  tibble(calories) |> 
  mutate(elf = if_else(is.na(calories), 1, 0),
         elf = cumsum(elf)) |> 
  drop_na() |> 
  group_by(elf) |> 
  summarise(total_cal = sum(calories)) |> 
  ungroup() |> 
  arrange(desc(total_cal))

# Part 2

elves |> 
  slice_max(n = 3, order_by = total_cal) |> 
  summarise(all_three = sum(total_cal))

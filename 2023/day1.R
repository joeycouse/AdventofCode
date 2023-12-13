library(tidyverse)

lines <- readLines("./2023/day1.txt")

df <- 
  tibble(lines)

df |> 
  rowwise() |> 
  mutate(numbers = str_extract_all(lines, pattern = regex("[:digit:]")),
         numbers = str_flatten(numbers),
         numbers = if_else(nchar(numbers) > 2, paste0(str_sub(numbers, 1,1),str_sub(numbers, start = -1, end = -1), collapse = ''), numbers),
         numbers = if_else(nchar(numbers) < 2, paste0(numbers, numbers, collapse = ''), numbers)
         ) |> 
  ungroup() |> 
  summarise(total = sum(as.numeric(numbers)))


# Part 2
rev_pattern <- stringi::stri_reverse("one|two|three|four|five|six|seven|eight|nine|]:tigid:[")

rev_pattern

res <- df |> 
  rowwise() |> 
  mutate(reverse_order = stringi::stri_reverse(lines),
         first_number = str_extract(lines, pattern = regex("[:digit:]|one|two|three|four|five|six|seven|eight|nine")),
         last_number = str_extract(reverse_order, pattern = rev_pattern) |> stringi::stri_reverse(),
         first_number = str_replace_all(first_number, pattern = c("one" = "1",'two' = "2", "three" = "3", "four" = '4', 'five' = '5', 'six' = '6', 'seven' = '7', 'eight' = '8', 'nine' = '9')),
         last_number = str_replace_all(last_number, pattern = c("one" = "1",'two' = "2", "three" = "3", "four" = '4', 'five' = '5', 'six' = '6', 'seven' = '7', 'eight' = '8', 'nine' = '9')),
         final_number = paste0(first_number, last_number) |> as.numeric()
        ) |> 
  ungroup() |> 
  summarise(total = sum(final_number))


####
####

input <- readLines("./2023/day1.txt")

input |>
  stringr::str_extract_all("\\d") |>
  purrr::map_chr(~ paste0(head(.x, 1), tail(.x, 1))) |>
  as.integer() |>
  sum()

numbers <- c(
  "one" = 1, "two" = 2, "three" = 3, "four" = 4, "five" = 5,
  "six" = 6, "seven" = 7, "eight" = 8, "nine" = 9,
  setNames(nm = 1:9)
)

srebmun <- numbers
names(srebmun) <- stringi::stri_reverse(names(srebmun))

get_digit <- function(x, ref) {
  res <- x |>
    stringr::str_extract(paste0("(", paste0(names(ref), collapse = "|"), ")"))
  
  ref[res]
}

sum(
  get_digit(input, numbers) * 10 +
    get_digit(stringi::stri_reverse(input), srebmun)
)

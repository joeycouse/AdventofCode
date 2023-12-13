# Day 6
# 
library(tidyverse)

input <- read.delim("./2023/day6.txt", sep = '', header = F, row.names = 1) |> 
  t() |> 
  as.data.frame()
rownames(input) <- NULL
colnames(input) <- c('time', 'distance')

hold_time_calc <- function(time, distance, type){
  c = -distance
  b = time
  a = -1

  switch(type,
         max = floor(((-b)-sqrt(b^2 - 4*a*c))/(2*a)),
         min = ceiling(((-b)+sqrt(b^2 - 4*a*c))/(2*a)))
  
}

res <- 
  input |> 
  mutate(
    min_hold = hold_time_calc(time, distance, "min"),
    max_hold = hold_time_calc(time, distance, "max"),
    total_options = max_hold - min_hold + 1
)

prod(res$total_options)

# Part 2

part2<-readLines("./2023/day6.txt")
part2 <- str_remove_all(part2, pattern = regex("[A-z:\\s]")) |> as.numeric()

hold_time_calc(part2[1], part2[2], type = 'max') - hold_time_calc(part2[1], part2[2], type = 'min') +1


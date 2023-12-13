library(tidyverse)

dat <- 
  read_delim('rock_paper_scissors.txt', delim = ' ', col_names = c('player_one', 'player_two'))

points <-
  tribble(~player_two, ~points,
          'rock', 1,
          'paper', 2,
          'scissors', 3)

get_outcome <- function(player_one, player_two){
  
  if (player_one == player_two){
    return(3)
  } else if (player_one == 'rock' && player_two == 'paper'){
    return(6)
  } else if (player_one == 'paper' && player_two == 'scissors'){
    return(6)
  } else if (player_one == 'scissors' && player_two == 'rock'){
    return(6)
  } else {
    return(0)
  }
  
}

dat |> 
  mutate(player_one = case_when(player_one == 'A' ~ 'rock',
                                player_one == 'B' ~ 'paper',
                                player_one == 'C' ~ 'scissors'),
         player_two = case_when(player_two == 'X' ~ 'rock',
                                player_two == 'Y' ~ 'paper',
                                player_two == 'Z' ~ 'scissors')) |> 
  left_join(points) |> 
  rowwise() |> 
  mutate(outcome_points = get_outcome(player_one, player_two),
         total_points = points + outcome_points) |> 
  ungroup() |> 
  summarise(overall_score = sum(total_points))

#### Part Two

get_play <- function(player_one, outcome){
  
  if (outcome == 'draw'){
    return(player_one)
  } else if (outcome == 'win') {
    
    if (player_one == 'rock') {
      return('paper')
    } else if (player_one == 'paper') {
      return('scissors')
    } else {
      return('rock')
    }
  
  } else {
    
    if (player_one == 'rock') {
      return('scissors')
    } else if (player_one == 'paper') {
      return('rock')
    } else {
      return('paper')
    }
    
  }
  
}


dat |> 
  mutate(player_one = case_when(player_one == 'A' ~ 'rock',
                                player_one == 'B' ~ 'paper',
                                player_one == 'C' ~ 'scissors'),
         outcome = case_when(player_two == 'X' ~ 'lose',
                             player_two == 'Y' ~ 'draw',
                             player_two == 'Z' ~ 'win')
         ) |> 
  rowwise() |> 
  mutate(player_two = get_play(player_one, outcome)) |> 
  left_join(points) |> 
  mutate(outcome_points = get_outcome(player_one, player_two),
         total_points = points + outcome_points) |> 
  ungroup() |> 
  summarise(overall_score = sum(total_points))
  

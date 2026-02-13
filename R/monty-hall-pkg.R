#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'  Select the contestant's initial door
#' @description
#'  Randomly selects one of three doors (1,2, or 3) as the contestant's initial door.
#' @return 
#'  Returns an integer of 1, 2, or 3
#' @examples
#'   select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  Host opens a goat door
#' @description
#'  Decides which door the host opens after the contestant's initial door is selected.
#'  Always reveals a goat.
#' @param game A character vector of length 3 created by create_game().
#' @param a.pick An integer of 1, 2, or 3 indicating the contestant's initial pick.
#' @return 
#'  An integer 1, 2, or 3 indicating which door the host opens.
#' @examples
#' game <- create_game()
#' a.pick <- select_door()
#' open_goat_door(game, a.pick)
#' @export
#' 
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'  Contestant stays with their door or switches
#' @description
#' Determines the contestant's final door choice based on whether they
#' choose to stay with the original pick or switch to the remaining unopened door.
#' @param stay If TRUE, keep the original pick. If FALSE, switch doors.
#' @param opened.door An integer of 1, 2, or 3 indicating the door opened by the host.
#' @param a.pick An integer of 1, 2, or 3 indicating the contestant's initial pick.
#' @return 
#'  An integer 1, 2, or 3 indicating the contestant's final pick.
#' @examples
#' game <- create_game()
#' a.pick <- select_door()
#' opened.door <- open_goat_door(game, a.pick)
#' change_door(TRUE, opened.door, a.pick)
#' change_door(FALSE, opened.door, a.pick)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'  Determine outcome of game
#' @description
#'  Evaluates whether the contestant's final pick results in winning the car or not
#' @param final.pick An integer of 1, 2, or 3 indicating the contestant's final pick.
#' @param game A character vector of length 3 created by create_game().
#' @return 
#' A character string: "WIN" if the final pick contains the car,
#' otherwise "LOSE".
#' @examples
#'   game <- create_game()
#'   determine_winner(1, game)
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title 
#'  Play a Monty Hall game
#' @description
#'  Simulates a single Monty Hall game and evaluates both the stay and switch strategies.
#' @return 
#'  A data frame with two rows (stay and switch) and a column
#' indicating whether the outcome was "WIN" or "LOSE".
#' @examples
#'   play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'  Simulate multiple Monty Hall games
#' @description
#'  Repeats play_game() n times, prints the proportion of wins and losses
#' for each strategy, and returns the combined results.
#' @param n Integer. The number of games to simulate.
#' @return 
#'  A data frame containing the outcomes for all simulated games.
#' @examples
#'   play_n_games(10)
#' @export
play_n_games <- function( n=100 )
{
  
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  print(round(prop.table(table(results.df), margin = 1), 2))
  
  return( results.df )

}

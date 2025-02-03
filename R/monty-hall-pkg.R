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
#' @param ... no arguments are used by the function.
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
#'   Select a door.
#'
#' @description
#'   `select_door()` allows a contestant to choose 1 door.
#'
#' @details
#'   The contestant chooses one door out of the three. 
#'
#' @param 
#'   Numeric
#' 
#' @return The function returns a numeric value that indicates 
#'         which door was picked.
#'
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
#'   Opening the goat door
#' @description
#'   'open_goat_door()' chooses a goat door if the car door is selected.
#' @details
#'   If the contestant selects a car door, one of the two goat 
#'   doors are randomly selected.
#' @param 
#'   Numeric
#' @return 
#'   The function will be a numeric value that indicates a door.
#' @examples
#'   open_goat_door()
#' @export
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
#'   Switching doors
#' @description
#'   'change_door()' reflects the choices when contestant wants 
#'    the change their choice.
#' @details
#'   If the contestant stays with their choice, the final pick is the same.
#'   If the contestant switches, the choice will be one of the remaining door.
#' @param 
#'   Numeric
#' @return 
#'   The function will be a numeric value that indicates a door.
#' @examples
#'  change_door()
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
#'   Determine the winner
#' @description
#'   'determine_winner()' lets us know if the contestant is the winner.
#' @details
#'   If the final choice is a car, the contestant wins. If it is a goat, the
#'   contestant loses.
#' @param
#'   logical
#' @return 
#'   The function will be true or false, which is defined as win or lose.
#' @examples
#'   determine_winner()
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
#'   Game Results
#' @description
#'   'play_game()' function shows us the game results with the two strategies
#'   of stay and switch. 
#' @details
#'   There are two strategies of the game, which results in different outcomes.
#' @param 
#'   logical 
#' @return 
#'   The function will be true or false, which is defined as win or lose.
#' @examples
#'    play_game()
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
#'   Probability Table
#' @description
#'   'play_n_games()' function lets us see the probability charts for
#'   the wins and losses of the game.
#' @details
#'   This shows us the probability when 100 games are played. 
#' @param 
#'   Numeric
#' @return 
#'   The function will be a numeric value that shows the win and loss rates. 
#' @examples
#'    play_n_games()
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}

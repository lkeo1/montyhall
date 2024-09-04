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
#'   CONTESTANT SELECTS A DOOR
#' @description
#'  The contestant makes their first selection at random.
#' @details
#'  This function will simulate the contestant's initial choice in the Monty Hall game by selecting at random one of the three doors. 
#' @param ... no arguments are used by the function
#' @return 
#'  The function returns an integer representing the door number chosen by the contestant. 
#' @examples
#'  select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  HOST OPENS GOAT DOOR
#' @description
#'  The host will open a door that reveals a goat based on the contestant's first choice.
#' @details
#'  This function will determine which door the host will open to reveal a goat. 
#'  In the case that the contestant chose the door with a car, the host will randomly
#'  select one of the two doors with a goat. In the case that the contestant chose the 
#'  door with a goat, the host will select and open the only remaining goat door. 
#' @param 
#'  The game vector indicates the status of each door ("car" or "goat")
#'  The a.pick vector is the door number that the contestant initially chose (1,2, or 3)
#' @return 
#'  This will return the door number that the host opened to reveal the goat (1,2, or 3)
#' @examples
#' open_goat_door(game = c("car", "goat", "goat"), a.pick = 1)
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
#'  CHANGE DOORS
#' @description
#'  The contestant is given the option to change their initial selection 
#'  to the remaining closed door. 
#' @details
#'  This function decides the contestant's final door choice. STAY = TRUE 
#'  if the contestant keeps their initial choice. STAY = FALSE if the contestant
#'  switches to the remaining unopened door. 
#' @param 
#'  'stay' indicates whether the contestant should stay with their initial pick (TRUE) 
#'  or switch to the other remaining door (FALSE).
#'  'opened.door' is the door the host opened to reveal a goat (1,2 or 3)
#'  'a.pick' is the initial door chosen by the contestant (1,2, or 3)
#' @return 
#'  The final door number that is chosen by the contestant (1,2 or 3)
#' @examples
#'  change_door(stay = TRUE, opened.door = 2, a.pick = 1)
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
#'  DETERMINE IF CONTESTANT HAS WON
#' @description
#'  This will determine if the contestant won or lost based on the 
#'  contestant's choice. 
#' @details
#'  This function checks if the final door choice in the game has the 
#'  contestant has won (chose the car) or lost (chose a goat). 
#' @param 
#' 'final.pick' is the final door that the contestant chose (1,2, or 3)
#' 'game' shows the status of each door whether it is car or goat.
#' @return 
#'  This indicates the result of the game. "WIN" if contestant chooses the car 
#'  and "LOSE" if contestant chooses the goat. 
#' @examples
#' determine_winner(final.pick = 1, game = c("car", "goat", "goat"))
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
#'  Play Game
#' @description
#' This will simulate a Monty Hall game.
#' @details
#' This function will simulate a Monty Hall game. This includes all the 
#' steps of contestant choosing an initial door, the host opening a goat
#' door,and if the contestant chooses to stay with their initial pick or 
#' switch.
#' @param ... no arguments are used by the function.
#' @return 
#'  A data frame with the strategy ("stay" or "switch") 
#'  and the corresponding outcome ("WIN" or "LOSE")
#' @examples
#' play_game()
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
#' Playing Multiple Games 
#' @description
#' Simulates multiple games of Monty Hall 
#' and calculates the proportion of wins for each strategy.
#' @details
#' This function runs a specified number of Monty Hall games and computes 
#' the proportion of wins for both the stay and switch strategies. 
#' It returns a data frame with the results and prints the proportions.
#' @param 
#' n An integer specifying the number of games to simulate. Default is 100
#' @return 
#' A data frame with the results of all games. 
#' The proportions of wins for each strategy are also printed.
#' @examples
#' play_n_games(n = 100)
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

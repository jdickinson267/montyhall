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
#' Contestant Selects A Door
#'
#' @description
#' 'select_door' function sets up the possible door selections and makes a
#' selection at random.
#'
#' @details
#' This step in the game is when a contestant sees the amount of doors in the
#' game and chooses a door at random.  The function will return the door selection.
#'
#' @param
#' No arguments used in this function.
#'
#' @return
#' The function returns a length 1 character vector indicating the position of
#' the chosen door.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host Opens Goat Door
#'
#' @description
#''open_goat_door' function provides the position of one of the doors with a goat.
#'
#' @details
#' This step in the game is when the host opens one of the doors that is not a
#' winning door, reducing the remaining choice of doors for the contestant to
#' choose from.
#'
#' @param
#' The parameters to this function include the amount of doors and the position
#' of the cars and goats in the game.
#'
#' @return
#' The function returns a length 1 character vector indicating the position of one
#' of the goats in the game.
#'
#' @examples
#' this.game <- c("goat","car","goat")
#' my.initial.pick <- 1
#' open_goat_door( this.game, my.initial.pick )
#'
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
#' Change Doors
#'
#' @description
#' 'change_door' function allows the contestant the option to change their choice
#' in doors when they have already opened one of the losing doors.
#'
#' @details
#' This step in the game is when the contestant has to decide to stay or switch to
#' a new door choice before the final prize reveal.
#'
#' @param
#' The parameters to this function include the amount of doors in the game and whether
#' or not the contestant chooses to keep their current door choice.
#'
#' @return
#' The function returns a length 1 character vector indicating the door choice.
#'
#' @examples
#' change_door( stay=T, opened.door=1, a.pick=3 )
#'
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
#' Determine if Contestant Has Won
#'
#' @description
#' 'determine_winner' function returns whether or not the final door choice is
#' the position of the car.
#'
#' @details
#' This step in the game is the final reveal of the doors and whether the contestant
#' has won the car or not.
#'
#' @param
#' The parameters to this function include the previous steps being completed to provide
#' the game's results.
#'
#' @return
#' The function returns a character string of "LOSE" or "WIN".
#'
#' @examples
#' this.game <- c("goat","car","goat")
#'determine_winner( final.pick=1, game=this.game )
#'
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
#' Full Game Test
#'
#' @description
#' 'play_game' function combines all of the previous steps into one function to play
#' a full game and provide the resulting outcomes.
#'
#' @details
#' This would be a full game played without individual steps, providing a table of the strategy
#' and the resulting outcome based on that choice.
#'
#' @param
#' No arguments used in this function.
#'
#' @return
#' The function returns a table of the strategies and the results of either "LOSE" or "WIN".
#'
#' @examples
#' play_game( )
#'
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
#' Adding the Game to a Loop
#'
#' @description
#' "play_n_games' function plays a certain number of games and provides the
#' probability of how likely the strategy will result in each outcome.
#'
#' @details
#' This would be a full game played without individual steps, providing a table of the strategy
#' and the resulting probability of each outcome based on that choice.
#'
#' @param
#' The parameters to this function include the previous steps being completed to provide
#' the game's probability of each result when ran n times.
#'
#' @return
#' The function returns a table of the strategies and the probability of either "LOSE" or "WIN".
#'
#' @examples
#' play_n_games( )
#'
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

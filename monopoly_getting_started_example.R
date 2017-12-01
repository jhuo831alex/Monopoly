gameboard <- data.frame(space = 1:40, title = c("Go" , "Mediterranean Avenue" , "Community Chest" , "Baltic Avenue" , "Income Tax" , "Reading Railroad" , "Oriental Avenue" , "Chance" , "Vermont Avenue" , "Connecticut Avenue" , "Jail" , "St. Charles Place" , "Electric Company" , "States Avenue" , "Virginia Avenue" , "Pennsylvania Railroad" , "St. James Place" , "Community Chest" , "Tennessee Avenue" , "New York Avenue" , "Free Parking" , "Kentucky Avenue" , "Chance" , "Indiana Avenue" , "Illinois Avenue" , "B & O Railroad" , "Atlantic Avenue" , "Ventnor Avenue" , "Water Works" , "Marvin Gardens" , "Go to jail" , "Pacific Avenue" , "North Carolina Avenue" , "Community Chest" , "Pennsylvania Avenue" , "Short Line Railroad" , "Chance" , "Park Place" , "Luxury Tax" , "Boardwalk"))
chancedeck <- data.frame(index = 1:15, card = c("Advance to Go" , "Advance to Illinois Ave." , "Advance to St. Charles Place" , "Advance token to nearest Utility" , "Advance token to the nearest Railroad" , "Take a ride on the Reading Railroad" , "Take a walk on the Boardwalk" , "Go to Jail" , "Go Back 3 Spaces" , "Bank pays you dividend of $50" , "Get out of Jail Free" , "Make general repairs on all your property" , "Pay poor tax of $15" , "You have been elected Chairman of the Board" , "Your building loan matures"))
communitydeck <- data.frame(index = 1:16, card = c("Advance to Go" , "Go to Jail" , "Bank error in your favor. Collect $200" , "Doctor's fees Pay $50" , "From sale of stock you get $45" , "Get Out of Jail Free" , "Grand Opera Night Opening" , "Xmas Fund matures" , "Income tax refund" , "Life insurance matures. Collect $100" , "Pay hospital fees of $100" , "Pay school tax of $150" , "Receive for services $25" , "You are assessed for street repairs" , "You have won second prize in a beauty contest" , "You inherit $100"))

### To get you started, here is a simple function to roll two dice.

# Dice --------------------------------------------------------------------

dice <- function(verbose=FALSE){
  faces <- sample(1:6, 2, replace=TRUE)
  if(faces[1] == faces[2]) doubles = TRUE
  else doubles = FALSE
  movement = sum(faces)
  if(verbose) cat("Rolled:", faces[1], faces[2], "\n")
  return(list(faces=faces, doubles=doubles, movement=movement))
}

# Manual Dice -------------------------------------------------------------

# this function allows you to set some manual dice

Dice = setRefClass("Dice", 
                   fields = list(
                     rolls = "numeric",
                     pos = "numeric",
                     verbose = "logical"
                   ), 
                   methods = list(
                     roll = function() {
                       faces = rolls[pos + seq_len(2)]
                       pos <<- pos + 2
                       if(faces[1] == faces[2]) doubles = TRUE
                       else doubles = FALSE
                       movement = sum(faces)
                       if(verbose) cat("Rolled:", faces[1], faces[2], "\n")
                       return(list(faces=faces, doubles=doubles, movement=movement))
                     }
                   )
)

# I highly recommend the use of a reference class to keep track of each player. 
# I've created a *very basic* reference class with a pair of methods to get you started. 
# This starting reference class has only one field in it and only two methods 
# You'll definitely want to add more fields and methods, but I'll let you figure out and decide what those are.


# Player Reference Class --------------------------------------------------


# a **very basic** reference class for our players
player <- setRefClass("player", 
  fields = list(
    pos = "numeric",      # position on the board
    verbose = "logical"
  ), 
  methods = list(
    move_n = function(n) {
      if(verbose) cat("Player at:", pos)
      if(verbose) cat(" Player moves:", n)
      pos <<- pos + n
      if(pos > 40) pos <<- pos - 40
      if(verbose) cat(" Player now at:", pos,"\n")
    },
    go_2_space_n = function(n){
      if(verbose) cat("Player at:", pos,".")
      pos <<- n
      if(verbose) cat(" Player now at:", pos,".\n")
    }
  )
)

player1 <- player$new(pos = 1, verbose = TRUE)  # create new players
player2 <- player$new(pos = 1, verbose = TRUE)


# Space Tracking Reference Class ------------------------------------------

# a *basic* reference class to keep track of where people landed
tracking <- setRefClass("tracking",
  fields = list(
    tally = "numeric"
  ),
  methods = list(
    increase_count = function(n){
      tally[n] <<- tally[n] + 1
    }
  )
)

space_tracking <- tracking$new(tally = rep(0,40))


# Taking a turn -----------------------------------------------------------

taketurn <- function(player, tracking){
  roll <- dice()
  # roll <- dice(verbose = TRUE)  # this will only work if you are not using the manual dice
  player$move_n(roll$movement)
  tracking$increase_count(player$pos)
}


# testing one turn --------------------------------------------------------

set.seed(1)

taketurn(player1, space_tracking)  # roll a 2,3 the player is now on space 6
taketurn(player2, space_tracking)  # roll a 4,6 the player is now on space 11

# check to verify
space_tracking$tally  ## tallys have been updated to show that spot 6 and spot 11 have been landed on 1 time each
player1  # shows that player 1 is on spot 6

# Running the simulation --------------------------------------------------

set.seed(1)

space_tracking <- tracking$new(tally = rep(0,40),verbose=FALSE)
for(i in 1:100){ # simulate 100 games
  cat("#### NEW GAME",i,"##### \n")
  player1 <- player$new(pos = 1, verbose = FALSE)  # new players for each game
  player2 <- player$new(pos = 1, verbose = FALSE)
  for(i in 1:150){ # 150 turns for each game
    if(player1$verbose) cat("Player 1 turn\n")
    taketurn(player1, space_tracking)  
    if(player2$verbose) cat("Player 2 turn\n")
    taketurn(player2, space_tracking)  
  }
}
# the results after 100 turns. No rules have been implemented
results <- cbind(gameboard, tally = space_tracking$tally)
results <- cbind(results, rel = results$tally/sum(results$tally))
print(results)
sum(results$tally)


# Example Using Manual Dice (Useful for testing) --------------------------

set.seed(10)
setdice <- Dice$new(rolls = c(6, 4, 5, 3, 3, 5, 6, 2, 5, 4, 4, 1, 2, 6, 4, 4, 4, 4, 2, 2, 
                              4, 3, 4, 4, 1, 4, 3, 4, 1, 2, 3, 6, 5, 4, 5, 5, 1, 2, 5, 4, 
                              3, 3, 1, 1, 2, 1, 1, 3),
                    pos = 0, verbose = TRUE)
dice <- function() setdice$roll()
space_tracking <- tracking$new(tally = rep(0,40))
player1 <- player$new(pos = 1, verbose = TRUE)  # new players for each game
for(i in 1:20){ # 100 turns for each game
  cat("\n## Turn", i,"\n")
  taketurn(player1, space_tracking) 
}

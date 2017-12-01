gameboard <- data.frame(space = 1:40, title = c("Go" , "Mediterranean Avenue" , "Community Chest" , "Baltic Avenue" , "Income Tax" , "Reading Railroad" , "Oriental Avenue" , "Chance" , "Vermont Avenue" , "Connecticut Avenue" , "Jail" , "St. Charles Place" , "Electric Company" , "States Avenue" , "Virginia Avenue" , "Pennsylvania Railroad" , "St. James Place" , "Community Chest" , "Tennessee Avenue" , "New York Avenue" , "Free Parking" , "Kentucky Avenue" , "Chance" , "Indiana Avenue" , "Illinois Avenue" , "B & O Railroad" , "Atlantic Avenue" , "Ventnor Avenue" , "Water Works" , "Marvin Gardens" , "Go to jail" , "Pacific Avenue" , "North Carolina Avenue" , "Community Chest" , "Pennsylvania Avenue" , "Short Line Railroad" , "Chance" , "Park Place" , "Luxury Tax" , "Boardwalk"),stringsAsFactors = FALSE)
chancedeck <- data.frame(index = 1:15, card = c("Advance to Go" , "Advance to Illinois Ave." , "Advance to St. Charles Place" , "Advance token to nearest Utility" , "Advance token to the nearest Railroad" , "Take a ride on the Reading Railroad" , "Take a walk on the Boardwalk" , "Go to Jail" , "Go Back 3 Spaces" , "Bank pays you dividend of $50" , "Get out of Jail Free" , "Make general repairs on all your property" , "Pay poor tax of $15" , "You have been elected Chairman of the Board" , "Your building loan matures"),stringsAsFactors = FALSE)
communitydeck <- data.frame(index = 1:16, card = c("Advance to Go" , "Go to Jail" , "Bank error in your favor. Collect $200" , "Doctor's fees Pay $50" , "From sale of stock you get $45" , "Get Out of Jail Free" , "Grand Opera Night Opening" , "Xmas Fund matures" , "Income tax refund" , "Life insurance matures. Collect $100" , "Pay hospital fees of $100" , "Pay school tax of $150" , "Receive for services $25" , "You are assessed for street repairs" , "You have won second prize in a beauty contest" , "You inherit $100"),stringsAsFactors = FALSE)


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

# Player Reference Class --------------------------------------------------

player <- setRefClass("player", 
                      fields = list(
                        pos = "numeric",      # position on the board
                        verbose = "logical",
                        doublecount = "numeric",
                        jailstate = "logical",
                        jailcount = "numeric"
                      ), 
                      methods = list(
                        move_n = function(n) {
                          if(verbose) cat("Player at: ", pos," ",gameboard[pos,2],".",sep="")
                          if(verbose) cat(" Player moves:", n,"\n")
                          pos <<- pos + n
                          if(pos > 40) pos <<- pos - 40
                          if(verbose) cat("Player now at: ", pos,", ",gameboard[pos,2],".\n",sep="")
                          if(pos==31) go_to_jail()
                        },
                        go_2_space_n = function(n){
                          if(verbose) cat("Player at:", pos,".")
                          pos <<- n
                          if(verbose) cat(" Player now at:", pos,".\n")
                          if(pos==31) go_to_jail()
                        },
                        go_to_jail = function(){
                          if(verbose) cat("Going to jail.\n")
                          pos <<- 11
                          jailstate <<- TRUE
                        },
                        increase_jailcount = function(){
                          jailcount <<- jailcount+1
                          if(verbose) cat("Jail turn:",jailcount,"\n")
                        },
                        getting_out_of_jail = function(){
                          jailstate <<- FALSE
                          if(verbose) cat("Getting out of Jail.\n")
                        },
                        clear_jailcount = function(){
                          jailcount<<-0
                        },
                        increase_doublecount = function(){
                          doublecount <<- doublecount+1
                          if(verbose) cat("Double count is now:",doublecount,"\n")
                        },
                        clear_doublecount = function(){
                          doublecount <<- 0
                        }
                      )
)

# Tracking Reference Class ------------------------------------------------

tracking <- setRefClass("tracking",
                        fields = list(
                          tally = "numeric",
                          verbose = "logical"
                        ),
                        methods = list(
                          increase_count = function(n){
                            tally[n] <<- tally[n] + 1
                            if(verbose) cat("Tally at",n,gameboard[n,2],"\n")
                          }
                        )
)

# chancechest function ----------------------------------------------------

chancechest <- function(player,tracking,verbose=FALSE){
    chancecard <- sample(1:15,1)
    old_pos = player$pos
    if(verbose) cat("Drew Chance card\n","Chance Card ",chancecard," ",chancedeck[chancecard,2],"\n",sep="")
    if(chancecard==1) player$go_2_space_n(1)
    if(chancecard==2) player$go_2_space_n(25)
    if(chancecard==3) player$go_2_space_n(12)
    if(chancecard==4){
      if(player$pos==8||player$pos==37) player$go_2_space_n(13)
      if(player$pos==23) player$go_2_space_n(29)
    }
    if(chancecard==5){
      if(player$pos==8) player$go_2_space_n(16)
      if(player$pos==23) player$go_2_space_n(26)
      if(player$pos==37) player$go_2_space_n(6)
    }
    if(chancecard==6) player$go_2_space_n(6)
    if(chancecard==7) player$go_2_space_n(40)
    if(chancecard==8) player$go_to_jail()
    if(chancecard==9) player$go_2_space_n(player$pos-3)
    if(old_pos!=player$pos) {tracking$increase_count(player$pos)}
}

# communitychest function ------------------------------------------------------

communitychest <- function(player,tracking,verbose=FALSE){
  communitycard <- sample(1:16,1)
  old_pos = player$pos
  if(verbose) cat("Drew Community card\n","Community Card ",communitycard," ",communitydeck[communitycard,2],"\n",sep="")
  if(communitycard==1) player$go_2_space_n(1)
  if(communitycard==2) player$go_to_jail()
  if(old_pos!=player$pos) {tracking$increase_count(player$pos)}
}

# taketurn function -------------------------------------------------------

taketurn <- function(player,tracking,verbose=FALSE){
  roll <- dice()
  afterjail_double = FALSE
  
  if(player$jailstate && player$jailcount <= 2){
    if(roll$doubles){
      player$getting_out_of_jail()
      afterjail_double = TRUE
    }else{
      player$increase_jailcount()
      if(player$jailcount!=3) {tracking$increase_count(11)}
    }
  }
  
  if(player$jailstate && player$jailcount == 3){
    player$getting_out_of_jail()
    afterjail_double = TRUE
  }
  
  if(!player$jailstate){
    
    if((player$doublecount<2)||(player$doublecount==2 && !roll$doubles)){
      player$move_n(roll$movement)
      tracking$increase_count(player$pos)
    }

    if(roll$doubles && !afterjail_double){
      player$increase_doublecount()
      if(player$doublecount<=2) {
        if(player$pos==3||player$pos==18||player$pos==34){communitychest(player,tracking,verbose)}
        if(player$pos==8||player$pos==23||player$pos==37){chancechest(player,tracking,verbose)}
        if(!player$jailstate){taketurn(player,tracking,verbose=FALSE)}
        }
      if(player$doublecount==3) {
        player$go_to_jail()
        tracking$increase_count(player$pos)
      }
    }
    
    if(player$doublecount < 3){
      if(player$pos==3||player$pos==18||player$pos==34){communitychest(player,tracking,verbose)}
      if(player$pos==8||player$pos==23||player$pos==37){chancechest(player,tracking,verbose)}
    }
    player$clear_doublecount()
    player$clear_jailcount()
  }
}

# 20 verbose turns --------------------------------------------------------

set.seed(10)
setdice <- Dice$new(rolls = c(6, 4, 5, 3, 3, 5, 6, 2, 5, 4, 4, 1, 2, 6, 4, 4, 4, 4, 2, 2, 
                              4, 3, 4, 4, 1, 4, 3, 4, 1, 2, 3, 6, 5, 4, 5, 5, 1, 2, 5, 4, 
                              3, 3, 1, 1, 2, 1, 1, 3),
                    pos = 0, verbose = TRUE)
dice <- function() setdice$roll()
space_tracking <- tracking$new(tally = rep(0,40), verbose = TRUE)
player1 <- player$new(pos = 1, verbose = TRUE,doublecount=0,jailstate=FALSE,jailcount=0)  # new players for each game
for(i in 1:20){ # 100 turns for each game
  cat("\n## Turn", i,"\n")
  taketurn(player1, space_tracking,verbose=TRUE) 
}
space_tracking$tally

# 1000 simulations --------------------------------------------------------

set.seed(1)

space_tracking <- tracking$new(tally = rep(0,40),verbose=FALSE)
for(i in 1:1000){ # simulate 1000 games
  #cat("#### NEW GAME",i,"##### \n")
  player1 <- player$new(pos = 1, verbose = FALSE,doublecount=0,jailstate=FALSE,jailcount=0)  # new players for each game
  player2 <- player$new(pos = 1, verbose = FALSE,doublecount=0,jailstate=FALSE,jailcount=0)
  for(i in 1:150){ # 150 turns for each game
    if(player1$verbose) cat("Player 1 turn\n")
    taketurn(player1, space_tracking)  
    if(player2$verbose) cat("Player 2 turn\n")
    taketurn(player2, space_tracking)  
  }
}

results <- cbind(gameboard, tally = space_tracking$tally)
results <- cbind(results, rel = results$tally/sum(results$tally))
print(results)
library(dplyr)
arrange(results,desc(rel))
sum(results$tally)


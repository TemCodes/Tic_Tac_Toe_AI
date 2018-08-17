display <- function(x){
  cat(' ')
  cat(state[1],state[2],state[3], sep = " | ")
  cat("\n")
  cat("---+---+---",'\n')
  cat(' ')
  cat(state[4],state[5],state[6], sep = " | ")
  cat("\n")
  cat("---+---+---",'\n')
  cat(' ')
  cat(state[7],state[8],state[9], sep = " | ")
  cat('\n')
  cat('\n')
}

prompt_user<-function(who,state){
  illegal=TRUE
  while(illegal){
    if(who ==1 )
      {
      pos<-as.double(readline('Where should x play (1 to 9): '))
    }
    else if(who ==2)
    {
      pos<-as.double(readline('Where should o play (1 to 9): '))
    }
    if(pos != 1 && pos != 2 && pos != 3 && pos != 4 && pos != 5 && pos != 6 && pos != 7 && pos != 8 && pos != 9 ){
      cat('Please pick a number from 1 to 9','\n')
    }else if (state[pos]  == 'x' || state[pos] == 'o'){
          cat('This spot has already been played','\n')
    }
    else{
      illegal=FALSE
    }
  }
  update(state,who,pos)
}

update <- function(state, who, pos){
  if(who == 1){
    state[pos] <<-'x' 
  }else{
    state[pos] <<- 'o'
  }
}

check_winner <- function(state){
  for(i in (1:8)){
    if(all(triples[[i]] %in% which(state=='x'))){
      cat('x wins','\n','\n')
      winner<<-TRUE
    }
  }
  for(i in (1:8)){
    if(all(triples[[i]] %in% which(state=='o'))){
      cat('o wins','\n','\n')
      winner<<-TRUE
    }
  }
  if( all(state[1:9] != (1:9)) ){
    cat('game has gone to a draw','\n')
    winner<<-TRUE
  }
}

computer_turn<-function(state)
  {
#   #checks for winning move
#   #check for 2/3 spots in winning move for x or o and spot is empty

  

  # for(i in (1:8)){
  #   if(sum( triples[[i]]%in% which(state=='x'))==2 ){
  #     pos<-setdiff(triples[[i]],which(state=='x'))      pos<-sample(1:9,1)

  #     update(state,2,pos)
  #   }
  #   else
  #   {
  # 
  #   }
  # }
  
  

#   for(i in 1:8)
#   {
#     checktrip = triples[[i]]
#     if(sum(state[checktrip[1]] == "computer", 
#                  state[checktrip[2]], 
#                  state[checktrip[3]])==2 & sum(is.na(state[checktrip[[1]]] ) ) )
#     
#      }

 
#   
#   #check for blocking move
# }else{
#   # if two in a row, check if next one is empty
# }

#else do a random spot
rand<-sample(1:9,1)
while(state[rand] == 'x' || state[rand] == 'o'){
  rand<-sample(1:9,1)}
pos<-rand
update(state,2,pos)
   
}


play <- function(){
  state <<-  as.character(1:9)
  winner <<- FALSE
  triples <<- list(
    c(1,2,3),
    c(4,5,6),
    c(7,8,9),
    c(1,4,7),
    c(2,5,8),
    c(3,6,9),
    c(1,5,9),
    c(3,5,7)  )
  
  num_players<-as.character(readline('How many players (1 if against computer): '))
  if(num_players == '2')
  {
    while(winner == FALSE)
    {
      display(state)
      prompt_user(1,state)
      check_winner(state)
      if(winner == TRUE){
        display(state)
        break
        }
      display(state)
      prompt_user(2,state)
      check_winner(state)
      if(winner == TRUE){
        display(state)
        break
      }
    }
  }
  
  if(num_players== '1')
  {
    choice<-as.character(readline('Do you want to go first (type 1) or second (type 2): '))
    if(choice == '1')
    {
      while(winner == FALSE)
      {
        display(state)
        prompt_user(1,state)
        check_winner(state)
        if(winner == TRUE){
          display(state)
          break
        }
        display(state)
        computer_turn(state)
        check_winner(state)
        if(winner == TRUE){
          display(state)
          break
        }
      }
    }
    if(choice == '2')
    {
      while(winner == FALSE)
      {
        display(state)
        computer_turn(state)
        check_winner(state)
        if(winner == TRUE){
          display(state)
          break
        }
        display(state)
        prompt_user(2,state)
        check_winner(state)
        if(winner == TRUE){
          display(state)
          break
        }
      }
    }
  }
}
play()
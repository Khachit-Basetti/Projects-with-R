#Coup game

draw=function(){
  deck=c('duke','Assassin','Captain','Ambassadar','Contessa')
  hand=sample(deck,size=3,replace = T)
  print(hand)
}
draw()

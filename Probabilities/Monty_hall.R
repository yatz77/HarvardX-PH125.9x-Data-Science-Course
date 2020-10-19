B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3) # puts prizes in a random order
  prize <- sample(c("car", "goat", "goat")) # note which door has a prize
  prize_door <- doors[prize == "car"] # note which door is chosen
  my_pick <- sample(doors, 1) # note which door has prize
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1) # open door with no prize that isn`t chosen
  stick <- my_pick # stick with origingal door
  stick == prize_door # test whether the original door has the prize
})
mean(stick) # probability of chossing prize door when sticking

switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat")) # puts prizes in random order
  prize_door <- doors[prize == "car"] # note which door has prize
  my_pick <- sample(doors, 1) # note which door is chosen first
  show <- sample(doors[!doors%in%c(my_pick, prize_door)], 1) # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)] # switch to the door that wasn't chosen first or opened
  switch == prize_door # test whether the switched door has the prize
})
mean(switch) # probability of choosing prize door when switching

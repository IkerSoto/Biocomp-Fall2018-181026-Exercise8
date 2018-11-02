###Exercise 8 ###
### Iker Soto###
setwd("../Biocomp-Fall2018-181026-Exercise8/")
Game<-read.table("UWvMSU_1-22-13.txt",header = TRUE,stringsAsFactors = FALSE) 
#Checking structure of the dataframe
Game 
#Creating empty dataframes where the cumulative scores of each team will be stored
UW<-0
MSU<-0
#Adding an additional column in the Game table called cumulative 
Game$Cumulative<-NA
#Starting loop to analyze and add scores for each team row by row.
for(i in 1:(nrow(Game))){
  if(Game[i,2]=="UW"){ #Row by row check if the team that scored is UW...
    UW<- UW + Game[i,3] #... If so, add the previous score (stored in the UW dataframe) to the score registered for the row
    Game[i,4]<-UW #Put the calculated cumulative score in the Cumulative column 
  }else if(Game[i,2]=="MSU"){ #Row by row check if the team that scored is MSU...
  MSU<-MSU + Game[i,3]#... If so, add the previous score (stored in the MSU dataframe) to the score registered for the row
  Game[i,4]<-MSU #Put the calculated cumulative score in the Cumulative column
  }
}
#Making a plot for the cumulative scores of both teams with ggplot
library(ggplot2)
library(grid)
library(gridExtra)
g<-ggplot(points<-Game,aes(x=time,y=Cumulative,group=team))
g+geom_line(aes(color=team))+geom_point(aes(color=team))+theme_classic()


###Guessing Game
GuessingGame<-function(){
  choice<-sample(1:100,size = 1) #Argument to make computer chose a random number between 1:100
  number=readline(prompt="What's your number?") #Setting a prompt so that the user can provide input in the terminal
  #The code will enter a while loop if the number provided by the user is different than
  #the number selected at random by the computer
  while(number!=choice){ 
    #An if loop inside the while loop that establishes what the computer should print if the number guessed by the user is 
    # greater than the number selected at random. 
    if(number < choice){ 
      print("Higher")    
      number=readline(prompt="Guess again")
    } 
    #What the computer should print if the number guessed by the user is lower than the number selected at random
    else if (number > choice){
      print("Lower")
      number=readline(prompt="Guess again")
    }
  }
    print("That's correct!") #What the computer should print
                             #If the number guessed is equal to the number selected by the computer
  }
GuessingGame()







#Part 1
#The population will be declining (redlisted) due to hunting from wolfes, foxes, humans (and natural death)
#The high number of offsprings will help the population, but won't be enought.
#The declining population will initially be a fast decline that then stabilizes and the 
#population will reach its natural carrying capacity or been completely killed of by humans
#"Humans tries to remove the remaining resident. 

#Part 2, First model. Just reproduction no hunting/threats 
#Sexually mature by the age of 1. 5-16 offsprings. 

t_max<-10
#I first create a leslie matrix A
A<-matrix(0,nrow=8, ncol=8)
s<-c(0.2,0.6,0.6,0.6,0.5,0.3,0.2)
for(i in 2: (nrow(A)))
{ A[i, i - 1] <- s[i-1]} #Assign the survival rate to the sub diagonal. 


#Starting population = P0 = 100
#We do not know the age distribution of the starting population,
N <- matrix(0, t_max, nrow(A))
#So I assume the following starting population. 
N[1,]<- c(20,20,15,15,10,10,10,5)


set.seed(11) #Should be random, but needs to be repeatable. 
for (t in 1:(t_max-1))
{ A[1,]<- c(0, sample(5:16,7)) #Adds the random number 
  #of babies to the first row in the Leslie matrix
  N[t+1,]<- A%*% N[t,] #According to lecture slides 
}
N<-round(N,digits=0) #Easier to read the matrix, populations are discrete
matplot(x=1:t_max, y=N, ylab="# of Individuals", type="l", xlab="time", main="No threats")
print("Population development with no threat")
print("No real limitation (more than s) -> population will increase to 00")
print(N)

#Part 3, Adding wolves and foxes
#Assumption: Wolves kills evenly over all age gropes (with 20% per year)
#Foxes will just affect newborns, and kill 50% of them each year. 
#Less wolves in Sweden and more foxes. Reasonable assumption

#Just wolves
N <- matrix(0, t_max, nrow(A)) #Reset matrix to 0
N[1,]<- c(20,20,15,15,10,10,10,5)

set.seed(11)
for (t in 1:(t_max-1)) 
{ A[1,]<- c(0, sample(5:16,7))  
N[t+1,]<- A%*% N[t,] 
N[t+1,]<- N[t+1,]*0.8 #Same as before, but wolves kills a fraction. 
}
N<-round(N,digits=0) 
matplot(x=1:t_max, y=N, ylab="# of Individuals", type="l", xlab="time", main="Impact of Wolfes")


#Just foxes 
N <- matrix(0, t_max, nrow(A))  #Reset matrix to 0
N[1,]<- c(20,20,15,15,10,10,10,5)

set.seed(11)
for (t in 1:(t_max-1)) 
{ A[1,]<- c(0, sample(5:16,7))  
N[t+1,]<- A%*% N[t,] 
N[t+1,1]<- N[t+1,1]*0.5 #Same as before, but foxes kills 50% of newborns 
}
N<-round(N,digits=0) 
matplot(x=1:t_max, y=N, ylab="# of Individuals", type="l", xlab="time", main="Impact of Foxes")

#Part 4 Human hunting + Fox and Wolves
#Eliminate population
#Now assume we hunt a fixed number of adults per year. 
t_max<-10
N <- matrix(0, t_max, nrow(A))  #Reset matrix to 0
N[1,]<- c(20,20,15,15,10,10,10,5)

set.seed(11)
for (t in 1:(t_max-1)) 
{ A[1,]<- c(0, sample(5:16,7))  

N[t+1,]<- A%*% N[t,] 
N[t+1,]<- N[t+1,]*0.8 #Wolves
N[t+1,1]<- round(N[t+1,1]*0.5) #Fox

N[t+1,]<-round(N[t+1,],digits=0) 

#Humans, say we kill a specific number of adults. 
for (k in 2:8) {if(N[t+1,k]>8){N[t+1,k]<- N[t+1,k]-8   }
 else {N[t+1,k]<- N[t+1,k]-N[t+1,k]} 
  #kill 8 in each age group (not infants) if possible. Else kill as many as left. 
}}
print("Population development with hunting, eliminate within 10 years")
print(N)
matplot(x=1:t_max, y=N, ylab="# of Individuals", type="l", xlab="time", main="Eliminate population within 10 years")


#Eliminate population within 20 years
t_max<-20
N <- matrix(0, t_max, nrow(A))  #Reset matrix to 0
N[1,]<- c(20,20,15,15,10,10,10,5)

set.seed(11)
for (t in 1:(t_max-1)) 
{ A[1,]<- c(0, sample(5:16,7))  

N[t+1,]<- A%*% N[t,] 
N[t+1,]<- N[t+1,]*0.8 #Wolves
N[t+1,1]<- round(N[t+1,1]*0.5) #Fox

N[t+1,]<-round(N[t+1,],digits=0) 

#Humans, say we kill a specific number of adults. 
for (k in 2:8) {if(N[t+1,k]>3){N[t+1,k]<- N[t+1,k]-4   }
  else {N[t+1,k]<- N[t+1,k]-N[t+1,k]} 
  #kill 4 in each age group (not infants) if possible. Else kill as many as left. 
  
}}
print("Population development with hunting, eliminate within 20 years")
print(N)
matplot(x=1:t_max, y=N, ylab="# of Individuals", type="l", xlab="time", main="Eliminate population within 20 year")



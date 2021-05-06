
#Given growth rates
r_best<-0.01676
r_medium<-0.00549
r_worst<-(-0.04550)

#Initial population
P0<-100


#Tracking time
T_max <-10
#Part2, T_max <-25

#Arrays describing the population development 
P_best <-c(rep(0,T_max))
P_medium <-c(rep(0,T_max))
P_worst <-c(rep(0,T_max))

P_best <-P0
P_medium <-P0
P_worst <-P0

#Population development pop(t+1)=r*pop(t)
for(t in 1:(T_max-1)){P_best[t+1]<-P_best[t]*(1+r_best)}
for(t in 1:(T_max-1)){P_medium[t+1]<-P_medium[t]*(1+r_medium)}
for(t in 1:(T_max-1)){P_worst[t+1]<-P_worst[t]*(1+r_worst)}

#Plotting: 
plot(x=1:T_max, y=P_best, type="l", col="blue", xlab="Years", ylab="Bobcat population", ylim=c(P_worst[T_max],P_best[T_max]))
points(x=1:T_max, y=P_worst, type="l", col="green")
points(x=1:T_max, y=P_medium, type="l", col="red")
legend(x=(T_max/2), y=(100), legend=c("Best", "Medium", "Worst"), 
       col=c("Blue", "red", "green"), lty="solid")

#Part 3, Hunting strategies in growing (Best) population. 
#Hunt (1 per year, 5 per year, 1% per year, , 5%)
T_max<-25
Pop<-matrix(0,nrow=4,ncol=T_max) #Make a matrix over the population changes. 
Pop[,1]<-P0


for(i in 1:4) { #Looping through all options
P_best<-P0

for(t in 1:(T_max-1)){
#Array of hunting strategies
Hunting<-c(1,5,0.01*P_best[t], 0.05*P_best[t])  
#Change of population
P_best[t+1]<-(1+r_best)*P_best[t]-Hunting[i] 

Pop[i,(t+1)]<-P_best[t+1] }}

#Plotting
plot(x=1:T_max, y=Pop[1,], type="l", col="blue", xlab="Years", ylab="Bobcat population", ylim=c(0,120))
points(x=1:T_max, y=Pop[2,], type="l", col="black")

points(x=1:T_max, y=Pop[3,], type="l", col="green")
points(x=1:T_max, y=Pop[4,], type="l", col="red")
legend(x=15, y=105, legend=c("One per year", "Five per year", "One percent", "Five percent"), 
       col=c("blue", "black", "green", "red"),lty=c("solid"))

#Part 4, Individual strategy, stabilizes at 200.
P_best->P0 #Reset population
T_max<-70
#Start hunting 1.676% after 40 years. 
for(t in 1:(T_max-1)){
  if (t>40){P_best[t+1]<-(1+r_best)*P_best[t]-(0.01676*P_best[t])}
  else {P_best[t+1]<-(1+r_best)*P_best[t]}}

#Plotting
plot(x=1:T_max, y=P_best, type="l", col="blue", xlab="Years", ylab="Bobcat population", ylim=c(100,220))
legend(x=5, y=200, legend=c("New strategy"), col=c("blue"), lty="solid")

#Part 5 Worst case, add animals. 
T_max<-25
Pop<-matrix(0,nrow=4,ncol=T_max) #Make a matrix over the population changes. 
Pop[]<-P0


for(i in 1:4) { #Looping through all options

  P_worst<-P0
  for(t in 1:(T_max-1)){
    #Array of adding strategies
    Adding<-c(3,10,0.01*P_worst[t], 0.05*P_worst[t])  
    #Change of population
    P_worst[t+1]<-(1+r_worst)*P_worst[t]+Adding[i] 
    Pop[i,(t+1)]<-P_worst[t+1] }}

#Plotting
plot(x=1:T_max, y=Pop[1,], type="l", col="blue", xlab="Years", ylab="Bobcat population",
     ylim=c(40,200))
points(x=1:T_max, y=Pop[2,], type="l", col="black")
points(x=1:T_max, y=Pop[3,], type="l", col="green")
points(x=1:T_max, y=Pop[4,], type="l", col="red")
legend(x=2, y=200, legend=c("Three per year", "Ten per year", "One percent", "Five percent"), 
       col=c("blue", "black", "green", "red"),lty=c("solid"))

#Part 6, Stabilizing at 50 and 200. 
T_max<-110
P_worst2<-c(rep(0,T_max))
P_worst2[1]<-100
for(t in 1:(T_max-1)){
P_worst[t+1]<-P_worst[t]*((1+r_worst)-r_worst*(50/P_worst[t])) #

P_worst2[t+1]<-P_worst2[t]*((1+r_worst)-r_worst*(200/P_worst2[t])) #
}
plot(x=1:T_max, y=P_worst, type="l", col="blue", xlab="Years", ylab="Bobcat population", ylim=c(45,220))
points(x=1:T_max, y=P_worst2, type="l", col="red")
legend(x=50, y=150, legend=c("Population 1", "Population 2"), 
       col=c("red", "blue"),lty=c("solid"))


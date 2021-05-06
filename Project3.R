##Project 3

##Part 1 - Leslie matrix

A <- matrix(0, nrow = 16, ncol = 16)

for(i in 2: (nrow(A)))
{
  
  A[i, i - 1] <- 0.71
  
}

A[2,1] <- 0.34

A[1,] <- 1.2
A[1,1] <- 0.63
A[1,2] <- 0.63

A

##part 2 - eigenvalue

eigen(A)$values[] ##Fist one is the dominant eigen value, represents growth rate after long time

eigen(A)$vectors[,1] ##Dominant eigen vector

X <- abs(eigen(A)$vectors[,1]) 
X / sum(X) ##Distribution of age classes

##part 3 - population
tmax <- 10

p0 <- rep(0, 16)
p0[2] <- 100
p0[3] <- 50
p0[4] <- 50
p0[5] <- 25
p0[6] <- 10

N <- matrix(0, tmax, nrow(A))
N[1,] <- p0

for(i in 1:(tmax - 1))
{
  N[i + 1,] <- A %*% N[i,]
}

matplot(1:tmax, N, xlab = "time", type = "b", lty = "solid")
matplot(1:tmax, N, xlab = "time", type = "b", lty = "solid", ylim = c(0, 250))
matplot(1:tmax, log(N), xlab = "time", type = "b", lty = "solid")

##Part of population after 10 years
barplot(N[10,] / sum(N[10,]), xlab = "Age class", ylab = "Part of total population")
##Part of population after long time
barplot(X / sum(X), xlab = "Age class", ylab = "Part of total population")

##-----------------------------------------------------------------------------

##PART 4 WORST CASE DATA

##Part 1 - Leslie matrix

B <- matrix(0, nrow = 16, ncol = 16)

for(i in 2: (nrow(B)))
{
  
  B[i, i - 1] <- 0.68
  
}

B[2,1] <- 0.32

B[1,] <- 1.15
B[1,1] <- 0.6
B[1,2] <- 0.6

B

##part 2 - eigenvalue

eigen(B)$values[] ##Fist one is the dominant eigen value, represents growth rate after long time

eigen(B)$vectors[,1] ##Dominant eigen vector

X <- abs(eigen(B)$vectors[,1]) 
X / sum(X) ##Distribution of age classes

##part 3 - population
tmax <- 10

p0 <- rep(0, 16)
p0[2] <- 100
p0[3] <- 50
p0[4] <- 50
p0[5] <- 25
p0[6] <- 10

N <- matrix(0, tmax, nrow(B))
N[1,] <- p0

for(i in 1:(tmax - 1))
{
  N[i + 1,] <- B %*% N[i,]
}

matplot(1:tmax, N, xlab = "time", type = "b", lty = "solid")
matplot(1:tmax, N, xlab = "time", type = "b", lty = "solid", ylim = c(0, 250))
matplot(1:tmax, log(N), xlab = "time", type = "b", lty = "solid")

##Part of population after 10 years
barplot(N[10,] / sum(N[10,]), xlab = "Age class", ylab = "Part of total population")
##Part of population after long time
barplot(X / sum(X), xlab = "Age class", ylab = "Part of total population")


##part 5 - catastrophies

A <- matrix(0, nrow = 16, ncol = 16)

for(i in 2: (nrow(A)))
{
  
  A[i, i - 1] <- 0.71
  
}

A[2,1] <- 0.34

A[1,] <- 1.2
A[1,1] <- 0.63
A[1,2] <- 0.63

Catastrophy <- A
Catastrophy[1,] <- A[1,] * 0.7

tmax <- 10
nreps <- 100

p0 <- rep(0, 16)
p0[2] <- 100
p0[3] <- 50
p0[4] <- 50
p0[5] <- 25
p0[6] <- 10

N <- array(0, dim = c(tmax, nrow(A), nreps))
N[1,,] <- p0

for(j in 1:nreps)
{
for(i in 1:(tmax - 1))
{
  if(sample(1:25, 1, replace = FALSE) == 1){
  N[i + 1,,j] <- Catastrophy %*% N[i,,j]
  }
  else{
    N[i + 1,,j] <- A %*% N[i,,j]
  }
}
}

##Sl? samman alla ?ldersklasser till en total population, per ?r

P <- matrix(0, tmax, nreps)

for(i in 1:nreps)
{
  for(j in 1:tmax)
  {
    P[j,i] <- sum(N[j, ,i])
  }
}

P

boxplot(P, 1:tmax, xlab = "Iterations", ylab ="total # of Individuals")
boxplot(t(P), 1:tmax, xlab = "Time", ylab ="total # of Individuals")

matplot(x = 1: tmax, y = N[,,1], type="l", xlab="Time", ylab="# of Individuals", 
      ylim=c(0,1500))

matplot(x = 1: tmax, y = N[,,2], type="l", xlab="Time", ylab="# of Individuals", 
        ylim=c(0,1500))

matplot(x = 1: tmax, y = N[,,3], type="l", xlab="Time", ylab="# of Individuals", 
        ylim=c(0,1500))

matplot(x = 1: tmax, y = N[,,7], type="l", xlab="Time", ylab="# of Individuals", 
        ylim=c(0,1500))




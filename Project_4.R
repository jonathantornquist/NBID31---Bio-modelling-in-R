install.packages("diagram")
library(diagram)

Transition <- matrix(c(2, 7, 9, 2, 0, 0, 1,
                       0, 3, 19, 17, 1, 0, 2,
                       0, 2, 20, 11, 10, 4, 23,
                       0, 0, 10, 26, 26, 6, 24,
                       0, 0, 0, 5, 22, 2, 30,
                       0, 0, 0, 0, 0, 1, 0, 
                       0, 0, 0, 0, 0, 0, 1), 7, 7, TRUE)
Division <- c(21, 42, 70, 92, 59, 1, 1)
for (i in 1:7) {
Transition[i,] <- Transition[i,]/Division[i]
}

A <- t(Transition)
Names <- c(1, 2, 3, 4, 5, 6, 7)

##plotmat(A, pos = c(1, 2, 2, 2), name=Names, relsize = 0.75, lwd = 1, box.lwd = 2, cex.text = 0.8, self.cex = 0.5, 
       ## self.shiftx = c(0.1, -0.1, 0.1, -0.1), box.type = "circle", box.prop = 0.5, box.size = 0.1)
plotmat(round(A, 3), pos = 7, curve = 0.7, name = Names, relsize = 0.75, lwd = 1, box.lwd = 2, cex.txt = 0.8,
        self.cex = 0.5, box.type = "circle", box.prop = 1, box.size = 0.05, self.lwd = 2)

## PART TWO

eigen(A)$values
eigen(A)$vectors

B <- A ## Creating a replica of A to work with
for (i in 1:10000) {
  B <- B%*%B
}
B ## Probabilities of ending up in the different states. Absorption probability.
PlotMatrix<- B[6:7, 1:5]
boxplot(PlotMatrix)


Q <- A[1:5, 1:5]

I <- diag(x=1, 5, 5)


F <- solve(I - Q)

F

Sums <- colSums(F)
Sums
xaxel <- c(1,2,3,4,5)
names(Sums) <- xaxel

barplot(Sums)






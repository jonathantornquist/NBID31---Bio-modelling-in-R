z<- readline(prompt = "Enter a number: ")
z<-as.numeric(z)

if (z>100) {print(sprintf(" z^3 = %s",z**3))}
if (z%%17==0) {print(sprintf(" sqrt(z) = %s",round(sqrt(z),digits=2)))}
if (z<10) {print(c(1:z))}

#------------------------------------------------------
#Intermezzo 8.1
#All even numbers 2-100
Z<-c(seq(from=2, to=100, by=2))

#Division with 12
element<-0 #Counter for number of times Z mod 12=0
Divisible_numbers<-rep(0,10)
for(i in 1:length(Z)) {
if ((Z[i]%%12)==0) {element<-element+1  #%% = mod
Divisible_numbers[element]<-Z[i]}}
print("Number of elements that could be divided by 12 =")
print(element)

#Sum of Z
print("Sum of Z")
print(sum(Z))

if (sum(Z)==(51*50))
{print("Yes, the sum of Z = 50*51")} else 
{print("No, sum(Z) are not 50*51")}

print("The product of Z[5]*Z[10]*Z[15] is:")
prod<-(Z[5]*Z[10]*Z[15])
print(prod)

#Create a vector y that contains all numbers 
#between 0 and 30 that are divisible by 3. 
#Find the five elements of y that are also 
#elements of z.
y<-rep(0,11)
element_of_y_z<-rep(0,5)
a<-1
l<-1
for (j in 0:30) {
if ((j%%3)==0) {y[a]<-j
#Looking through all Z, 
#if match with Y -> assign element_of_y&z 
for (k in 1:length(Z)) {if (y[a]==Z[k]) 
{element_of_y_z[l]<-y[a]
l<-l+1}}

a<-a+1 }}
print("Matching elements in y and z =")
print(element_of_y_z)

#Have defined Z<-c(seq(from=2, to=100, by=2))
Z2<-c(1:50)*2
counter<-0
for (u in 1:length(Z)) {
if (Z2[u]==Z[u]) {} #if they are the same do nothing, 
else {counter<-1}} #Otherwise counter <-1

if (counter==0) {print("They are exactly the same vector")
} else {print("They are not the same vector")}

#print(Z^2), Every Integer in the vector Z gets squared, 
# 2->4, 4->16, ... , 100->10 000

#-----------------------------------------------
print("Intermezzo 8.2")
data(trees) #Gets Girth, Hight and Volume of cherry trees, matrix
print(head(trees)) #Provides a overview of our matrix
print(sprintf("Average tree hegiht = %s", mean(trees$Height)))


girth_new<-rep(0,length(trees$Girth))
count<-1
#if the height is over 75 -> add girth to girth new.
for (i in 1:length(trees$Height)) {
if (trees[i,2]>75) {girth_new[count]<-trees[i,1]
count<-count+1}}
print(sprintf("Average girth for trees over 75ft tall = %s", 
              round(mean(girth_new[1:count-1]), digits=2)))


height_new<-rep(0,length(trees$Volume))
count2<-1
for (j in 1:length(trees$Volume)) {
if (15<trees[j,3]&35>trees[j,3] ) {height_new[count2]<-trees[j,2]
count2<-count2+1}}
print(sprintf("Max height if 15<Volume<35 is = %s", max(height_new)))
#----------------------------------------------------
print("Intermezzo 8.3")
Tot_ind<-0
#Setting the working dictionary to the map with the downloaded file. 
setwd("~/Desktop/NBID31")
ch6 <- read.table("H938_Euro_chr6.geno", header =TRUE)
#print(ch6)

#One individual can only have one allel combination. 
#Sum over all nA1A1 nA1A2 nA2A2 
#Talked with Anna about this. 
for (j in 1:length(ch6[,1])) {
for (i in 5:7) {Tot_ind<-ch6[j,i]+Tot_ind 
} } #nA1A1 starts at the 5th column
print(sprintf("The total individuals = %s", Tot_ind))  #Tot ind. =5 345 125

#find the row with the max(A1A1+A1A1+A2A2)
Max_sum<-0
for (j in 1:length(ch6[,1])) {
  for (i in 5:7) { if (Max_sum<ch6[1,i]) {Max_sum<-ch6[1,i]+Max_sum } }} 
print(sprintf("The max rowsum is = %s",Max_sum)) #Max_sum=124.

#Obtain the same result with rowSums
Tot_ind2<-0
for (k in 1:length(ch6[,1])) {
Tot_ind2<-rowSums(ch6[k,5:7])+Tot_ind2 }#Sums the row for the k row from column 5-7
print(sprintf("The total individuals by using Rowsum = %s", Tot_ind2)) 
#Gets the same number 5345125 

Max_Sum2<-0
Max_Sum2<-(max(rowSums(ch6[,5:7])))
print(sprintf("The max rowsum by using RowSum is = %s",Max_Sum2)) #Max_sum=124.


#How many are homozygotes =A1A1 + A2A2 <--> Tot_ind - Hetrozygotes
Hetrozygotes<-0
for (u in 1:length(ch6[,1])) {
Hetrozygotes<- ch6[u,6]+Hetrozygotes  }
print(sprintf("Number of homozygotes = %s",Tot_ind-Hetrozygotes)) #Homozygotes=3 750 114

#For how many of the 43,141 SPS are more than 99% homozygous
SNP<-0
for (w in 1:length(ch6[,1])) {
#(Homo/(Homo+Hetro) for all rows. If >0.99 increase counter)
if ((ch6[w,5]+ch6[w,7])/(ch6[w,5]+ch6[w,6]+ch6[w,7])>0.99) {SNP<-SNP+1}}
print(sprintf("SNP with homozygote 99+ percent = %s",SNP)) #SNP=3368

#----------------------------------------------------
print("Intermezzo 8.4")
#Run another script (Conditional.R) from this script
source('~/Desktop/NBID31/conditional.R')


 ##normally distributed growth rate ##L?gg in titel p? grafen
alpha <- c(1.5 / 12000, 1 / 12000, 0.8 / 12000) ##bad, typical, and good conditions
t_max <- 100

p0 <- 100

population <- rep(p0, t_max)

##1.1

for(i in 1 : (t_max - 1)) ##Weather conditions set to typical every year, growht rate normally distributed
{
  growth_rate <- rnorm(1, mean = 0.1, sd = 0.2)
  
  weather_condition <- alpha[2]
  
  population[i + 1] <- population[i] * (1 + growth_rate - weather_condition * population[i])
}

plot(x = 1: t_max, y = population, type="l", xlab="Time", ylab="# of Individuals", 
     col="blue", ylim=c(0,2000)) ##Positive growth over 100 years, random


for(i in 1 : (t_max - 1)) ##Weather conditions uniformly random each year, growht rate 0,1 (mean)
{
  growth_rate <- 0.1
  
  weather_condition <- alpha[sample (1:3, 1, replace = FALSE)]
  
  population[i + 1] <- population[i] * (1 + growth_rate - weather_condition * population[i])
}

plot(x = 1: t_max, y = population, type="l", xlab="Time", ylab="# of Individuals", 
     col="green", ylim=c(0,2000)) ##Positive growth until stabilization, some randomness



for(i in 1 : (t_max - 1)) ##Weather conditions uniformly random each year, growht rate normally distributed
{
  growth_rate <- rnorm(1, mean = 0.1, sd = 0.2)
  
  weather_condition <- alpha[sample (1:3, 1, replace = FALSE)]
  
  population[i + 1] <- population[i] * (1 + growth_rate - weather_condition * population[i])
}

plot(x = 1: t_max, y = population, type="l", xlab="Time", ylab="# of Individuals", 
     col="red", ylim=c(0,2000)) ##Positive growth over 100 years, partially exponential

##2.2

nreps <- 20

population_runs <- matrix(p0, nrow = t_max, ncol = nreps)

weather_condition <- 0

count_less_than_50 <- 0

for(j in 1: nreps)
{
for(i in 1 : (t_max - 1)) ##Weather conditions more likely to be bad, growth rate normally distributed
{
  growth_rate <- rnorm(1, mean = 0.1, sd = 0.2)
  
  if(weather_condition == alpha[1])
  {
    weather_condition <- alpha[sample (1:3, 1, replace = FALSE, prob = c(10, 1, 1))]
  } else
  {
    weather_condition <- alpha[sample (1:3, 1, replace = FALSE)]
  }
  
  population_runs[i + 1, j] <- population_runs[i, j] * (1 + growth_rate - weather_condition * population_runs[i, j])
  
  if(population_runs[i + 1, j] < 50){count_less_than_50 <- count_less_than_50 + 1}
}
}

count_less_than_50 / nreps

boxplot(t(population_runs), xlab = "Time", ylab ="# of Individuals")
##matplot(x = 1 : t_max, y = population_runs, type = "l", lty = "solid", xlab = "Time", ylab = "# of Individuals", 
##ylim = c(0, 50))
##The higher the probability to have bad consecutive weather the quicker the population will stabilize at 800



nreps <- 20

population_runs <- matrix(p0, nrow = t_max, ncol = nreps)

weather_condition <- 0

count_less_than_50 <- 0

for(j in 1: nreps)
{
  for(i in 1 : (t_max - 1)) ##Weather conditions more likely to be bad, growth rate has to be less than mean when previous year bad
  {
    growth_rate <- 1
     
    if(weather_condition == alpha[1])
    {
      weather_condition <- alpha[sample (1:3, 1, replace = FALSE, prob = c(10, 1, 1))]
      
      while(growth_rate > 0.1)
      {
        growth_rate <- rnorm(1, mean = 0.1, sd = 0.2)
      }
    } else
    {
      weather_condition <- alpha[sample (1:3, 1, replace = FALSE)]
      growth_rate <- rnorm(1, mean = 0.1, sd = 0.2)
    }
    
    population_runs[i + 1, j] <- population_runs[i, j] * (1 + growth_rate - weather_condition * population_runs[i, j])
    
    if(population_runs[i + 1, j] < 50){count_less_than_50 <- count_less_than_50 + 1}
  }
}

count_less_than_50 / nreps

boxplot(t(population_runs), xlab = "Time", ylab ="# of Individuals")
##poor weather kills population





# Intro to Random Vars
# ZMP
paste("#" ,format(Sys.Date(),format="%d %b %Y"))

##intro
# read in .csv
library(readr)
femaleMiceWeights <- read_csv("raw_data/femaleMiceWeights.csv")
View(femaleMiceWeights)

## average of each group
library(dplyr)
control <- filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist
#filter rows named "chow", and then select values bodyweight, 
#and then unlist for numeric values in a vector
treatment <- filter(dat,Diet=="hf") %>% select(Bodyweight) %>% unlist
print( mean(treatment) )
print(mean(control))

# difference in averages of each group
obsdiff <- mean(treatment) - mean(control)
print(obsdiff)
# So the hf diet mice are about 10% heavier

##random variables
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- "femaleControlsPopulation.csv"
if (!file.exists(filename)) download(url,destfile=filename)
population <- read.csv(filename)
population <- unlist(population) # turn it into a numeric

#Now let’s sample 12 mice three times and see how the average changes
control <- sample(population,12)
mean(control)
#the mean will vary. if we do this continuously we can learn about the dist. of the random variable

## null hypothesis
# the hypothesis that there is no statistically significant difference between specified populations, 
# with any observed difference being due to sampling or experimental error
#randomly sampling 24 control mice, giving them the same diet, 
#and then recording the difference in mean between two randomly split groups of 12 and 12
##12 control mice
control <- sample(population,12)
##another 12 control mice that we act as if they were not
treatment <- sample(population,12)
print(mean(treatment) - mean(control))
# lets do that 10,000 times
n <- 10000
null <- vector("numeric",n) # vector of 10000 zeroes
for (i in 1:n) { # for index in 1:n
  control <- sample(population,12) #sample 12 mice with control diet
  treatment <- sample(population,12)# same 12 mice w same diet, but call them 'treatment'
  null[i] <- mean(treatment) - mean(control) #index for each value is diff in means
}
#So what percent of the 10,000 are bigger than obsdiff?
mean(null >= obsdiff)
#When there is no diet effect, 
#we see a difference as big as the one we observed only 1.5% of the time
#e.g. p=0.0122

## Distributions
#compact way to describe large sets of data
library(UsingR)
x <- father.son$fheight #heights of men in a dataset
round(sample(x,10),1) #ten randomly selected heights

# Cumulative Distribution Function
# visualizing a distribution
#F(a)≡Pr(x≤a) 
# proportion of numbers in our list that are below a, for all possible values of a
# empirical cumulative distribution function
smallest <- floor( min(x) )
largest <- ceiling( max(x) )
values <- seq(smallest, largest,len=300)
heightecdf <- ecdf(x)
plot(values, heightecdf(values), type="l",
     xlab="a (Height in inches)",ylab="Pr(x <= a)")

# Histograms
# Pr(a≤x≤b)=F(b)−F(a)
hist(x)
bins <- seq(smallest, largest)
hist(x,breaks=bins,xlab="Height (in inches)",main="Adult men heights")

# Probability Distribution
#Unlike a fixed list of numbers, 
#we don’t actually observe all possible outcomes of random variables, 
#so instead of describing proportions, we describe probabilities. 
#if we pick a random height from our list, 
#then the probability of it falling between a and b is denoted with:
#Pr(a≤X≤b)=F(b)−F(a)
# capital X to describe a random var; probability dist. of a random var

#if we know the null distribution, we can compute p value
# we previously obtained 10000 outcomes of the random var under the null hypothesis 
#(treatment groups were the same)
n <- 100
library(rafalib)
nullplot(-5,5,1,30, xlab="Observed differences (grams)", ylab="Frequency")
totals <- vector("numeric",11)
for (i in 1:n) {
  control <- sample(population,12)
  treatment <- sample(population,12)
  nulldiff <- mean(treatment) - mean(control)
  j <- pmax(pmin(round(nulldiff)+6,11),1) #add a point to the figure every time we rerun the experiment
  totals[j] <- totals[j]+1
  text(j-6,totals[j],pch=15,round(nulldiff,1))
  ##if(i < 15) Sys.sleep(1) ##You can add this line to see values appear slowly
}

#values as big as obsdiff are rare (see red line)
hist(null, freq=TRUE)
abline(v=obsdiff, col="red", lwd=2)
  
# Normal Distribution
# if this normal approximation holds for our list, 
# then the population mean and variance of our list can be used in the normal dist formula
# An example of this would be when we noted above that only 1.5% of values 
#on the null distribution were above obsdiff. 
#We can compute the proportion of values below a value x with pnorm(x,mu,sigma) 
# without knowing all the values. The normal approximation works very well here:
1 - pnorm(obsdiff,mean(null),sd(null)) 
## [1] 0.01391929

## Summary
# Statistical Inference is the mathematical theory 
#that permits you to approximate this with only the data from your sample, 
#i.e. the original 24 mice. We will focus on this in the following sections.







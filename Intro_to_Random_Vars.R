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





